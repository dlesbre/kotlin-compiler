(* module codant les environnement de typage
Dependences:
- environnement.mli
- syntax.ml (erreurs.mli)
- type_syntax.ml (syntax.ml, erreurs.mli)

type environnement contient:
 - liste des classes definies (acces + ajout) avec type abstr et attributs
 - liste des variables globales + fct +constructeur (vu comme fct)
     non-modifiable
 - liste des variables locales (ajout possible si distinct) meme contenu
 - Ensembles des variables non-nulles (separees en locales globales)
 - this est considere comme une variable lambda (pas de conflit car mot-cle)
 - type/existence return

info sur une variable:
- nom
- mutable (var) on non (val)
- type reel (typ) ou abstrait (func), (abs_typ dans Type_syntax)
*)

open Syntax
open Type_syntax

let start_env_name = "$file"
let print_int_name = ".print_int"
let print_str_name = ".print_str"
let abs_typ_prefix = '\''

let empty_expr = TE_Const(C_Null)
let empty_expr_et_loc = {t_expr = empty_expr;
                         t_typ = tp_Null}

let sprintf = Format.sprintf
let failure =
  Erreurs.compiler_failure "environnement.ml" "Erreur de gestion de variable"

let rec repr_list fmt (f,l) = match l with
  |[]    -> Format.fprintf fmt ""
  |t::[] -> Format.fprintf fmt "%a" f t
  |t::q  -> Format.fprintf fmt "%a,@ %a" f t repr_list (f, q)
let rec repr_typ fmt = function
  | Tp_Class(s,a)    -> if a != [] then
                          Format.fprintf fmt "%s<@[%a@]>" s repr_list (repr_typ, a)
                        else
                          Format.fprintf fmt "%s" s
  | Tp_Arrow(a,t)     -> Format.fprintf fmt "(@[%a@]) -> %a" repr_list (repr_typ, a) repr_typ t
  | Tp_Nullable(t, b) -> Format.fprintf fmt "(@[%a@])?%s" repr_typ t (if b then "*" else "")
let repr_type t =
  repr_typ Format.str_formatter t;
  Format.flush_str_formatter ()


let rec replace_templates equal = function
  |Tp_Class(t, l) -> if l = [] && Smap.mem t equal then
                       Smap.find t equal
                     else
                       Tp_Class(t, List.map (replace_templates equal) l)
  |Tp_Arrow(arg, ret) -> Tp_Arrow(List.map (replace_templates equal) arg,
                                  replace_templates equal ret)
  |Tp_Nullable(a, b) -> Tp_Nullable(replace_templates equal a, b)

(* ======== Types ======== *)

type var =
  {is_mutable : bool;
   var_type   : abs_typ}

type environnement =
  {mutable types     : (ident list * var Smap.t) Smap.t;
                     (* name -> templates, attributes *)
   mutable abs_types : SSet.t; (* types abstraits *)
   mutable locals    : var Smap.t;
           globals   : var Smap.t; (* this stored as variable *)
   mutable lc_nulls  : SSet.t;
   mutable gb_nulls  : SSet.t;
           return    : typ option;
   mutable hasreturn : bool;
           scope     : ident; (*seulement no de fct/clas *)
   mutable children  : environnement list}

(* utile dans l'evaluation parresseurse
   -> champ1 non_null, champ2 null *)
let empty_nullstack = [SSet.empty, SSet.empty]
let nullstack = ref empty_nullstack


(* ======== Fonctions de lectures ======== *)

let variable_exists env name =
  Smap.mem name env.locals ||
  Smap.mem name env.globals

let rec type_exists env = function
  |Tp_Nullable(x, b) -> 
      let exists, x' = type_exists env x in
      exists, Tp_Nullable(x', b)
  |Tp_Arrow(args, ret) -> 
      let exists, ret' = type_exists env ret in
      let boolean, args', _ = type_exists_list env 0 true [] args in
      exists && boolean, Tp_Arrow(List.rev args', ret')
  |Tp_Class(name, temp) -> 
      if temp = [] && SSet.mem name env.abs_types then 
          (*replacement de type abstraits t par 't*)
          true, Tp_Class(sprintf "%c%s" abs_typ_prefix name, [])
      else
          let exists, temp', len = type_exists_list env 0 true [] temp in
          if Smap.mem name env.types then
               exists && len = List.length (fst (Smap.find name env.types)),
               Tp_Class(name, List.rev temp')
          else
              false, Tp_Class(name, List.rev temp')
and type_exists_list env n b l' = function
  |[] -> b, l', n
  |t::q -> let exists, t' = type_exists env t in
           type_exists_list env (n+1) (b && exists) (t'::l') q

let class_exists env name = Smap.mem name env.types

let attribute_exists env attr =
  let found = ref false in
  let is_mutable = ref false in
  Smap.iter (fun _ (_, attributes) ->
      if Smap.mem attr attributes then
        (found := true;
        if (Smap.find attr attributes).is_mutable then
            is_mutable := true)) env.types;
  !found, !is_mutable

let get env var = (* private *)
  if Smap.mem var env.locals then
    Smap.find var env.locals, true (* boolean is_local *)
  else
    if Smap.mem var env.globals then
      Smap.find var env.globals, false
    else
      failure "get"
      (sprintf "Variable %s non presente dans l'environnement" var) None

let is_mutable env var =
  (fst (get env var)).is_mutable

let is_attr_mutable env cls attr =
  let _, attributes = Smap.find cls env.types in
  (Smap.find attr attributes).is_mutable

let rec is_in var = function (* private *)
 |[] -> false, false
 |(non_null, null)::q -> if SSet.mem var non_null then
                           true, false
                         else if SSet.mem var null then
                           false, true
                         else is_in var q

let is_nonnull env var is_local = (* private *)
  let non_null, null = is_in var !nullstack in
  non_null || ((not null) &&
  if is_local then
    SSet.mem var env.lc_nulls
  else
    SSet.mem var env.gb_nulls)

let get_type env var =
  let v, is_local = get env var in
  match v.var_type with
  |TrueType(t) as a -> begin match t with
      |Tp_Nullable(a, b) ->
        TrueType(Tp_Nullable(a, is_nonnull env var is_local))
      |_                 -> a end
  |a               -> a

let get_return env = env.return
let get_hasreturn env = env.hasreturn
let get_scope env = env.scope

let get_truetype = function (* private *)
  | TrueType(a) -> a
  | AbstFunc(_,_,_) -> failure "get_truetype" "Un attribut n'est pas de type true type" None

let has_attribute env classe attr =
  let _, b = Smap.find classe env.types in
  Smap.mem attr b
let get_attr_type env classe templ attr pos =
    try let prototypes, attributes = Smap.find classe env.types in
      try replace_templates (List.fold_left2 (fun map x y -> Smap.add x y map) Smap.empty
                                        prototypes templ)
                      (get_truetype (Smap.find attr attributes).var_type)
      with Invalid_argument s -> failure "get_attr_type" (sprintf "Classe \"%s\" has %d prototypes, received %d templates" classe
    (List.length prototypes) (List.length templ)) (Some pos)
    with Not_found -> failure "get_attr_type" (sprintf "Classe \"%s\" not found" classe) (Some pos)


(* ======== Modification des environnements ======== *)

let set_hasreturn env b = env.hasreturn <- b

let add_variable env var pos =
  let name, typ, exp, is_mutable = match var with
    TV_Var(n, t, e) -> n, t, e, true
   |TV_Val(n, t, e) -> n, t, e, false in
  if Smap.mem name env.locals then
      Erreurs.raise_error "Conflit de noms"
      (sprintf "La variable \"%s\" est déjà définie dans ce bloc" name) pos;
  env.locals <- Smap.add name {is_mutable = is_mutable;
                               var_type = TrueType(typ)}
                         env.locals;
  match typ with
   |Tp_Nullable(_, _) ->
     if (match exp.t_typ with
      |Tp_Nullable(_, non_null) -> non_null
      |x when x = tp_Null       -> false
      |x                        -> true
     ) then env.lc_nulls <- SSet.add name env.lc_nulls
   |_                 -> ()

let rec type_param_list l = function (* private *)
  |[]        -> l
  |(_, t)::q -> type_param_list (t::l) q

let fun_type f =
  if f.f_templ = [] then
    TrueType(Tp_Arrow(List.rev (type_param_list [] f.f_param), f.f_typ))
  else
    AbstFunc(List.map (fun (_, x) -> x) f.f_param, f.f_typ, f.f_templ)

let add_function env f =
  if Smap.mem f.f_name env.locals then
      Erreurs.raise_error "Conflit de noms"
      (sprintf "La nom \"%s\" ne peut pas etre a la fois parametre@.  et nom de fonction ou classe"
                f.f_name) f.f_pos;
  env.locals <- Smap.add f.f_name
                        {is_mutable = false;
                         var_type = fun_type f} env.locals

let add_type env name templ =
  if Smap.mem name env.types then
    failure "add_type"
    (sprintf "Le type \"%s\" est deja defini." name) None;
  env.types <- Smap.add name (templ, Smap.empty) env.types

let add_abs_type env name = 
  env.abs_types <- SSet.add name env.abs_types;
  let r = sprintf "%c%s" abs_typ_prefix name in
  add_type env r [];
  r

let add_attribute env classe var pos =
  let name, typ, is_mutable = match var with
    TV_Var(n, t, _) -> n, t, true
   |TV_Val(n, t, _) -> n, t, false in
  if Smap.mem classe env.types then
  begin
    let templ, attributes = Smap.find classe env.types in
    if Smap.mem name attributes then
      Erreurs.raise_error "Conflit de noms"
      (sprintf "L'attribut \"%s\" est déjà définie dans la classe %s" name classe) pos;
    env.types <- Smap.add classe (templ, Smap.add name {is_mutable = is_mutable;
                                                        var_type = TrueType(typ)}
                                         attributes) env.types
  end else
    failure "add_attributes" "Nom de classe non present dans types" (Some pos)

let set_notnull env var is_nonnull =
  let f = if is_nonnull then SSet.add else SSet.remove in
  let _, islocal = get env var in
  (*Format.printf "Ajout de %s(%b) %b à %s@." var is_nonnull islocal env.scope;*)
  if islocal then
      env.lc_nulls <- f var env.lc_nulls
  else
    env.gb_nulls <- f var env.gb_nulls


(* ======== Gestion de nullstack ======== *)

let split = function (* private *)
  |t::q -> t, q
  |[] -> failure "split" "pile vide" None

let nullstack_not () = match !nullstack with
  |(a,b)::q -> nullstack := (b,a)::q
  |[] -> ()
let nullstack_add id b = match !nullstack with
  |(non_null,null)::q ->
    if b then
      nullstack := (SSet.add id non_null, null)::q
    else
      nullstack := (non_null, SSet.add id null)::q
  |[] -> failure "b_nonnull_add" "Pile vide" None

let nullstack_addlevel () =
  nullstack := (SSet.empty, SSet.empty)::(!nullstack)
let nullstack_poplevel () =
  let _, q = split !nullstack in nullstack := q
let nullstack_copylevel () =
  let t, q = split !nullstack in nullstack := t::t::q
let nullstack_collapse_union () =
  match !nullstack with
   |(a,b)::(c,d)::q -> nullstack := (SSet.union a c, SSet.union b d)::q
   |_ -> failure "nullstack_collapse_union" "La pile n'as pas 2 niveaux" None
let nullstack_collapse_inter () =
  match !nullstack with
   |(a,b)::(c,d)::q -> nullstack := (SSet.inter a c, SSet.inter b d)::q
   |_ -> failure "nullstack_collapse_union" "La pile n'as pas 2 niveaux" None

let make_permanent_nonnull env =
  let a,_ = split !nullstack in
  SSet.iter (fun x -> set_notnull env x true) (fst a)


(* ======== Gestion des environnements ======== *)

let map_union _ v1 v2 = Some v2
let set_fold env x set =
  if Smap.mem x env.locals then set (* local overides global *)
                           else SSet.add x set

let null_inter env env1 env2 =
  let set_inter = SSet.inter env1.gb_nulls env2.gb_nulls in (* variables visibles*)
  Smap.iter (fun k v -> set_notnull env k (SSet.mem k set_inter))
            env1.globals

let child env return scope is_child =
  let _, not_shadowed = SSet.partition (fun x -> Smap.mem x env.locals)
                        env.gb_nulls in
  let son = {types     = env.types;
             abs_types = env.abs_types;
             locals    = Smap.empty;
             globals   = Smap.union map_union env.globals env.locals;
             lc_nulls  = SSet.empty;
             gb_nulls  = SSet.union env.lc_nulls not_shadowed;
             return    = return;
             hasreturn = false;
             scope     = scope;
             children  = []} in
  if is_child then env.children <- son::env.children; son

let child_copy env = child env env.return env.scope true

let max a b = if a >= b then a else b

let rec nb_vars env =
  (List.fold_left (fun nb child -> max nb (nb_vars child)) 
                 (*Format.printf "--> Vars in %s\n" env.scope;
                 Smap.iter (fun k v -> Format.printf "   --> %s\n" k) env.locals;*)
                 0 env.children) + (Smap.cardinal env.locals)

let start_env =
  {types     = Smap.empty;
   abs_types = SSet.empty;
   locals    = Smap.empty;
   globals   = Smap.empty; (* this stored as variable *)
   lc_nulls  = SSet.empty;
   gb_nulls  = SSet.empty;
   return    = None;
   hasreturn = false;
   scope     = start_env_name;
   children  = []}

let () =
  add_type start_env "Int" [];
  add_type start_env "String" [];
  add_type start_env "Boolean" [];
  add_type start_env "Unit" [];
  add_type start_env "Null" [];
  add_type start_env "Array" ["t"];
  add_variable start_env (TV_Val(print_int_name,
                                 Tp_Arrow([tp_Int], tp_Unit),
                                 empty_expr_et_loc))
               Erreurs.empty_pos;
  add_variable start_env (TV_Val(print_str_name,
                                 Tp_Arrow([Tp_Nullable(tp_String, false)], tp_Unit),
                                 empty_expr_et_loc))
               Erreurs.empty_pos