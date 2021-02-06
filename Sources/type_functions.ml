(* module contenant diverse fonctions utiles au typage
Dependance:
- syntax.ml
- type_syntax.ml (syntax.ml)
- environnement.mli*)

open Syntax
open Type_syntax
open Environnement

let type_error = Erreurs.raise_error "Erreur de typage"
let failure = Erreurs.compiler_failure "type_functions.ml"

let sprintf = Format.sprintf

let add_nullable isnonnull = function
  |Tp_Nullable(a,b) -> Tp_Nullable(a, isnonnull || b)
  |t                -> Tp_Nullable(t, isnonnull)

let typ_nonnull = function (* private *)
  |Tp_Nullable(_,x) -> x
  |x when x = tp_Null -> false
  |_ -> true

let is_acces e = (* private *)
  match e.t_expr with
   |TE_AccessRead(TE_Var(name)) -> name, true
   |_ -> "", false

let null_test b obj1' obj2' =
  let name1, isacces1 = is_acces obj1' in
  let name2, isacces2 = is_acces obj2' in
  if isacces1 then match obj2'.t_typ with
    |x when x = tp_Null   -> nullstack_add name1 (not b)
    |x when typ_nonnull x -> nullstack_add name1 b
    |_ -> ();
  if isacces2 then match obj1'.t_typ with
    |x when x = tp_Null   -> nullstack_add name1 (not b)
    |x when typ_nonnull x -> nullstack_add name1 b
    |_ -> ()

(* ======== Egalites de types ======== *)

let rec list_and f a b = (* private *)
  match a, b with
   |[], [] -> true
   |t::q, t'::q' -> (f t t') && list_and f q q'
   |_, _ -> false

(* compatible est non commutatif, compatible a b vrai
   si l'on peut ecrire b dans un variable de type a
   (ex t? = t vrai mais t = t? faux)*)
let rec compatible typ1 typ2 =
  match typ1, typ2 with
   |Tp_Class(name, templ), Tp_Class(name', templ') ->
                           name = name' && (list_and compatible templ templ')
   |Tp_Arrow(arg, ret),    Tp_Arrow(arg', ret') ->
                           list_and compatible (ret::arg) (ret'::arg')
   |Tp_Nullable(_),        x when x = tp_Null   -> true
   |Tp_Nullable(t, _),     Tp_Nullable(t', _)   -> compatible t t'
   |Tp_Nullable(t, _),     t'                   -> compatible t t'
   |t',                    Tp_Nullable(t, true) -> compatible t' t
   |_,                     _                    -> false

(* symetrique,
renvoie un boolean et le type le plus general encapsulant les deux *)
let rec bicompatible typ1 typ2 =
  match typ1, typ2 with
   |Tp_Class(name, templ), Tp_Class(name', templ') ->
                           if name = name' then
                             let boolean, list = bicompatible_list templ templ' in
                             boolean, Tp_Class(name, list)
                           else
                             false, tp_Null
   |Tp_Arrow(arg, ret),    Tp_Arrow(arg', ret') ->
                           let b, t = bicompatible ret ret' in
                           if b then
                             let boolean, list = bicompatible_list arg arg' in
                             boolean, Tp_Arrow(list, t)
                           else
                             false, tp_Null
   |a, b when a = tp_Null && b = tp_Null        -> true, tp_Null
   |Tp_Class(_,_) as a, x when x = tp_Null      -> true, add_nullable false a
   |x, (Tp_Class(_,_) as a) when x = tp_Null    -> true, add_nullable false a
   |Tp_Nullable(t, n),     Tp_Nullable(t', n')  -> let a, b = bicompatible t t'
                                                   in a, add_nullable (n && n') b
   |Tp_Nullable(t, _),     t'                   -> bicompatible t t'
   |t',                    Tp_Nullable(t, _)    -> bicompatible t' t
   |_,                     _                    -> false, tp_Null

and bicompatible_list t1 t2 = (* private *)
  match t1, t2 with
   |[], [] -> true, []
   |t::q, t'::q' -> let boolean, typ = bicompatible t t' in
                    if boolean then
                      let bool', typ' = bicompatible_list q q' in
                      bool', typ::typ'
                    else
                      false, []
   |_, _         -> false, []

let rec list_any f l1 l2 = match l1, l2 with (* private *)
  |[], [] -> false
  |t::_, t'::_ when f t t' -> true
  |t::q, t'::q' -> list_any f q q'
  |_,_ -> true

let rec not_equal typ1 typ2 =
 match typ1, typ2 with
  |Tp_Class(n, l), Tp_Class(n',l') -> n <> n' || list_any not_equal l l'
  |Tp_Arrow(l, r), Tp_Arrow(l', r') -> r' <> r || list_any not_equal l l'
  |Tp_Nullable(a,_), Tp_Nullable(a',_) -> not_equal a a'
  |_, _ -> true



let get_blocexpr_type = function
   TE_Expr(e) -> e.t_typ
  |TE_Bloc(_, t) -> t

let get_variable_type env = function
  | TV_Var(n, t, _)
  | TV_Val(n, t, _) -> Environnement.get_type env n

let get_variable_pos = function
    V_Var(_,_,_,p)
  | V_Val(_,_,_,p) -> p

(* ======== Unification lors d'un appel ======== *)

let type_call env name vars pos = function
  |Tp_Arrow(t_param, t_ret)
  |Tp_Nullable(Tp_Arrow(t_param, t_ret), true) ->
    let i = ref 0 in
    (try List.iter2
        (fun e t -> incr i;
                    if compatible t e.t_typ then
                      ()
                    else
                      type_error
                      (sprintf "Le parametre %d de la fonction %s est de type %s,@.  et non %s"
                       !i name (repr_type t) (repr_type e.t_typ)) pos)
        vars t_param;
      TE_Call(name, vars), t_ret
    with |Invalid_argument _ ->
      type_error (sprintf "La fonction %s attend %d arguments.@.  Elle est appliquee ici a %d arguments"
                  name (List.length t_param) (List.length vars)) pos)
  |t -> type_error (sprintf
        "La variable %s est de type %s@.  Ce n'est pas une fonction, elle ne peut etre appelee."
         name (repr_type t)) pos

let rec unify equal pos true_typ abst_typ fun_name = (* private *)
(*Format.printf "  %s %s\n" (repr_type true_typ) (repr_type abst_typ);*)
  match true_typ, abst_typ with (* NON Commutatif! *)
   |Tp_Class(name, param) as t, (Tp_Class(name', param') as t')
     when name = name' && (param' <> [] || not (Smap.mem name' equal)) ->
        (try let equal', param'' = list_unify equal pos param param' fun_name in
            equal', Tp_Class(name, param'')
        with Invalid_argument _ ->
             Erreurs.raise_error "Erreur de typage (unification)"
             (sprintf "Impossible d'unifier les types %s@.  et %s dans l'appel a %s@.  Ces types sont incompatibles car leurs nombres de parametres de classe different"
                                        (repr_type t) (repr_type t') fun_name) pos)
   |Tp_Arrow(arg, ret) as t, (Tp_Arrow(arg', ret') as t') ->
        (try let equal', arg'' = list_unify equal pos arg arg' fun_name in
             let equal'', ret'' = unify equal' pos ret ret' fun_name in
             equal'', Tp_Arrow(arg'', ret'')
        with Invalid_argument _ ->
             Erreurs.raise_error "Erreur de typage (unification)"
             (sprintf "Impossible d'unifier les types %s@.  et %s dans l'appel a %s@.  Ces types sont incompatibles car leurs nombres d'arguments different"
                                        (repr_type t) (repr_type t') fun_name) pos)
   |Tp_Nullable(t, b), Tp_Nullable(t', _) ->
       let equal', t'' = unify equal pos t t' fun_name in
       equal', Tp_Nullable(t'', b)
   |Tp_Nullable(t, true), t' -> unify equal pos t t' fun_name
   |t, Tp_Nullable(t', b) when t = tp_Null -> equal, t
   |t, Tp_Nullable(t', b) ->
       let equal', t'' = unify equal pos t t' fun_name in
       equal', Tp_Nullable(t'', b)
   |t, Tp_Class(name, []) when Smap.mem name equal ->
       begin match Smap.find name equal with
        |None     -> (*Format.printf "    %s -> %s\n" (repr_type t) name;*)
                     Smap.add name (Some t) equal, t
        |Some(t') -> let boolean, t'' = bicompatible t t' in
                     if boolean then
                        Smap.add name (Some t'') equal, t''
                     else
                        Erreurs.raise_error "Erreur de typage (unification)"
                        (sprintf "Impossible d'unifier le type abstrait %s dans l'appel a %s@.  Il serait a la fois de type %s@.  et de type %s@.  Ces types sont incompatibles"
                                        name fun_name (repr_type t') (repr_type t)) pos
       end
   |t, t' -> Erreurs.raise_error "Erreur de typage (unification)"
                        (sprintf "Impossible d'unifier les types %s@.  et %s@.  Ces types sont incompatibles"
                                        (repr_type t) (repr_type t')) pos
and list_unify equal pos l1 l2 fun_name = (* private *)
  match l1, l2 with
   |[], [] -> equal, []
   |t::q, t'::q' -> let equal', t'' = unify equal pos t t' fun_name in
                    let equal'', q'' = list_unify equal' pos q q' fun_name in
                    equal'', t''::q''
   |_, _ -> failure "Erreur de typage (unification)" "list_unify" "les liste n'ont pas le meme longueur" (Some pos)

let unify_call type_in type_ret abs_type vars_type fun_name pos =
  (*Format.printf "Unifying: %s\n" fun_name;*)
  let abs_type' = List.fold_left (fun x a -> Smap.add a None x) Smap.empty abs_type in
  try let abs_type'', t' = list_unify abs_type' pos vars_type type_in fun_name in
      let ret' = replace_templates
                (Smap.mapi (fun k v -> match v with
                           |Some(t) -> t
                           |None    -> Erreurs.raise_error "Erreur de typage (unification)"
                                         (sprintf "Le type \"%s\" n'a pas ete unifie dans les arguments de %s"
                                          k fun_name) pos) abs_type'')
                 type_ret in
      (*Format.printf "%s : %s (%s)\n" fun_name (repr_type (Tp_Arrow(t', ret')))
      (repr_type type_ret);*)
      Tp_Arrow(t', ret')
  with Invalid_argument _ ->
      failure "Erreur de typage (unification)"
              "unify_call" "Tailles des liste d'arguments differentes non rattrapee"
                        (Some pos)


let add_free name used free =
  if not (SSet.mem name !used) then begin
      used := SSet.add name !used;
      free := name::(!free)
  end

let rec find_free_expr_et_loc used free exp =
  match exp.expr with
  | E_Const(_)         -> ()
  | E_Unop(_, e)       -> find_free_expr_et_loc used free e 
  | E_Binop(_, e1, e2) -> find_free_expr_et_loc used free e1;
                          find_free_expr_et_loc used free e2
  | E_Call(n, l)       -> add_free n used free;
                          List.iter (find_free_expr_et_loc used free) l
  | E_If(e, b1, b2)    -> find_free_expr_et_loc used free e;
                          find_free_blocexpr used free b1;
                          find_free_blocexpr used free b2
  | E_While(e,b)       -> find_free_expr_et_loc used free e;
                          find_free_blocexpr used free b;
  | E_Return(e)        -> find_free_expr_et_loc used free e
  | E_AccessRead(a)    -> find_free_acces used free a
  | E_AccessWrite(a,e) -> find_free_expr_et_loc used free e;
                          find_free_acces used free a
  | E_Lambda(param, _, b)  -> List.iter (fun (x,_) -> 
                                used := SSet.add x !used) param;
                              find_free_bloc used free b
and find_free_blocexpr used free = function
  | E_Bloc(b)          -> find_free_bloc used free b
  | E_Expr(e)          -> find_free_expr_et_loc used free e
and find_free_bloc used free (ve_list, _) = 
    List.iter (find_free_var_or_expr used free) ve_list 
and find_free_var_or_expr used free = function
  | VE_Var(v)          -> find_free_variable used free v
  | VE_Exp(e)          -> find_free_expr_et_loc used free e
and find_free_acces used free = function
  | E_Var(id)          -> add_free id used free
  | E_VarDot(e,_)
  | E_VarIntDot(e,_)   -> find_free_expr_et_loc used free e
and find_free_variable used free = function
  | V_Var(n,_,e,_) 
  | V_Val(n,_,e,_)     -> find_free_expr_et_loc used free e;
                          used := SSet.add n !used

let free_vars p_list bloc =
  let used = ref (List.fold_left (fun set (name,_) -> SSet.add name set) SSet.empty p_list) in
  let free = ref [] in
  find_free_bloc used free bloc;
  List.rev !free

(* ======== Typage d'un acces (cf typeur) ======== *)

let get_acces name pos = function
  | TrueType(n) -> n
  | AbstFunc(_,_,_) -> type_error (sprintf "La fonction \"%s\" est un prototype.@.  Elle ne peut qu'etre appelee, ce n'est pas une variable"
                                   name) pos

(* raccourci pour type_acces *)
let acces_shortcut env name attr templ f pos =
  if has_attribute env name attr then
      f (get_attr_type env name templ attr pos)
  else
      Erreurs.raise_error "Attribut inexistant"
      (sprintf "La classe \"%s\" n'a pas d'attribut \"%s\"" name attr) pos

let are_distinct f list msg pos =
  let rec runner s = function
    |[] -> ()
    |t::q -> let id = f t in
             if SSet.mem id s then
                Erreurs.raise_error "Conflit de nom" (sprintf "%s \"%s\"" msg id) pos
             else
                runner (SSet.add id s) q
  in runner SSet.empty list