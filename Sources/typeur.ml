(* Module typeur.ml, encode le typeur
Dependances:
- erreurs.mli (erreurs.ml)
- syntax.ml
- type_syntax.ml (syntax.ml, erreurs.mli)
- environnement.mli (type_syntax.mli, syntax.mli, erreurs.mli)
- type_functions.mli (environnement.mli, type_syntax.mli, syntax.mli, erreurs.mli*)

open Syntax
open Type_syntax
open Type_functions
open Environnement

let type_failure = Erreurs.compiler_failure "typeur.ml" "Echec du typeur"
let sprintf = Format.sprintf

let lambda_count = ref 0

let type_constant = function
  | C_Int(_)  -> tp_Int
  | C_Bool(_) -> tp_Boolean
  | C_Str(_)  -> tp_String
  | C_Null    -> tp_Null
  | C_Unit    -> tp_Unit

let add_return (bloc, typ) =
    List.append bloc [TVE_Exp({t_expr = TE_Return({t_expr = TE_Const(C_Unit);
                                          t_typ = tp_Unit});
                       t_typ = tp_Unit})], typ

let rec type_expr_et_loc env exp_et_loc =
 let texpr, typ = match exp_et_loc.expr with
  |E_Const(c)  -> TE_Const(c), type_constant c
  |E_Unop(op, obj) ->
    nullstack_addlevel ();
    let obj' = type_expr_et_loc env obj in
    let expected = begin match op with
      |Op_Moins -> tp_Int
      |Op_Non   -> nullstack_not (); tp_Boolean
      |_ -> type_failure (sprintf "operateur '%s' binaire et non unaire..." (repr_operator op))
              "syntax.ml fct type_expr_et_loc env" (Some exp_et_loc.loc)
    end in nullstack_collapse_union ();
           if compatible obj'.t_typ expected then
             TE_Unop(op, obj'), expected
           else type_error
             (sprintf "L'operateur unaire '%s' ne peut etre applique qu'a un type %s@.  Il est ici applique avec le type %s."
              (repr_operator op) (repr_type expected) (repr_type obj'.t_typ))
              exp_et_loc.loc
  |E_Binop(op, obj1, obj2) ->
    nullstack_addlevel ();
    let obj1' = type_expr_et_loc env obj1 in
    let previous_return = get_hasreturn env in
    if op = Op_Ou then (* utile pour le typage de obj2 *)
       (nullstack_copylevel (); nullstack_not ());
    let obj2' = type_expr_et_loc env obj2 in
    let rep = TE_Binop(op, obj1', obj2') in
    let test, typ, msg = begin match op with
      |Op_3eg |Op_N2eg ->
        (* test d'inegalite stricts *)
        (not_equal obj1'.t_typ tp_Int     &&
         not_equal obj1'.t_typ tp_Unit    &&
         not_equal obj1'.t_typ tp_Boolean &&
         not_equal obj2'.t_typ tp_Int     &&
         not_equal obj2'.t_typ tp_Unit    &&
         not_equal obj2'.t_typ tp_Boolean),
        tp_Boolean,
        "deux arguments\n  de types differents Int, Unit ou Boolean."
      |Op_2eg |Op_Neg |Op_Gt |Op_Ge |Op_Lt |Op_Le ->
        (compatible tp_Int obj1'.t_typ && compatible tp_Int obj2'.t_typ ),
        tp_Boolean,
        "deux arguments de type Int"
      |Op_Plus |Op_Moins |Op_Fois |Op_Div |Op_Modulo ->
        (compatible tp_Int obj1'.t_typ && compatible tp_Int obj2'.t_typ),
        tp_Int,
        "deux arguments de type Int"
      |Op_Et |Op_Ou ->
        (compatible tp_Boolean obj1'.t_typ && compatible tp_Boolean obj2'.t_typ),
        tp_Boolean,
        "deux arguments de type Boolean"
      |_ -> type_failure "type_expr_et_loc env"
            (sprintf "operateur %s non binaire" (repr_operator op))
            (Some exp_et_loc.loc)
    end in
    if test then
     begin (match op with
      |Op_3eg -> null_test true obj1' obj2'
      |Op_N2eg -> null_test false obj1' obj2'
      |Op_Ou -> nullstack_collapse_inter (); 
                set_hasreturn env previous_return 
                (* evaluation paresseuse, pas de garantir que le return soit evalue... *)
      |Op_Et -> set_hasreturn env previous_return
      |_ -> ());
    nullstack_collapse_union ();
    rep, typ end
    else type_error (sprintf
        "L'operateur binaire %s ne peut etre applique qu'avec %s@.  Et non a %s et %s"
        (repr_operator op) msg (repr_type obj1'.t_typ) (repr_type obj2'.t_typ))
      exp_et_loc.loc
  |E_While(cond, bloc) ->
    nullstack_addlevel ();
    let cond' = type_expr_et_loc env cond in
    if compatible tp_Boolean cond'.t_typ then
      let env' = child_copy env in
      make_permanent_nonnull env';
      let bloc' = type_blocexpr env' bloc in
      nullstack_not (); (* fin du while, la condition est fausse *)
      make_permanent_nonnull env;
      nullstack_poplevel ();
      TE_While(cond', bloc'), tp_Unit
    else
      type_error (sprintf
      "Une condition de bloc while doit etre un Boolean,@  et non %s"
      (repr_type cond'.t_typ)) cond.loc;
  |E_If(cond, expa, expb) ->
    nullstack_addlevel ();
    let cond' = type_expr_et_loc env cond in
    if compatible tp_Boolean cond'.t_typ then
    begin
        let inter = ref true in
        let env' = child_copy env in
        make_permanent_nonnull env';
        let expa' = type_blocexpr env' expa in
        nullstack_not ();
        if get_hasreturn env' then (
          inter := false;
          make_permanent_nonnull env);
        let env'' = child_copy env in
        make_permanent_nonnull env'';
        let expb' = type_blocexpr env'' expb in
        nullstack_not ();
        if get_hasreturn env'' then (
          inter := false;
          make_permanent_nonnull env);
        nullstack_poplevel ();
        let ta = get_blocexpr_type expa' in
        let tb = get_blocexpr_type expb' in
        let b, t' = bicompatible tb ta in
        if b then (
            set_hasreturn env 
            (get_hasreturn env ||(get_hasreturn env' && get_hasreturn env''));
            if !inter then null_inter env env' env'';
            TE_If(cond', expa', expb'), t')
        else
            type_error (sprintf "Les blocs if et else doivent etre de meme type,@.  (le bloc else est de type Unit si omis)@.  et non %s et %s"
            (repr_type ta) (repr_type tb))
             exp_et_loc.loc;
    end
    else
        type_error (sprintf
        "Une condition de bloc if doit etre un Boolean,@  et non %s"
        (repr_type cond'.t_typ)) cond.loc;
  |E_Return(obj) ->
    set_hasreturn env true;
    begin match get_return env with
     |None   -> Erreurs.raise_error "Erreur de semantique"
                "Le Mot-cle return n'a pas de sens dans ce contexte@.  (en-dehors d'une fonction ou dans un parametre...)"
                exp_et_loc.loc
     |Some(t) -> let env' = child_copy env in
                 let obj' = type_expr_et_loc env' obj in
                 if compatible t obj'.t_typ then
                     TE_Return(obj'), tp_Unit
                 else
                     type_error (sprintf
                     "Le type de cette expression ne correspond pas au type renvoye@.  par la fonction %s@.  Attendu : %s@.  Recu : %s"
                     (get_scope env) (repr_type t) (repr_type obj'.t_typ))
                     exp_et_loc.loc;
    end
  |E_Call(nam, vars) ->
    let vars' = List.map (type_expr_et_loc env) vars in
    let name = if nam = "print" then
                 begin match vars' with
                 |[t] -> if compatible tp_Int t.t_typ then
                           print_int_name
                         else (if compatible (Tp_Nullable(tp_String, false)) t.t_typ then
                           print_str_name
                         else nam)
                 |_ -> nam
                 end
               else nam in
    if variable_exists env name then
    begin
      type_call env name vars' exp_et_loc.loc
      (match get_type env name with
      |TrueType(a) ->  a
      |AbstFunc(type_in, type_ret, abs_type) ->
        let expected_length = List.length type_in in
        let true_length = List.length vars' in
        if expected_length = true_length then
          (unify_call type_in type_ret abs_type
                     (List.map (fun x -> x.t_typ) vars')
                     nam exp_et_loc.loc)
        else
          type_error (sprintf "La fonction \"%s\" attend %d arguments@.  Elle est appliquee ici a %d arguments"
                      name expected_length true_length) exp_et_loc.loc)
    end else Erreurs.raise_error "Nom invalide"
             (sprintf "La fonction \"%s\" n'est pas definie." name)
             exp_et_loc.loc
  |E_Lambda(param, t, code) ->
      let pseudo = sprintf ".lambda_%d" !lambda_count in
      incr lambda_count;
      let g = {f_name  = pseudo;
               f_param = param;
               f_typ   = t;
               f_body  = code;
               f_pos   = exp_et_loc.loc;
               f_templ = []} in
      let f = type_function env g false in
      let free = free_vars param code in
      TE_Lambda({l_name  = f.tf_name;
                 l_param = f.tf_param;
                 l_typ   = f.tf_typ;
                 l_body  = f.tf_body;
                 l_size  = f.tf_size;
                 l_free = free}), (* dans le cas d'un lambda *)
      get_acces pseudo exp_et_loc.loc (fun_type g)
      (* get_acces sert a faire disparaite le wrapper abs_typ
         on est sur que la fonction est un vrai type car elle n'est pas polymorpher
         (liste de template vide)*)
  |E_AccessRead(acc) ->
      let acc', typ, _, _ = type_acces env exp_et_loc.loc acc in
      TE_AccessRead(acc'), typ
  |E_AccessWrite(acc, exp) ->
      let acc', typ, is_mutable, msg = type_acces env exp_et_loc.loc acc in
      if is_mutable then
        let exp' = type_expr_et_loc env exp in
        if compatible typ exp'.t_typ then
          TE_AccessWrite(acc', exp'), tp_Unit
        else
          type_error (sprintf
            "Impossible d'affecter une valeur de type %s@.  a une variable de type %s"
            (repr_type exp'.t_typ) (repr_type typ)) exp_et_loc.loc
      else
        type_error msg exp_et_loc.loc (* non mutable *)
 in {t_expr = texpr; t_typ = typ}

and type_blocexpr env = function
  |E_Expr(e) -> TE_Expr(type_expr_et_loc env e)
  |E_Bloc(b) -> TE_Bloc(type_bloc env b)

and type_bloc env (v_list, pos) =
  let last_type = ref tp_Unit in
  let v_list' = List.map (function
   |VE_Var(v) -> let v' = type_variable env v in
                 last_type := tp_Unit;
                 TVE_Var(v')
   |VE_Exp(e) -> let e' = type_expr_et_loc env e in
                 last_type := e'.t_typ;
                 TVE_Exp(e')) v_list in
  v_list', !last_type

and type_variable env v =
  let name, typ_opt, exp, pos, is_mutable = match v with
    | V_Var(n,t,e,p) -> n,t,e,p,true
    | V_Val(n,t,e,p) -> n,t,e,p,false
  in
    let exp' = type_expr_et_loc env exp in
    let typ = match typ_opt with
      |None   -> exp'.t_typ
      |Some t -> let exists, t' = type_exists env t in
        if exists then
          if compatible t' exp'.t_typ then
            t'
          else
            type_error (sprintf
            "La variable \"%s\" est de type %s.@.  On lui affecte ici un type %s.@.  Ces deux types sont incompatibles."
            name (repr_type t') (repr_type exp'.t_typ)) pos
        else
          type_error (sprintf "Le type %s de var/val \"%s\" est mal forme"
          (repr_type t') name) pos in
      let v' = if is_mutable then
                 TV_Var(name, typ, exp')
               else
                 TV_Val(name, typ, exp')
      in add_variable env v' pos; v'

(* renvoi l'acces, le type, un booleen ismutable
   et une identifiant la variable (en cas de message d'erreur) *)
and type_acces env pos = function
  | E_Var(name) -> if variable_exists env name then
                     TE_Var(name),
                     get_acces name pos (get_type env name),
                     is_mutable env name,
                     sprintf "La valeur \"%s\" n'est pas mutable" name
                   else
                     Erreurs.raise_error "Erreur de nom" (sprintf
                          "La variable \"%s\" est non definie" name) pos
  | E_VarDot(exp, attr) ->
      let exp' = type_expr_et_loc env exp in
      begin match exp'.t_typ with
        |Tp_Class(name, templ)
        |Tp_Nullable(Tp_Class(name, templ), true) ->
            if has_attribute env name attr then
              TE_VarDot(exp', attr),
              acces_shortcut env name attr templ (fun x -> x) pos,
              is_attr_mutable env name attr,
              sprintf "L'attribut \"%s\" de la classe \"%s\" n'est pas mutable" attr name
            else
              Erreurs.raise_error "Erreur de nom" (sprintf
                "La classe \"%s\" n'a pas d'attribut \"%s\"" name attr) pos
        |t -> type_error (sprintf "Cette expression est de type %s@.  Ce n'est pas une classe, elle n'a pas d'attribut \"%s\""
                          (repr_type t) attr) pos
      end
  | E_VarIntDot(exp, attr) ->
      let exp' = type_expr_et_loc env exp in
      begin match exp'.t_typ with
        |x when x = tp_Null ->
            let exists, is_mutable = attribute_exists env attr in
            if exists then
                TE_VarIntDot(exp', attr),
                tp_Null,
                is_mutable,
                (sprintf "Il n'existe pas d'attribut \"%s\"@.  (dans toutes les classes) qui soit mutable" attr)
            else
                Erreurs.raise_error "Erreur de nom"
                (sprintf "Auncune classe n'a d'attribut \"%s\".@.  Cet acces est illegal"
                 attr) pos
        |Tp_Class(name, templ)
        |Tp_Nullable(Tp_Class(name, templ), true) ->
          if has_attribute env name attr then
            TE_VarDot(exp', attr), (* sur de la non-nullite *)
            acces_shortcut env name attr templ (fun x -> add_nullable true x) pos,
            is_attr_mutable env name attr,
            sprintf "L'attribut \"%s\" de la classe \"%s\" n'est pas mutable" attr name
          else
            Erreurs.raise_error "Erreur de nom" (sprintf
            "La classe \"%s\" n'a pas d'attribut \"%s\"" name attr) pos
        |Tp_Nullable(Tp_Class(name, templ), false) ->
          if has_attribute env name attr then
            TE_VarIntDot(exp', attr),
            acces_shortcut env name attr templ (fun x -> add_nullable false x) pos,
            is_attr_mutable env name attr,
            sprintf "L'attribut \"%s\" de la classe \"%s\" n'est pas mutable" attr name
          else
            Erreurs.raise_error "Erreur de nom" (sprintf
            "La classe \"%s\" n'a pas d'attribut \"%s\"" name attr) pos
        |t -> type_error (sprintf "Cette expression est de type %s@.  Ce n'est ni une classe, ni Null, ni une classe?, elle n'a pas d'attribut \"%s\""
                          (repr_type t) attr) pos
      end

and type_function env fonction add =
  are_distinct (fun (id, _) -> id) fonction.f_param
               (sprintf "La fonction \"%s\" a plusieurs parametres de meme nom"
                fonction.f_name) fonction.f_pos;
  are_distinct (fun x -> x) fonction.f_templ
               (sprintf "La fonction \"%s\" a plusieurs types abstraits de meme nom"
                fonction.f_name) fonction.f_pos;
  let env' = child env (Some fonction.f_typ) fonction.f_name false in
  let templ' = List.map (fun t -> add_abs_type env' t) fonction.f_templ in
  let param' = List.map (fun (id, typ) ->
             let exists, typ' = type_exists env' typ in
             if exists then (
              add_variable env'
              (TV_Val(id, typ', empty_expr_et_loc)) fonction.f_pos;
              id, typ')
             else
               type_error (sprintf "Le parametre \"%s\" de la fonction \"%s\"@.  est de type mal forme: %s"
                           id fonction.f_name (repr_type typ')) fonction.f_pos)
  fonction.f_param in
  let exists, t_ret = type_exists env' fonction.f_typ in
  if exists then
  begin
    let f = {f_name  = fonction.f_name;
             f_param = param';
             f_typ   = t_ret;
             f_body  = fonction.f_body;
             f_templ = templ';
             f_pos   = fonction.f_pos} in 
    if add then add_function env' f else (); (* add_function distingue si f polymorphe ou non*)
    let env'' = child env' (Some t_ret) f.f_name true in
    let body' = type_bloc env'' f.f_body in
    if f.f_typ = tp_Unit || get_hasreturn env'' then (
      if add then add_function env f else ();
      {tf_name  = f.f_name;
       tf_param = f.f_param;
       tf_typ   = f.f_typ;
       tf_body  = if get_hasreturn env'' then body' else add_return body';
       tf_templ = f.f_templ;
       tf_size  = nb_vars env''})
    else
      type_error (sprintf
      "La fonction \"%s\" a pour type de retour %s@.  Ce type n'est pas Unit, chaque branche du code doit posseder un return"
      fonction.f_name (repr_type t_ret)) fonction.f_pos
  end
  else
    type_error (sprintf "Le type %s renvoye par la fonction \"%s\" est mal forme."
               (repr_type t_ret) fonction.f_name) fonction.f_pos

let type_class env cls =
  if class_exists env cls.c_name then
     Erreurs.raise_error "Conflit de nom" 
     (sprintf "La classe \"%s\" est deja definie" cls.c_name) cls.c_pos;
  are_distinct (function |P_Var(id, _) |P_Val(id, _) -> id) cls.c_param
               (sprintf "La classe \"%s\" a plusieurs parametres de meme nom"
                cls.c_name) cls.c_pos;
  are_distinct (fun x -> x) cls.c_templ
               (sprintf "La classe \"%s\" a plusieurs types abstraits de meme nom"
                cls.c_name) cls.c_pos;
  are_distinct (function |V_Var(id,_,_,_) |V_Val(id,_,_,_) -> id) cls.c_vars
               (sprintf "La classe \"%s\" a plusieurs variable de meme nom"
                cls.c_name) cls.c_pos;
  let env' = child env None cls.c_name false in
  let templ' = List.map (fun t -> add_abs_type env' t) cls.c_templ in
  add_type env cls.c_name templ';
  add_type env' cls.c_name templ';
  let this_class_typ = Tp_Class(cls.c_name,
                                List.map (fun x -> Tp_Class(x, [])) templ') in
  let add_attr v p = add_attribute env' cls.c_name v p;
              add_attribute env cls.c_name v p in
  let add v p = add_variable env' v p; add_attr v p in
  let param' = List.tl (* remove this *)
      (List.map (function s -> let id, typ, isvar = match s with
             |P_Val(i, t) -> i, t, false
             |P_Var(i, t) -> i, t, true in
             let exists, typ' = type_exists env' typ in
             if exists then
               if isvar then
                 (add (TV_Var(id, typ', empty_expr_et_loc)) cls.c_pos;
                  P_Var(id, typ'))
                else
                 (add (TV_Val(id, typ', empty_expr_et_loc)) cls.c_pos;
                  P_Val(id, typ'))
             else
               type_error (sprintf "Le parametre \"%s\" de la classe \"%s\"@.  est de type mal forme: %s"
                           id cls.c_name (repr_type typ')) cls.c_pos)              
  ((P_Val("this", this_class_typ))::cls.c_param)) in
  let vars = List.map (function var -> let var' = type_variable env' var in
                            add_attr var' (get_variable_pos var); var') cls.c_vars in
  add_function env {f_name  = cls.c_name;
                    f_param = List.map (function |P_Val(i,t) |P_Var(i,t) -> i,t)
                                       param';
                    f_templ = templ';
                    f_typ   = this_class_typ;
                    f_body  = [], cls.c_pos;
                    f_pos   = cls.c_pos}; (* constructeur de classe *)
  {tc_name  = cls.c_name;
   tc_param = param';
   tc_vars  = vars;
   tc_templ = templ'}

let type_declaration env = function
  | D_Var(variable) -> let n = nb_vars env in
                       TD_Var begin match type_variable env variable with
                         TV_Val(name, typ, exp) -> TGV_Val(name, typ, exp, (nb_vars env) - n - 1)
                        |TV_Var(name, typ, exp) -> TGV_Var(name, typ, exp, (nb_vars env) - n - 1)
                       end
  | D_Class(classe) -> TD_Class(type_class env classe)
  | D_Fonction(fonction) -> TD_Fonction(type_function env fonction true)

let main_type = Tp_Arrow([Tp_Class("Array", [tp_String])], tp_Unit)

let rec get_main_pos = function
  | [] -> type_failure "get_main_pos" "No main found in decl list" None
  | (D_Fonction(f))::q when f.f_name = "main" -> f.f_pos
  | _::q -> get_main_pos q

let type_file decl =
  let env = start_env in
  let decl' = List.map (type_declaration env) decl in
  if variable_exists env "main" then
    match get_type env "main" with
      |TrueType(t) when t = main_type -> decl'
      |TrueType(t) -> type_error (sprintf 
                      "La fonction main doit etre de type %s.@.  Elle est ici de type %s" 
                      (repr_type main_type) (repr_type t)) (get_main_pos decl)
      |AbstFunc(_,_,_) -> type_error "La fonction main ne peut pas etre une fonction polymorphique" 
      (get_main_pos decl)
  else
    Erreurs.raise_error "Pas de main:" 
    "Ce programme ne possede pas de points d'entree (fonction main)\n  Du coup il ne fait rien et sa compilation se limite au fichier vide.\n  Je ne pense pas que ce soit ce que vous voulez..."
    (Erreurs.file_start_pos ())