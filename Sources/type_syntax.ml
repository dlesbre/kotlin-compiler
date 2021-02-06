(* Module type_syntax, fournit une version decoree avec les types
des arbres de sytaxes abstaites de Syntax
Dependances:
- syntax.ml (erreurs.mli)
- erreurs.mli*)

open Syntax
open Erreurs

type abs_typ =
  |TrueType of typ
  (* AbstFunc(type args, type renv, types abstraits) *)
  |AbstFunc of typ list * typ * ident list

type type_expr_et_typ = {t_expr : typ_expr;
                         t_typ  : typ}
and typ_expr =
  | TE_Const of constant
  | TE_Unop of operator * type_expr_et_typ (* - expr | !expr *)
  | TE_Binop of operator * type_expr_et_typ * type_expr_et_typ
  | TE_Call of ident * type_expr_et_typ list
  | TE_If of type_expr_et_typ * typ_blocexpr * typ_blocexpr (*if (a,b,c) <=> if a then b else c*)
  | TE_While of type_expr_et_typ * typ_blocexpr
  | TE_Return of type_expr_et_typ
  | TE_AccessRead of typ_acces
  | TE_AccessWrite of typ_acces * type_expr_et_typ
  | TE_Lambda of typ_lambda
and typ_blocexpr =
  | TE_Bloc of typ_bloc
  | TE_Expr of type_expr_et_typ
and typ_bloc =
    typ_var_or_expr list * typ
and typ_var_or_expr =
  | TVE_Var of typ_variable
  | TVE_Exp of type_expr_et_typ
and typ_acces =
  | TE_Var of ident
  | TE_VarDot of type_expr_et_typ * ident
  | TE_VarIntDot of type_expr_et_typ * ident
and typ_variable =
  | TV_Var of ident * typ * type_expr_et_typ
  | TV_Val of ident * typ * type_expr_et_typ
and typ_lambda =
 {l_name  : ident;
  l_param : parametre list;
  l_typ   : typ;
  l_body  : typ_bloc;
  l_free  : ident list;
  l_size  : int}

type typ_global_variable = 
  | TGV_Var of ident * typ * type_expr_et_typ * int (* nb de variables locales *)
  | TGV_Val of ident * typ * type_expr_et_typ * int
type typ_fonction =
 {tf_name  : ident;
  tf_param : parametre list;
  tf_typ   : typ;
  tf_body  : typ_bloc;
  tf_templ : ident list;
  tf_size  : int}

type typ_classe =
 {tc_name  : ident;
  tc_param : parametre_c list;
  tc_vars  : typ_variable list;
  tc_templ : ident list}

type typ_declaration =
  | TD_Var of typ_global_variable
  | TD_Class of typ_classe
  | TD_Fonction of typ_fonction

type typ_file = typ_declaration list