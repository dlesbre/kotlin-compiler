(* fichier définnisant les arbres de syntaxe abstraite de petit Kotlin
Dependances
- erreurs.mli *)

(* types representant des valeurs *)

open Erreurs (* type position *)

type operator =
  | Op_Plus | Op_Moins | Op_Fois | Op_Div | Op_Modulo
  | Op_Et   | Op_Ou    | Op_Non
  | Op_3eg  | Op_N2eg  | Op_2eg  | Op_Neg
  | Op_Lt   | Op_Le    | Op_Gt   | Op_Ge

type constant =
  | C_Int of int
  | C_Str of string
  | C_Bool of bool
  | C_Null
  | C_Unit (* uniquement utile pour le return *)

type ident = string

module Ident = struct
    type t = ident
    let compare = compare
end

module Smap = Map.Make(Ident)
module SSet = Set.Make(Ident)

(* arbre de syntaxe abstraite *)

type typ =
  (*typ ? le booleen represente si on est sur que non-nul.*)
  | Tp_Nullable of typ * bool
  (* Tp_Arrow(type args, type retour) *)
  | Tp_Arrow of typ list * typ
  | Tp_Class of ident * typ list

let tp_Null    = Tp_Class("Null", [])
let tp_Int     = Tp_Class("Int", [])
let tp_Boolean = Tp_Class("Boolean", [])
let tp_String  = Tp_Class("String", [])
let tp_Unit    = Tp_Class("Unit", [])
let tp_Array   = Tp_Class("Array", [Tp_Class("t", [])])

type parametre = ident * typ
type parametre_c =
  |P_Var of parametre
  |P_Val of parametre

type expr_et_loc = {expr : expr; loc : position}
and expr =
  | E_Const of constant
  | E_Unop of operator * expr_et_loc (* - expr | !expr *)
  | E_Binop of operator * expr_et_loc * expr_et_loc
  | E_Call of ident * expr_et_loc list
  | E_If of expr_et_loc * blocexpr * blocexpr (*if (a,b,c) <=> if a then b else c*)
  | E_While of expr_et_loc * blocexpr
  | E_Return of expr_et_loc
  | E_AccessRead of acces
  | E_AccessWrite of acces * expr_et_loc
  | E_Lambda of parametre list * typ * bloc
and blocexpr =
  | E_Bloc of bloc
  | E_Expr of expr_et_loc
and bloc =
    var_or_expr list * position
and var_or_expr =
  | VE_Var of variable
  | VE_Exp of expr_et_loc
and acces =
  | E_Var of ident
  | E_VarDot of expr_et_loc * ident
  | E_VarIntDot of expr_et_loc * ident
and variable =
  | V_Var of ident * typ option * expr_et_loc * position
  | V_Val of ident * typ option * expr_et_loc * position

type fonction = {f_name  : ident;
                 f_param : parametre list;
                 f_typ   : typ;
                 f_body  : bloc;
                 f_templ : ident list;
                 f_pos   : position}

type classe = {c_name  : ident;
               c_param : parametre_c list;
               c_vars  : variable list;
               c_templ : ident list;
               c_pos   : position}

type declaration =
  | D_Var of variable
  | D_Class of classe
  | D_Fonction of fonction

type file = declaration list

let repr_operator = function
  |Op_Plus   -> "+"
  |Op_Moins  -> "-"
  |Op_Fois   -> "*"
  |Op_Div    -> "/"
  |Op_Modulo -> "%"
  |Op_Et     -> "&&"
  |Op_Ou     -> "||"
  |Op_Non    -> "!"
  |Op_3eg    -> "==="
  |Op_N2eg   -> "!=="
  |Op_2eg    -> "=="
  |Op_Neg    -> "!="
  |Op_Lt     -> "<"
  |Op_Le     -> "<="
  |Op_Gt     -> ">"
  |Op_Ge     -> ">="