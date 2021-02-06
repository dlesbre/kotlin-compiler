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

(*
val type_constant    : constant -> typ

(* bloc mutuellement recursif *)
val type_expr_et_loc : environnement -> expr_et_loc -> typ_expr_et_loc
val type_blocexpr    : environnement -> blocexpr -> typ_blocexpr
val type_bloc        : environnement -> bloc -> typ_bloc
val type_variable    : environnement -> variable -> typ_variable
                      (* renvoie l'acces, son type, ismutable, un message d'eereur clair *)
val type_acces       : environnement -> Erreurs.position -> acces ->
                                        typ_acces * typ * bool * string
val type_function    : environnement -> fonction -> bool -> typ_fonction (* indique si la fonction est anonyme ou non*)
val type_class       : environnement -> classe -> typ_classe
*)

val type_file        : file -> typ_file