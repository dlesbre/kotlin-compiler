(* compile les arbres de typages vers de l'assembleur x86-64 AT&T
Dépendances:
 - syntax.ml (erreurs.mli)
 - type_syntax.ml (syntax.ml, erreurs.mli)
 - allocation.mli (type_syntax.ml, syntax.ml erreurs.mli) *)

open Syntax
open Type_syntax
open Allocation

(*

(* bloc mutuellement recursif *)
val convert_constant         : Format.formatter -> constant -> unit
val convert_type_expr_et_typ : Format.formatter -> type_expr_et_typ -> unit
(* calcule les arguments lors d'un appel*)
val calc_args                : Format.formatter -> type_expr_et_typ list -> unit
(* initialise les premiers parametres d'une classe, utilise nb_vars et %rbx *)
val init_class               : Format.formatter -> type_expr_et_typ list -> unit
val convert_typ_blocexpr     : Format.formatter -> typ_blocexpr -> unit
val convert_typ_bloc         : Format.formatter -> typ_bloc -> unit
val convert_typ_var_or_expr  : Format.formatter -> typ_var_or_expr -> unit
val convert_typ_variable     : Format.formatter -> typ_variable -> unit

(* Le formatteur utilise est str_formatter, chacune de ces fonctions
   rajoute la chaine genere dans une liste qui lui est propre:
   ainsi on produit le code dans l'ordre ou il a ete saisi (pas de conflit sur les variables)
   mais on l'ecrit dans un ordre different en parcourant les liste generes *)
val convert_typ_global_variable : typ_global_variable -> unit
val convert_typ_fonction        : typ_fonction -> unit
val convert_typ_classe          : typ_classe -> unit
val convert_typ_declaration     : typ_declaration -> unit

(* gere les fonction anonyme et encapsuleurs de constructeurs *)
val wrappers_and_lambdas : Format.formatter -> string -> unit

*)

(* convertit en enregistre dans le fichier specifie*)
val convert_typ_file : typ_file -> string -> unit