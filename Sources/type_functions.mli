(* module contenant diverse fonctions utiles au typage
Dependance:
- syntax.ml
- type_syntax.ml (syntax.ml)
- environnement.mli*)

open Syntax
open Type_syntax
open Environnement

(* souleve une erreur de typage *)
val type_error : string -> Erreurs.position -> 'a

(* t -> t?, t? -> t? (evite l'emboitement)*)
val add_nullable : bool -> typ -> typ

(* verifie si un test booleen certifie qu'un variable est non-nulle
   bool : vrai -> ===, faux -> !===*)
val null_test : bool -> type_expr_et_typ -> type_expr_et_typ -> unit

(* ======== Egalites de types ======== *)

(* compatible est non commutatif, compatible a b vrai
   si l'on peut ecrire b dans un variable de type a
   (ex t? = t vrai mais t = t? faux)*)
val compatible : typ -> typ -> bool
(* symetrique, renvoie un test de compatibilie
  et le type le plus general encapsulant les deux *)
val bicompatible : typ -> typ -> bool * typ
(* inegalite structurelle *)
val not_equal : typ -> typ -> bool


val get_blocexpr_type : typ_blocexpr -> typ
val get_variable_type : environnement -> typ_variable -> abs_typ
val get_variable_pos  : variable -> Erreurs.position

(* ======== Unification lors d'un appel ======== *)

(* verifie que les arguements sont du bon typ et renvoie l'expression type *)
val type_call : environnement -> ident -> type_expr_et_typ list -> Erreurs.position ->
                typ -> typ_expr * typ
(* unifie un appel a fonction polymorphe
arguments: type args -> type ret -> parametre abstraits -> type recu 
-> nom fonction -> position (messages d'erreurs) *)
val unify_call : typ list -> typ -> ident list -> typ list -> 
                 ident -> Erreurs.position -> typ
    (* renvoie les variables libres (fct de premiere classe*)
val free_vars : parametre list -> bloc -> ident list

(* ======== Typage d'un acces (cf typeur) ======== *)

(* N'accepte qu'un vrai type, souleve un erreur sinon
  le nom de variable et position ne servent qu'en cas d'erreur*)
val get_acces : ident -> Erreurs.position -> abs_typ -> typ
(* verifie que l'attribut existe et renvoie son type *)
val acces_shortcut : environnement -> ident -> ident -> typ list ->
                     (typ -> typ) -> Erreurs.position -> typ

(* verifie si tous les element de la liste sont distincts,
   souleve une erreur avec le message et la position donnee sinon*)
val are_distinct : ('a -> ident) -> 'a list ->
                   string -> Erreurs.position -> unit