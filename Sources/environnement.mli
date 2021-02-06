(* module codant les environnement de typage
Dependences:
- variables.mli
- syntax.ml (erreurs.mli)

le test et rattrapage d'erreur se font au niveau du typeur,
les acces interdits (variables non definies...) provoqueront un echec ici*)

open Syntax
open Type_syntax

type environnement

val repr_type     : typ      -> string

(* remplace les types abstrait par leur equivalents *)
val replace_templates : typ Smap.t -> typ -> typ

(* ======== Fonctions de lectures ======== *)
val variable_exists  : environnement -> ident -> bool
                      (* typ replace le type abstrait t par 't pour eviter les conflits
                         seulement significatif si le boolean est vrai.*)
val type_exists      : environnement -> typ   -> bool * typ
                      (*unicite des noms de classes...*)
val class_exists     : environnement -> ident -> bool
                      (* une classe a cette attribut? en mutable ?*)
val attribute_exists : environnement -> ident -> bool * bool
val is_mutable       : environnement -> ident -> bool (*x est de type var *)
val is_attr_mutable  : environnement -> ident -> ident -> bool
val get_type         : environnement -> ident -> abs_typ (* type de x *)
val get_return       : environnement -> typ option    (* type de retour si defini*)
val get_hasreturn    : environnement -> bool (* indique si un return a ete rencontre*)
val get_scope        : environnement -> ident (* nom de l'environnement *)
val has_attribute    : environnement -> ident -> ident -> bool
                                       (* nom_classe -> nom_attribut *)
val get_attr_type    : environnement -> ident -> typ list -> ident -> 
                        Erreurs.position -> typ
                      (* nom_classe, type abstrait classe, attribut, pos pour debug*)


(* ======== Modification des environnements ======== *)
val set_hasreturn : environnement -> bool -> unit
val add_variable  : environnement -> typ_variable -> Erreurs.position -> unit
val add_type      : environnement -> ident -> ident list -> unit
val add_abs_type  : environnement -> ident -> ident (* new name *)
val fun_type      : fonction -> abs_typ
val add_function  : environnement -> fonction -> unit
                                      (* ident est le nom de la classe *)
val add_attribute : environnement -> ident -> typ_variable -> Erreurs.position -> unit

(* ======== Gestion de nullstack ======== *)

(* perds l'info de non-nullite(apres un return ou un appel)*)
(*val flush_nonnull : environnement -> unit
val flush_bnonnull: unit -> unit ( seulement l'eval de booleens *)
val set_notnull   : environnement -> ident -> bool -> unit (*affectation, permanent*)
val nullstack_not : unit -> unit (* inverse les liste nulls/non_nulls *)
val nullstack_add : ident -> bool -> unit (* eval paresseuse *)
val nullstack_addlevel : unit -> unit
val nullstack_poplevel : unit -> unit
val nullstack_copylevel : unit -> unit
(* fusion des deux elements du sommet de la pile *)
val nullstack_collapse_union : unit -> unit
val nullstack_collapse_inter : unit -> unit
 (* transforme les infos sur booleans en info persistantes (blocs ifs)*)
val make_permanent_nonnull : environnement -> unit


(* ======== Gestion des environnements ======== *)

(* renvoie un fils de l'environnement
  (fusion variables globales et locale)
  typ option indique la presence ou non d'un return
  ident est le nom de l'environnement (ex nom de la fonction, du bloc)
  le booleen indique s'il s'agit d'un sous-bloc ou d'une nouvelle fonction 
    (pour compter les variables)*)
val child      : environnement -> typ option -> ident -> bool -> environnement
val child_copy : environnement -> environnement (* copie les parametre de env *)
val nb_vars    : environnement -> int (* nombre de variable definie dans un fonction *)


(* utile pour if, rajoute l'intersection des non_nulls de if_true et if_false*)
val null_inter : environnement -> environnement -> environnement -> unit
val start_env  : environnement

(* constantes *)
val print_int_name : string
val print_str_name : string
val abs_typ_prefix : char
val empty_expr_et_loc : type_expr_et_typ