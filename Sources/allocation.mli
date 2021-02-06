(* module allocation, sert à l'allocation de variable,
   remplace le noms symboliques par des variables globales ou des 
   entiers representant un decalage par rapport a %rbp
   
Dependances:
 - syntax.ml (erreurs.mli) (pour Smap)
 - type_syntax.ml (syntax.ml, erreurs.mli)
 - erreurs.mli*)

open Syntax
open Type_syntax

(*type pa_variable =
   P_Global of string
  |P_Local  of int*)

val rbp : string (* frame pointer *)
val rsp : string (* stack pointer *)

val rax : string (* result storer *)
val al  : string (* byte of %rax *)

val rdx : string (* used in division, temporary var for operations *)
val rsi : string (* saved for tempory use in calculation *)
val rdi : string (* args for malloc and printf *)
val rbx : string (* contient les classes dans les constructeurs *)
val r12 : string (* contient les fermetures dans les fonctions *)

val add_arg     : ident -> unit (*rajoute un argument (avant %rbp) *)
val add         : ident -> unit (*rajoute une variable (apres %rbp) *)
val add_func    : ident -> unit
val add_class   : typ_classe -> unit
val find        : ident -> string
val call_type   : ident -> int (* -1 pour fermeture, 0 pour fonction, 1 pour constructeur*)
val is_global   : ident -> bool

val attr_offset : typ   -> ident  -> int (* position d'un argument par rapport a debut de la classe *)
val class_size  : ident -> int (* taille en memoire d'une classe *)
val class_args  : ident -> int (* nombre d'arguments du constructeur *)
val add_closure : ident list -> parametre list -> unit

val sub_bloc : unit -> unit
val pop_bloc : unit -> unit
val sub_root : unit -> unit
