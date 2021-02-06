(* Module erreurs, gere les exception du compilateur
Aucune dependance.

Invalid_Input -> code de sortie 1
Compiler_failure -> code de sortie 2

les exceptions sont leves par raise_error compiler_failure respectivement
apres impression du message, elle sont rattrapees dans execute*)


type position

exception Invalid_Input
exception Compiler_Failure

val file : string ref

val empty_pos       : position
val make_single_pos : Lexing.position -> position
val make_pos        : Lexing.position -> Lexing.position -> position
val file_start_pos  : unit -> position

val raise_error     : string -> string -> position -> 'a
val show_warning    : string -> string -> position -> unit
                   (* erreur -> message -> pos *)
val compiler_failure: string -> string -> string -> string -> position option  -> 'a
                   (* ml file -> erreur -> fct   -> message -> pos lecture *)

val execute: ('a -> 'b) -> 'a -> 'b
(* execute f a calcule la fonction f(a) et
   termine avec les bons codes d'erreurs au besoin *)