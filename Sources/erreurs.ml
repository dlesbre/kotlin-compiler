(* Module erreurs, gere les exception du compilateur
Aucune dependance.

Invalid_Input -> code de sortie 1
Compiler_failure -> code de sortie 2

les exceptions sont leves par raise_error compiler_failure respectivement
apres impression du message, elle sont rattrapees dans execute*)

type position = {file      : string;
                 line_start: int;
                 line_end  : int;
                 char_start: int;
                 char_end  : int}

exception Invalid_Input
exception Compiler_Failure

let file = ref ""

let make_single_pos (pos : Lexing.position) =
  {file       = !file;
     line_start = pos.pos_lnum;
     char_start = pos.pos_cnum - pos.pos_bol;
     line_end   = pos.pos_lnum;
     char_end   = pos.pos_cnum - pos.pos_bol}

let make_pos (startpos : Lexing.position) (endpos : Lexing.position) =
    {file       = !file;
     line_start = startpos.pos_lnum;
     char_start = startpos.pos_cnum - startpos.pos_bol;
     line_end   = endpos.pos_lnum;
     char_end   = endpos.pos_cnum - endpos.pos_bol}

let file_start_pos () =
    {file       = !file;
     line_start = 1;
     char_start = 0;
     line_end   = 1;
     char_end   = 0}

let empty_pos = {file = ""; line_start = 0; line_end = 0; char_start = -1; char_end = -1}

let repr_pos pos =
    Format.sprintf "File \"%s\", line %d, characters %d-%d"
                  pos.file pos.line_start pos.char_start pos.char_end

let raise_error erreur message pos =
    Format.printf "%s@.%s :@.  %s@." (repr_pos pos) erreur message;
    raise Invalid_Input

let show_warning erreur message pos =
    Format.printf "%s@.%s :@.  %s@." (repr_pos pos) erreur message

let compiler_failure fichier erreur fonction message position  =
    Format.printf "%s@.Compiler Error (%s) :@.  %s@.Error raised from function %s in %s@."
                  (match position with
                     |Some(pos) -> repr_pos pos
                     |None -> "No postion")
                  erreur message fonction fichier;
    raise Compiler_Failure

let execute f a =
  try f a with
   |Invalid_Input    -> exit 1
   |Compiler_Failure -> exit 2