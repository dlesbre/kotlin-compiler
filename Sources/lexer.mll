{
(* Fichier ocamllex, definit le lexer
Dependances:
- parser.mli (parser.mly syntax.ml erreurs.mli)
- erreurs.mli *)

open Parser

let lexer_error msg pos =
  Erreurs.raise_error "Erreur de syntaxe" msg (Erreurs.make_single_pos pos)

let lexer_failure =
  Erreurs.compiler_failure "lexer.mll" "Echec du Lexer"

let current_string = ref ""

let kwds = Hashtbl.create 20
let sbls = Hashtbl.create 40
let () =
  Hashtbl.add kwds "data"   TK_Data;
  Hashtbl.add kwds "class"  TK_Class;
  Hashtbl.add kwds "if"     TK_If;
  Hashtbl.add kwds "else"   TK_Else;
  Hashtbl.add kwds "while"  TK_While;
  Hashtbl.add kwds "true"   (TConst(C_Bool(true)));
  Hashtbl.add kwds "false"  (TConst(C_Bool(false)));
  Hashtbl.add kwds "null"   (TConst(C_Null));
  Hashtbl.add kwds "fun"    TK_Fun;
  Hashtbl.add kwds "return" TK_Return;
  Hashtbl.add kwds "this"   TK_This;
  Hashtbl.add kwds "val"    TK_Val;
  Hashtbl.add kwds "var"    TK_Var

let () =
  Hashtbl.add sbls "+"   TO_Plus;
  Hashtbl.add sbls "-"   TO_Moins;
  Hashtbl.add sbls "*"   TO_Fois;
  Hashtbl.add sbls "/"   TO_Div;
  Hashtbl.add sbls "%"   TO_Modulo;
  Hashtbl.add sbls "&&"  TO_Et;
  Hashtbl.add sbls "||"  TO_Ou;
  Hashtbl.add sbls "!"   TO_Non;
  Hashtbl.add sbls "===" TO_3eg;
  Hashtbl.add sbls "!==" TO_N2eg;
  Hashtbl.add sbls "=="  TO_2eg;
  Hashtbl.add sbls "!="  TO_Neg;
  Hashtbl.add sbls "<"   TO_Lt;
  Hashtbl.add sbls "<="  TO_Le;
  Hashtbl.add sbls ">"   TO_Gt;
  Hashtbl.add sbls ">="  TO_Ge;
  Hashtbl.add sbls "="   T_Eq;
  Hashtbl.add sbls "("   TS_Par0;
  Hashtbl.add sbls ")"   TS_Par1;
  Hashtbl.add sbls "{"   TS_Brack0;
  Hashtbl.add sbls "}"   TS_Brack1;
  Hashtbl.add sbls ","   TS_Comma;
  Hashtbl.add sbls ":"   TS_Colon;
  Hashtbl.add sbls ";"   TS_Semicolon;
  Hashtbl.add sbls "."   TS_Dot;
  Hashtbl.add sbls "?."  TS_IntDot;
  Hashtbl.add sbls "?"   TS_Int;
  Hashtbl.add sbls "->"  TS_Arrow


let match_ident ident =
  if Hashtbl.mem kwds ident then
    (Hashtbl.find kwds ident)
  else TIdent(ident)

let match_symbol symb =
 try Hashtbl.find sbls symb
 with Not_found -> lexer_failure "match_symbol"
         (Format.sprintf "La chaine '%s' n'est pas un symbole valide" symb)
         None

}

let endl = '\n' | '\r'

let symbol = "+" | "-" | "*" | "/" | "%" | "&&" | "||" | "!"
             | "===" | "!==" | "==" | "!=" | "<" | "<=" | ">" | ">="
             | "(" | ")" | "{" | "}" | "." | ":" | "," | ";" | "?." | "?" | "->" | "="

let alpha = ['a'-'z' 'A'-'Z' '_']
let bit = ['0' '1']
let hexa = ['0'-'9' 'a'-'f' 'A'-'F']
let digit = ['0'-'9']

let ident = alpha (alpha | digit)*
let entier =      digit | (digit (digit | '_')* digit)
  | ("0x" | "0X") (hexa | hexa  (hexa  | '_')* hexa)
  | ("0b" | "0B") (bit  | bit   (bit   | '_')* bit)

let esc = "\\t" | "\\n" | "\\\\" | "\\\""

rule token = parse
  | [' ' '\t']     { token lexbuf }
  | endl           { Lexing.new_line lexbuf; token lexbuf }
  | "//"           { commentaire_1ligne lexbuf }
  | "/*"           { commentaire_multiligne lexbuf; token lexbuf }
  | symbol as s    { match_symbol s }
  | ident as s     { match_ident s }
  | entier as n    { match int_of_string_opt n with
                      |None    -> lexer_error "Entier non representable sur 32bits" lexbuf.lex_curr_p
                      |Some(n') -> if n' < 2147483648 then
					                 TConst (C_Int (n')) 
								   else
								     lexer_error "Entier non representable sur 32bits" lexbuf.lex_curr_p}
  | '"'            { current_string := ""; string_parse lexbuf }
  | eof            { TEOF }
  | _ as c         { lexer_error (Format.sprintf "Caractere '%c' non reconnu" c)
                                 lexbuf.lex_curr_p }

and commentaire_1ligne = parse
  | eof  { TEOF }
  | endl { Lexing.new_line lexbuf; token lexbuf }
  | _    { commentaire_1ligne lexbuf }

and commentaire_multiligne = parse
  | eof  { lexer_error "Fin de fichier lors du scan d'un commentaire"
           lexbuf.lex_curr_p }
  | "/*" { commentaire_multiligne lexbuf; commentaire_multiligne lexbuf }
  | "*/" {  }
  | endl { Lexing.new_line lexbuf; commentaire_multiligne lexbuf }
  | _    { commentaire_multiligne lexbuf }

and string_parse = parse
  | endl   { Lexing.new_line lexbuf;
             lexer_error "Fin de ligne lors du scan d'une chaine de caracteres"
             lexbuf.lex_curr_p }
  | esc as e { current_string := !current_string ^ e; string_parse lexbuf }
  | "\""   { TConst (C_Str (!current_string)) }
  | eof    { lexer_error "Fin de fichier lors du scan d'une chaine de caracteres"
                         lexbuf.lex_curr_p }
  | _ as c { let id = int_of_char c in
             if (32 <= id && id <= 126 && c <> '\\') then (
               current_string := !current_string ^ (String.make 1 c);
               string_parse lexbuf)
             else
               lexer_error
               (Format.sprintf "Le caractere '%c' (code ascii %d) ne peut être stockée dans un chaine.@.  Les caractères valides ont code ascii compris entre 32 et 126@.  (sauf \" et \\), ainsi que \\n \\\\ \\\" et \\t."
               c id)
               lexbuf.lex_curr_p }