(* Fichier principal, gére les arguments de la
ligne de commande et appel les differents modules
Dependances:
- erreurs.mli
- lexer.ml (lexer.mll)
- parser.mli (parser.mly, syntax.ml)
- typeur.mli (syntax.ml, type_syntax.ml, environnement.mli, type_functions.mli)
- compiler.ml (allocator.mli, type_syntax.ml, syntax.ml erreurs.mli 
*)

(* parametres *)
let parse_only = ref false
let type_only = ref false
let filepath = ref ""
let outpath = ref ""

let compile (filepath, outpath) =
  Erreurs.file := filepath;
  let channel = open_in filepath in
  let lexbuf = Lexing.from_channel channel in
  let syntax_tree = Parser.kotlin_parser Lexer.token lexbuf in
  close_in channel;
  if !parse_only then
      Format.printf "Parsing du fichier realise avec succes@."
  else begin
      let type_tree = Typeur.type_file syntax_tree in
      if !type_only then
          Format.printf "Parsing et typage realise avec succes@."
      else begin
          Compiler.convert_typ_file type_tree outpath
      end
  end

let exists filename =
  try close_in (open_in filename); true
  with Sys_error _ -> false

let main () =
  Arg.parse
  ["--parse-only", Arg.Set parse_only, " se contente de parser le fichier argument";
   "--type-only", Arg.Set type_only, " se contente de parser et typer le fichier argument";
   "-o", Arg.Set_string outpath, " specifie le fichier de sortie (par defaut <non_entree>.s"]
   (function path ->
     if !filepath = "" then
       filepath := path
     else (Format.printf "Erreur de ligne de commande:@.  Ce programme n'accepte qu'un seul nom de fichier en argument@.  Je ne sais pas quoi faire de %s@."
           path; exit 1))
    "Compilateur de petit-Kotlin\n";
  if !filepath = "" then
     (Format.printf "Erreur de ligne de commande:@.  Aucun nom de fichier reçu en argument@.  Si vous ne me demandez rien, je ne fais rien...@.";
      exit 1);
  if exists !filepath then begin
    if !outpath = "" then
       outpath := Filename.remove_extension (!filepath) ^ ".s";
    Erreurs.execute compile (!filepath, !outpath)
  end else
     (Format.printf "Erreur de lecture de fichier:@.  Je ne trouve pas ou n'arrvie pas à lire \"%s\"@.  Du coup j'ai un peu de mal à le compiler...@."
      !filepath; exit 1)

let () = main ()