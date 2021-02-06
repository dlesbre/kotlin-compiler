%{
(* Fichier menhir
parseur realisant l'analyse syntaxique de petit Kotlin
Dépendances:
- erreurs.mli (erreurs.ml)
- syntax.ml (erreurs.mli) *)

  open Syntax

  let sprintf = Format.sprintf

  let collapse_option v = function
    |None     -> v
    |Some(v') -> v'

(*  let base_types = Hashtbl.create 20
  let () =
    Hashtbl.add base_types "Int" Tp_Int;
    Hashtbl.add base_types "Unit" Tp_Unit;
    Hashtbl.add base_types "String" Tp_String;
    Hashtbl.add base_types "Null" Tp_Null;
    Hashtbl.add base_types "Boolean" Tp_Boolean*)

  let syntax_error msg pos1 pos2 = Erreurs.raise_error "Erreur de syntaxe"
                                  msg (Erreurs.make_pos pos1 pos2)

%}

%token TEOF

/* mots-cles */
%token TK_Data TK_Class TK_If TK_Else TK_While
%token TK_Fun TK_Return TK_This TK_Val TK_Var

/* separateurs */
%token TS_Par0 TS_Par1 TS_Brack0 TS_Brack1 TS_Comma TS_Colon TS_Semicolon
%token TS_Dot TS_IntDot TS_Int TS_Arrow
%token T_Eq

/* operateurs */
%token TO_Plus TO_Moins TO_Fois TO_Div TO_Modulo
%token TO_Et TO_Ou TO_Non
%token TO_3eg TO_N2eg TO_2eg TO_Neg
%token TO_Lt TO_Le TO_Gt TO_Ge

%token <string>TIdent
%token <Syntax.constant>TConst

/* pritorite et associativites */
%nonassoc P_If
%nonassoc TK_Else
%nonassoc P_While TK_Return
%right T_Eq
%left TO_Ou
%left TO_Et
%left TO_2eg TO_3eg TO_N2eg TO_Neg
%left TO_Le TO_Lt TO_Ge TO_Gt
%left TO_Plus TO_Moins
%left TO_Fois TO_Modulo TO_Div
%right TO_Non
%left TS_Dot TS_IntDot
%right TS_Arrow
%nonassoc TS_Int

%start kotlin_parser

%type <Syntax.file> kotlin_parser
%type <declaration> decl
%type <variable> var
%type <classe> cls
%type <fonction> fct
%type <parametre> param
%type <parametre_c> param_c
%type <typ> typp
%type <expr_et_loc> exp_et_loc
%type <expr> exp
%type <acces> acc
%type <bloc> block
%type <blocexpr> blocexp
%type <operator> binop

%%

/* regles de grammaire */

kotlin_parser:
  |d = decl*; TEOF { d }

decl:
  |v = var; TS_Semicolon { D_Var(v) }
  |c = cls               { D_Class(c) }
  |f = fct               { D_Fonction(f) }
  |error                 { syntax_error "Une declaration doit commencer par \"var\", \"val\", \"fun\" ou \"data class\".\n  Une declaration de variable doit finir par un symbole \";\"."
                           $startpos $endpos }

var:
  |TK_Val; name = TIdent; t = hastypopt; T_Eq; e = exp_et_loc
           { V_Val(name, t, e, Erreurs.make_pos $startpos $endpos) }
  |TK_Var; name = TIdent; t = hastypopt; T_Eq; e = exp_et_loc
           { V_Var(name, t, e, Erreurs.make_pos $startpos $endpos) }
  |TK_Var; error | TK_Val; error
           { syntax_error "Ce nom de variable est un identificateur invalide"
             $startpos $endpos }
  |TK_Var; a=TIdent; error | TK_Val; a=TIdent; error
           { syntax_error (sprintf "La declaration de la variable \"%s\" est invalide@.  La syntaxe est [val | var] %s [: type]? = expr"
                           a a) $startpos $endpos }
  |TK_Var; name = TIdent; hastypopt; T_Eq; error
  |TK_Val; name = TIdent; hastypopt; T_Eq; error
           { syntax_error (sprintf "Expression invalide dans la declaration de \"%s\""
                           name) $startpos $endpos }
  |TK_Var; name = TIdent; hastypopt; T_Eq; exp_et_loc; TK_Val
  |TK_Var; name = TIdent; hastypopt; T_Eq; exp_et_loc; TK_Var
  |TK_Val; name = TIdent; hastypopt; T_Eq; exp_et_loc; TK_Val
  |TK_Val; name = TIdent; hastypopt; T_Eq; exp_et_loc; TK_Var
           { syntax_error (sprintf "Declarations de variables non separes par \";\" (apres \"%s\")"
                           name) $startpos $endpos }

cls:
  |TK_Data; TK_Class; name = TIdent; template = sqbrace_template_list?;
            TS_Par0; paramc = separated_nonempty_list(TS_Comma, param_c); TS_Par1;
            variables = brack_vars_list?
        { {c_name  = name;
           c_param = paramc;
           c_vars  = collapse_option [] variables;
           c_templ = collapse_option [] template;
           c_pos   = Erreurs.make_pos $startpos $endpos}
        }
  |TK_Data; error
          { syntax_error "Mot-cle \"data\" non suivi de \"class\"."
            $startpos $endpos }
  |TK_Class
          { syntax_error "Mot-cle \"class\" non precede de \"data\"."
            $startpos $endpos }
  |TK_Data; TK_Class; error
          { syntax_error "La declaration de classe est invalide.\n  Syntaxe: \"data class ident <t1, ...>? (val|var a: type, ...) {vars,...}?\"."
            $startpos $endpos }
  |TK_Data; TK_Class; name=TIdent; TO_Lt; TO_Gt
          { syntax_error (sprintf "Liste de types abstraits vides dans la declaration de \"%s\"" name)
            $startpos $endpos }
  |TK_Data; TK_Class; name=TIdent; error
          { syntax_error (sprintf "La declaration de la classe \"%s\" est invalide@.  Syntaxe: \"data class %s <t, ...>? (val|var b : type,...) {vars,...}?\"."
                          name name) $startpos $endpos }
 |TK_Data; TK_Class; name = TIdent; template = sqbrace_template_list?;
            TS_Par0; TS_Par1;
          { syntax_error (sprintf "Liste de parametre de classe vide dans la declaration de \"%s\"."
                          name) $startpos $endpos }
 |TK_Data; TK_Class; name = TIdent; template = sqbrace_template_list?;
            TS_Par0; paramc = separated_nonempty_list(TS_Comma, param_c); error
          { syntax_error (sprintf "Delimiteur \")\" manquant dans la liste des parametres de \"%s\""
                          name) $startpos $endpos }

fct:
  |TK_Fun; template = sqbrace_template_list?; name = TIdent;
           TS_Par0; params = separated_list(TS_Comma, param); TS_Par1;
           tp = hastyp; b = block
        { {f_name  = name;
           f_param = params;
           f_typ   = tp;
           f_body  = b;
           f_templ = collapse_option [] template;
           f_pos   = Erreurs.make_pos $startpos $endpos}
        }
  |TK_Fun; TO_Lt; TO_Gt; name = TIdent
        { syntax_error (sprintf "Liste de types abstraits vide dans la declaration de \"%s\""
                          name) $startpos $endpos }
  |TK_Fun; TO_Lt; TO_Gt
        { syntax_error "Liste de types abstraits vide dans la declaration de cette fonction"
          $startpos $endpos }
  |TK_Fun; sqbrace_template_list?; name = TIdent; error
        { syntax_error (sprintf "La declaration de la fonction \"%s\" est invalide@.  Syntaxe: \"fun [<type abtr,...>]? %s (x : type, ...) [: type]? {bloc}\"."
                          name name) $startpos $endpos }
  |TK_Fun; sqbrace_template_list?; error { syntax_error "Identificateur invalide dans la declaration de la fonction"
                                           $startpos $endpos}

  sqbrace_template_list:
    |TO_Lt; id = separated_nonempty_list(TS_Comma, TIdent); TO_Gt  { id }
    |TO_Lt; error   { syntax_error "Liste de types abstraits invalide.@.  Bien saisir des identificateurs valides separes par \",\"."
                          $startpos $endpos }
    |TO_Lt; id = separated_nonempty_list(TS_Comma, TIdent); error
                    { syntax_error "Delimiteur \">\" manquant."
                          $startpos $endpos }
  brack_vars_list:
    |TS_Brack0; variables = pending_list(TS_Semicolon, var); TS_Brack1 { variables }
    |TS_Brack0; error { syntax_error "Liste de variables invalide.@.  Syntaxe: \"{vars; ... ;?}\"."
                          $startpos $endpos }

  pending_list(sep, X):
    | a = ioption(pending_nonempty_list(sep, X)) { collapse_option [] a }
  pending_nonempty_list(sep, X):
    | a=X                      { [a]  }
    | a=X; sep                 { [a]  }
    | a=X; sep; b=pending_nonempty_list(sep, X) { a::b }

 %inline hastyp:
    |TS_Colon; t=typp { t }
    |{ tp_Unit }

 %inline hastypopt:
    |TS_Colon; t=typp { Some t }
    |{ None }

param:
  |i = TIdent; TS_Colon; t = typp { (i,t) }
  |error { syntax_error "Ce nom de parametre est un identificateur invalide"
                          $startpos $endpos }
  |i = TIdent; error { syntax_error (sprintf "Parametre \"%s\" invalide@.  La syntaxe valide est \"%s : type\"."  i i)
                          $startpos $endpos }
param_c:
  |TK_Val; p = param { P_Val(p) }
  |TK_Var; p = param { P_Var(p) }
  |error { syntax_error "Parametre de classe invalide\n  La syntaxe valide est \"val ident : type\" ou \"var ident : type\"."
                          $startpos $endpos }

typp:
  |i = TIdent; l = sqbrace_typp_list?
             {Tp_Class(i, collapse_option [] l)
               (*match l with
              |None -> if Hashtbl.mem base_types i then
                          Hashtbl.find base_types i
                       else Tp_Class(i, [])
              |Some l' -> if i = "Array" then begin
                    match l' with
                    |[t] -> Tp_Array(t)
                    |_ -> Erreurs.raise_error "Erreur de typage"
                          "La classe Array accepte un unique parametre abstrait"
                          (Erreurs.make_pos $startpos $endpos)
                    end else Tp_Class(i, l')*)}
  |t = typp; TS_Int {match t with
                     |Tp_Nullable(_) as a -> a
                     |_                   -> Tp_Nullable(t, false)}
  |TS_Par0; l = separated_list(TS_Comma, typp); TS_Par1; t=arrow?
              { match t with
              |Some(t') -> Tp_Arrow(l, t')
              |None -> match l with
                       |[t'] -> t'
                       |_ -> syntax_error "Type invalide\n  La syntaxe valide est \"(type)\" ou \"(type, ...) -> type\"."
                          $startpos $endpos }

  arrow:
    |TS_Arrow; t=typp { t }
  sqbrace_typp_list:
    |TO_Lt; id = separated_nonempty_list(TS_Comma, typp); TO_Gt  { id }
    |TO_Lt; error   { syntax_error "Liste de types invalide.\n  Bien saisir des types valides separes par \",\"."
                          $startpos $endpos }
    |TO_Lt; id = separated_nonempty_list(TS_Comma, typp); error
                    { syntax_error "Delimiteur \">\" manquant."
                          $startpos $endpos }

exp_et_loc:
  |e = exp { {expr = e; loc = Erreurs.make_pos $startpos $endpos} }

exp:
  | c = TConst                                { E_Const c }
  | TK_This                                   { E_AccessRead(E_Var("this")) }
  | TO_Non; e = exp_et_loc                    { E_Unop(Op_Non, e) }
  | TO_Moins; e = exp_et_loc                  { E_Unop(Op_Moins, e) }
  | TS_Par0; e = exp_et_loc; TS_Par1          { e.expr }
  | TS_Par0; exp_et_loc; error                { syntax_error "Delimiteur \")\" manquant d'ans l'expression paranthesee." $startpos $endpos }
  | a = acc                                   { E_AccessRead(a) }
  | a = acc; T_Eq; e = exp_et_loc             { E_AccessWrite(a,e) }
  | a = acc; T_Eq; error                      { syntax_error "Expression invalide dans l'affectation." $startpos $endpos}
  | e1 = exp_et_loc; b=binop; e2 = exp_et_loc { E_Binop(b, e1, e2) }
  | TK_Return; e = ioption(exp_et_loc)        { E_Return(collapse_option
                                                  ({expr=E_Const(C_Unit);
                                                    loc = Erreurs.make_pos $startpos $endpos})
                                                   e) }
  | f = TIdent; TS_Par0; e = separated_list(TS_Comma, exp_et_loc); TS_Par1
              { E_Call(f, e) }
  | f = TIdent; TS_Par0; error
              { syntax_error (sprintf "Liste d'arguments invalide dans l'appel de \"%s\".@.  Saisir des expressions séparées par \",\""
                f) $startpos $endpos }
  | f = TIdent; TS_Par0; separated_nonempty_list(TS_Comma, exp_et_loc); error
              { syntax_error (sprintf "Delimiteur \")\" manquant dans l'appel de \"%s\"."
                f) $startpos $endpos }
  | TK_If; TS_Par0; cond = exp_et_loc; TS_Par1; iftrue = blocexp %prec P_If
              { E_If(cond, iftrue, E_Bloc([], (Erreurs.make_single_pos $startpos))) }
  | TK_If; TS_Par0; cond = exp_et_loc; TS_Par1; iftrue = blocexp; TK_Else; iffalse = blocexp
              { E_If(cond, iftrue, iffalse) }
  | TK_If; TS_Par0; exp_et_loc; TS_Par1; error
              { syntax_error "Expression invalide dans le bloc if_true du bloc if"
                 $startpos $endpos }
  | TK_If; TS_Par0; exp_et_loc; error
              { syntax_error "Delimiteur \")\" manquant dans le bloc if."
                 $startpos $endpos }
  | TK_If; TS_Par0; TS_Par1
              { syntax_error "Syntaxe du bloc if invalide, condition vide.\n  Syntaxe \"if (condition) si_vrai [else si_faux]\""
                 $startpos $endpos }
  | TK_If; TS_Par0; error
              { syntax_error "Expression invalide dans la condition du bloc if"
                 $startpos $endpos }
  | TK_If; error
              { syntax_error "Syntaxe du bloc if invalide, paranthese \"(\" manquante.\n  Syntaxe \"if (condition) si_vrai [else si_faux]\""
                 $startpos $endpos }
  | TK_While; TS_Par0; cond = exp_et_loc; TS_Par1; b = blocexp %prec P_While
              { E_While(cond, b) }
  | TK_While; TS_Par0; exp_et_loc; error
              { syntax_error "Delimiteur \")\" manquant dans le bloc while."
                 $startpos $endpos }
  | TK_While; error
              { syntax_error "Syntaxe du bloc while invalide, paranthese \"(\" manquante.\n  Syntaxe \"while (condition) bloc_ou_exp\""
                 $startpos $endpos }
  | TK_Fun; TS_Par0; p=separated_list(TS_Comma, param); TS_Par1;
            tp = hastyp; b = block { E_Lambda(p, tp, b) }
  | TK_Fun; error
              { syntax_error "Syntaxe de fonction anonyme invalide.\n  Syntaxe \"fun (ident : type, ...) bloc\""
                 $startpos $endpos }

block:
  | TS_Brack0; TS_Brack1 { ([], (Erreurs.make_pos $startpos $endpos)) }
  | TS_Brack0; l=pending_nonempty_list(TS_Semicolon, var_or_exp); TS_Brack1
             { (l, (Erreurs.make_pos $startpos $endpos)) }
  | TS_Brack0; error { syntax_error "Bloc invalide.\n  Saisir une liste d'expression separees par des symboles \";\"." $startpos $endpos }

var_or_exp:
  |v = var        { VE_Var(v) }
  |e = exp_et_loc { VE_Exp(e) }

%inline blocexp:
  | b = block      { E_Bloc(b) }
  | e = exp_et_loc { E_Expr(e) }

acc:
  | c = TIdent                            { E_Var(c) }
  | e = exp_et_loc; TS_Dot; c = TIdent    { E_VarDot(e, c) }
  | e = exp_et_loc; TS_IntDot; c = TIdent { E_VarIntDot(e, c) }
  | exp_et_loc; TS_Dot; error | exp_et_loc; TS_IntDot; error
                { syntax_error "Identifiant invalide dans l'acces" $startpos $endpos}
 %inline binop:
    | TO_Plus   { Op_Plus }
    | TO_Moins  { Op_Moins }
    | TO_Fois   { Op_Fois }
    | TO_Div    { Op_Div }
    | TO_Modulo { Op_Modulo }
    | TO_Et     { Op_Et }
    | TO_Ou     { Op_Ou }
    | TO_3eg    { Op_3eg }
    | TO_N2eg   { Op_N2eg }
    | TO_2eg    { Op_2eg }
    | TO_Neg    { Op_Neg }
    | TO_Lt     { Op_Lt }
    | TO_Le     { Op_Le }
    | TO_Gt     { Op_Gt }
    | TO_Ge     { Op_Ge }