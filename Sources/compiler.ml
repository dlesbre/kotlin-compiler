(* compile les arbres de typages vers de l'assembleur x86-64 AT&T
Dépendances:
 - syntax.ml (erreurs.mli)
 - type_syntax.ml (syntax.ml, erreurs.mli)
 - allocation.mli (type_syntax.ml, syntax.ml erreurs.mli)*)

open Syntax
open Type_syntax
open Allocation

let fprintf = Format.fprintf
let sprintf = Format.sprintf
let failure = Erreurs.compiler_failure "compiler.ml" "Echec de la production de code"

(* all labels start with "." to differ from kotlin identifiers *)
let make_label name =
  let i = ref (-1) in
  function () -> (incr i; Format.sprintf ".%s_%d" name !i)
let make_dual_label name name2 =
  let i = ref (-1) in
  function () -> (incr i; Format.sprintf ".%s_%d" name !i, 
                          Format.sprintf ".%s_%d" name2 !i)

let string_label = make_label "string"
let if_label     = make_dual_label "if" "end_if"
let while_label  = make_dual_label "while" "end_while"
let lazy_label   = make_label "lazy"    (* evaluation paresseuse *)
let null_label   = make_label "null"    (* acces exp?.attr *)
let wrapper      = make_label "wrapper" (* convrtir constructeur en fcts *)

let attr_nb = ref 0
let strings = ref Smap.empty
let lambdas = ref []
let wrappers= ref []
let classes = ref []
let globals = ref []
let fcts    = ref []
let data    = ref []
let main    = ref ""
let save_r12= ref false

let append x y = y := x::(!y)

let leave fmt () = fprintf fmt "\tmovq %s, %s\n\tpopq %s\n" 
                           rbp rsp rbp
let enter fmt n  = if n = 0 then fprintf fmt "\tpushq %s\n\tmovq %s, %s\n" 
                                         rbp
                                         rsp rbp 
                            else  fprintf fmt "\tpushq %s\n\tmovq %s, %s\n\tsubq $%d, %s\n" 
                                          rbp rsp rbp (8*n) rsp

let rec remove_globals = function
  | [] -> []
  | t::q when is_global t -> remove_globals q
  | t::q -> t::(remove_globals q)

let rec make_closure fmt (l, n) = match l with
  | []   -> fprintf fmt ""
  | t::q -> fprintf fmt "\tmovq %s, %s\n\tmovq %s, %d(%s)\n%a"
                    (find t) rdx
                    rdx (8*n) rax
                    make_closure (q, n+1)

let convert_constant fmt c = match c with
  | C_Int(i) -> fprintf fmt "$%d" i
  | C_Str(s) -> let label = 
                  if Smap.mem s !strings then
                    Smap.find s !strings
                  else
                    (let lbl = string_label () in
                    strings := Smap.add s lbl !strings; lbl) in
                fprintf fmt "$%s" label
  | C_Bool(b) -> fprintf fmt (if b then "$1" else "$0")
  | C_Null
  | C_Unit -> fprintf fmt "$0"

let rec convert_type_expr_et_typ fmt exp =
  match exp.t_expr with
  | TE_Const(constant) -> fprintf fmt "\tmovq %a, %s\n" convert_constant constant rax
  | TE_Unop(operator, exp') -> 
       fprintf fmt "%a\t%s %s\n" 
               convert_type_expr_et_typ exp'
               (match operator with
                    Op_Moins -> "negq"
                   |Op_Non   -> "xorq $1,"
                   |op -> failure "convert_type_expr_et_typ" "operateur non unaire" None) 
               rax
  | TE_Binop(operator, exp1, exp2) ->
      begin match operator with
        | Op_Et | Op_Ou ->
            let lbl = lazy_label () in
            fprintf fmt "%a\ttestq %s, %s\n\tj%s %s\n%a%s:\n"
                    convert_type_expr_et_typ exp1
                    rax rax
                    (if operator == Op_Et then "e" else "ne")
                    lbl
                    convert_type_expr_et_typ exp2
                    lbl
        | Op_Plus | Op_Moins | Op_Fois ->
            fprintf fmt "%a\tpushq %s\n%a\tpopq %s\n\t%s %s, %s\n"
                    convert_type_expr_et_typ exp2
                    rax
                    convert_type_expr_et_typ exp1
                    rdx
                    begin match operator with
                        Op_Plus   -> "addq"
                      | Op_Moins  -> "subq"
                      | Op_Fois   -> "imulq"
                      | _         -> failure "convert_type_expr_et_typ" "operateur non arithmetique" None 
                    end rdx rax
        | Op_Div | Op_Modulo ->
            fprintf fmt "%a\tpushq %s\n%a\tpopq %s\n\tcqto\n\tidivq %s\n%s"
                    convert_type_expr_et_typ exp2
                    rax
                    convert_type_expr_et_typ exp1
                    rsi
                    rsi
                    (if operator = Op_Div then "" else "\tmovq %rdx, %rax\n")
        | Op_Le | Op_Lt | Op_Gt | Op_Ge | Op_2eg | Op_Neg | Op_3eg | Op_N2eg ->
            fprintf fmt ("%a\tpushq %s\n%a\tmovq %s, %s\n\tpopq %s\n" ^^ 
                         "\txorq %s, %s\n\tcmpq %s, %s\n\tset%s %s\n")
                    convert_type_expr_et_typ exp1
                    rax
                    convert_type_expr_et_typ exp2
                    rax rsi (* exp2 -> rsi *)
                    rdx     (* exp1 -> rdx *)
                    rax rax (* 0 -> rax *)
                    rsi rdx (* flags of exp1 operator exp2 *)
                    (match operator with
                        Op_Le            -> "le"
                      | Op_Lt            -> "l"
                      | Op_Gt            -> "g" 
                      | Op_Ge            -> "ge"
                      | Op_2eg | Op_3eg  -> "e"
                      | Op_Neg | Op_N2eg -> "ne"
                      | _ -> failure "convert_type_expr_et_typ" "Pas un operateur de comparaison" None)
                    al
        | Op_Non -> failure "convert_type_expr_et_typ" "Operateur non binaire" None
      end
  | TE_If(cond, bloc_if, bloc_else) -> 
      let if_lbl, end_if_lbl = if_label () in
      fprintf fmt "%a\ttestq %s, %s\n\tjne %s\n%a\tjmp %s\n%s:\n%a%s:\n"
              convert_type_expr_et_typ cond
              rax rax (* flags de %rax *)
              if_lbl  (* jump to if si non nul *)
              convert_typ_blocexpr bloc_else
              end_if_lbl (* jump to endif *)
              if_lbl
              convert_typ_blocexpr bloc_if
              end_if_lbl
  | TE_While(cond, blocexpr) ->
      let while_lbl, end_while_lbl = while_label () in
      fprintf fmt "\tjmp %s\n%s:\n%a%s:\n%a\ttestq %s, %s\n\tjne %s\n\txorq %s, %s\n"
              end_while_lbl
              while_lbl
              convert_typ_blocexpr blocexpr
              end_while_lbl
              convert_type_expr_et_typ cond
              rax rax (* drapeaux de %rax and %rax*)
              while_lbl
              rax rax (* type unit *)
  | TE_Return(exp') ->
      fprintf fmt "%a%a\tret\n"
              convert_type_expr_et_typ exp'
              leave ()
  | TE_AccessRead(a) -> 
    begin match a with
      | TE_Var(name) -> begin match call_type name with
                          | 0 -> (* create closure *)
                            fprintf fmt "\tmovq $8, %s\n\tcall malloc\n\tmovq $%s, (%s)"
                                    rdi
                                    name rax
                          | -1 -> 
                            fprintf fmt "\tmovq %s, %s\n" (find name) rax
                          | _ -> (* create constructor wrapper *)
                            let lbl = wrapper () in
                            fprintf fmt "\tmovq $8, %s\n\tcall malloc\n\tmovq $%s, (%s)"
                                    rdi
                                    lbl rax;
                            append (lbl, name) wrappers
                        end
      | TE_VarDot(exp, attr) -> fprintf fmt "%a\tmovq %d(%s), %s\n"
                                        convert_type_expr_et_typ exp
                                        (attr_offset exp.t_typ attr) rax rax
      | TE_VarIntDot(exp, attr) -> 
          let lbl = null_label () in
          fprintf fmt "%a\ttestq %s, %s\n\tje %s\n\tmovq %d(%s), %s\n%s:"
                  convert_type_expr_et_typ exp
                  rax rax (* %rax est-il nul*)
                  lbl (* si nul on evalue pas *)
                  (attr_offset exp.t_typ attr) rax rax
                  lbl
    end
  | TE_AccessWrite(a, new_value) -> 
      begin match a with
      | TE_Var(name) -> fprintf fmt "%a\tmovq %s, %s\n\txorq %s, %s\n" 
                                convert_type_expr_et_typ new_value
                                rax (find name)
                                rax rax
      | TE_VarDot(addr, attr) -> fprintf fmt "%a\tpushq %s\n%a\tpopq %s\n\tmovq %s, %d(%s)\n\txorq %s, %s\n"
                                        convert_type_expr_et_typ new_value
                                        rax (* on place le resultat a ecrire sur la pile *)
                                        convert_type_expr_et_typ addr (* objet -> rax *)
                                        rdx (* resultat -> rdx *)
                                        rdx (attr_offset addr.t_typ attr) rax 
                                         (* rdx -> offset(%rax)*)
                                        rax rax (* type unit *)
      | TE_VarIntDot(addr, attr) -> 
          let lbl = null_label () in
          fprintf fmt "%a\ttestq %s, %s\n\tje %s\n\tpushq %s\n%a\tpopq %s\n\tmovq %s, %d(%s)\n%s:\txorq %s, %s\n"
                  convert_type_expr_et_typ addr (* on commence par l'addresse 
                                                  pour ne pas calculer en cas de nullite*)
                  rax rax (* %rax est-il nul*)
                  lbl (* si nul on evalue pas *)
                  rax (* addresse sur la pils *)
                  convert_type_expr_et_typ new_value (* new_value -> rax *)
                  rdx (* addresse -> rdx *)
                  rax (attr_offset addr.t_typ attr) rdx (* rax -> offset(rdx)*)
                  lbl
                  rax rax
    end
  | TE_Call(ident, args) ->
      let deb, fin = if !save_r12 then "\tpushq %r12\n", "\tpopq %r12\n" else "", "" in
      begin match call_type ident with
        | 0  -> (* fonction *)
          fprintf fmt "%s%a\tcall %s\n\taddq $%d, %s\n%s"
                  deb
                  calc_args args
                  (if ident = "main" then ".main" else ident)
                  ((List.length args)*8) rsp
                  fin
        | -1 -> (* fermeture *)
          let closure = (find ident) in
          fprintf fmt "%s\tpushq %s\n%a\tmovq %s, %s\n\tcall *(%s)\n\taddq $%d, %s\n%s"
                  deb
                  closure (* fermeture en premier argument *)
                  calc_args args
                  closure rdx (* evite d'avoir de reference memoire imbriquees *)
                  rdx
                  ((List.length args + 1)*8) rsp
                  fin
        | _ -> (* constructeur *)
          let x = !attr_nb in
          attr_nb := -8;
          fprintf fmt "%s\tmovq $%d, %s\n\tcall malloc\n\tpushq %s\n\tmovq %s, %s\n%a\tcall %s\n\tmovq %s, %s\n\tpopq %s\n%s"
                  deb
                  ((class_size ident)*8) rax
                  rbx (* saving old %rbx *)
                  rax rbx (* moving allocated class to %rbx *)
                  init_class args (* calculating args *)
                  ident (* call constructor *)
                  rbx rax (* moving class to result register %rax *)
                  rbx (* restoring old %rbx *)
                  fin;
          attr_nb := x
      end
  | TE_Lambda(lambda) ->
      let lambda' = {lambda with
                     l_free = remove_globals lambda.l_free} in
      fprintf fmt "\tmovq $%d, %s\n\tcall malloc\n\tmovq $%s, (%s)\n%a"
              ((List.length lambda'.l_free + 1)*8) rdi
              lambda'.l_name rax
              make_closure (lambda'.l_free, 1);
      append lambda' lambdas
and calc_args fmt = function
  | []   -> fprintf fmt ""
  | t::q -> fprintf fmt "%a\tpushq %s\n%a" 
                    convert_type_expr_et_typ t
                    rax
                    calc_args q
and init_class fmt = function
  | []   -> fprintf fmt ""
  | t::q -> attr_nb := !attr_nb + 8;
            fprintf fmt "%a\tmovq %s, %d(%s)\n%a"
                    convert_type_expr_et_typ t
                    rax (!attr_nb) rbx
                    init_class q
and convert_typ_blocexpr fmt = function
  | TE_Bloc(b) -> convert_typ_bloc fmt b
  | TE_Expr(e) -> convert_type_expr_et_typ fmt e
and convert_typ_bloc fmt (ve_list, _) =
    if ve_list <> [] then begin
        sub_bloc ();
        List.iter (convert_typ_var_or_expr fmt) ve_list;
        pop_bloc ();
    end
    else fprintf fmt "\txorq %s, %s\n" rax rax (* type unit *)
and convert_typ_var_or_expr fmt = function
  | TVE_Var(v) -> convert_typ_variable fmt v
  | TVE_Exp(e) -> convert_type_expr_et_typ fmt e
and convert_typ_variable fmt = function
  | TV_Var(n,_,e)
  | TV_Val(n,_,e) -> add n;
                     fprintf fmt "%a\tmovq %s, %s\n"
                             convert_type_expr_et_typ e
                             rax (find n)

let sfmt = Format.str_formatter
let flush = Format.flush_str_formatter

let convert_typ_global_variable = function
  | TGV_Var(i,_,e,n)
  | TGV_Val(i,_,e,n) -> add i;
                        sub_root ();
                        fprintf sfmt "%a%a\tmovq %s, %s\n%a"
                            enter n
                            convert_type_expr_et_typ e
                            rax i
                            leave ();
                        append (flush ()) globals;
                        append (sprintf "%s:\t.quad 0\n" i) data;
                        pop_bloc ()

let convert_typ_fonction f =
  add_func f.tf_name;
  sub_root ();
  List.iter (fun (x,_) -> add_arg x) (List.rev f.tf_param); (* first arguments are on top...*)
  fprintf sfmt "%s:\n%a%a"
          (if f.tf_name <> "main" then f.tf_name else ".main")
          enter f.tf_size
          convert_typ_bloc f.tf_body;
  if f.tf_name <> "main" then append (flush ()) fcts
                         else main := flush ();
  pop_bloc ()

let rec convert_typ_v_list fmt = function
  | []   -> fprintf fmt ""
  | t::q -> match t with
             | TV_Var(n, _, e) 
             | TV_Val(n, _, e) -> fprintf fmt "%a\tmovq %s, %s\n%a"
                                          convert_type_expr_et_typ e
                                          rax (find n)
                                          convert_typ_v_list q

let convert_typ_classe cls =
    add_class cls; (* adds a bloc... *)
    fprintf sfmt "%s:\n%a\tret\n"
            cls.tc_name
            convert_typ_v_list cls.tc_vars;
    append (flush ()) classes;
    pop_bloc ()

let convert_typ_declaration = function
  | TD_Var(v) -> convert_typ_global_variable v
  | TD_Class(c) -> convert_typ_classe c
  | TD_Fonction(f) -> convert_typ_fonction f

let repr_comment comment = if comment <> "" then "\n\t\t# " ^ comment ^"\n" else ""

let rec repr_list fmt (list, comment) = match list with
  | []   -> fprintf fmt "" (* no comment if empty list *)
  | t::q -> fprintf fmt "%s%a%s" 
              (repr_comment comment)
              repr_list (q, "") t
let repr_strings fmt () =
  if Smap.cardinal !strings <> 0 then fprintf fmt "\n\t\t# Chaines de caracteres\n";
  Smap.iter (fun k v -> fprintf fmt "%s:\t.string \"%s\"\n" v k) !strings

let rec get_cls id = function
  | []   -> failure "get_cls" "Class doesn't exist" None
  | t::q -> if t.tc_name = id then t
                              else get_cls id q

let rec wrappers_and_lambdas fmt comment =
  match !wrappers, !lambdas with
    | [], []  -> fprintf fmt ""
    | (pseudo, name)::q, _ -> 
                 wrappers := q;
                 let size = class_args name in
                 sub_bloc ();
                 for i = 0 to size - 1 do
                    add_arg (sprintf "%d" i);
                 done;
                 fprintf fmt "%s%s:\n%a%a"
                         (repr_comment comment)
                         pseudo
                         convert_type_expr_et_typ 
                         ({t_expr = TE_Call(name, List.init size
                                    (fun i -> {t_expr = TE_AccessRead(TE_Var(sprintf "%d" i));
                                                 t_typ = tp_Unit}));
                           t_typ = tp_Unit})
                         wrappers_and_lambdas "";
                 pop_bloc ()
    | _, t::q -> lambdas := q;
                 save_r12 := true;
                 add_closure t.l_free t.l_param;
                 fprintf fmt "%s%s:\n%a\tmovq %d(%s), %s\n%a%a"
                         (repr_comment comment)
                         t.l_name
                         enter t.l_size
                         ((List.length t.l_param + 2)*8) rbp r12 (* closure -> %r12 *)
                         convert_typ_bloc t.l_body
                         wrappers_and_lambdas "";
                 pop_bloc ()


let convert_typ_file type_tree outpath =
  add_func ".print_int";
  add_func ".print_str";
  List.iter convert_typ_declaration type_tree;
  let oc = open_out outpath in
  let fmt = Format.formatter_of_out_channel oc in
  fprintf fmt 
"\t\t.text
\t\t.globl main
main:
%a
\t\t# Appel a main
\tpushq $0 # argument de main
\tcall .main
\taddq $8, %s
\tret
%s%a%s%a%a
\t\t.data\n%a%a
\t\t# Formatteurs pour printf
%s@."
          repr_list (!globals, "Initialisation des variables globales")
          rsp !main
          repr_list (!fcts, "Fonctions globales et polymorphes")
         "
\t\t# Fonctions d'impression (predefinies)
.print_int:
\tmovl 8(%rsp), %esi
\tmovq $.sprint_int, %rdi 
\txorq %rax, %rax
\tcall printf
\tret
.print_str:
\tmovq 8(%rsp), %rsi
\ttest %rsi, %rsi
\tje .print_null
\tmovq $.sprint_str, %rdi 
\txorq %rax, %rax
\tcall printf
\tret
.print_null:
\tmovq $.sprint_null, %rdi 
\txorq %rax, %rax
\tcall printf
\tret\n"
          repr_list (!classes, "Constructeurs de classes")
          wrappers_and_lambdas "Fonctions anonymes"
          repr_list (!data, "Variables globales")
          repr_strings ()
          ".sprint_int:\t.string \"%d\"\t# afficher sur 32 bits\n.sprint_str:\t.string \"%s\"\n.sprint_null:\t.string \"null\"";
  close_out oc