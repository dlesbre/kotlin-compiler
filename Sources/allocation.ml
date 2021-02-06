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
  | P_Local  of int*)
let sprintf = Format.sprintf
let fprintf = Format.fprintf

let rbp = "%rbp" (* frame pointer *)
let rsp = "%rsp" (* stack pointer *)

let rax = "%rax" (* result storer *)
let al  = "%al"  (* byte of %rax *)

let rdx = "%rdx" (* used in division, temporary var for operations *)
let rsi = "%rsi" (* saved for tempory use in calculation *)
let rbx = "%rbx" (* contient les classes dans les constructeurs *)
let r12 = "%r12" (* contient les fermetures dans les fonctions *)

let rdi = "%rdi" (* pour malloc et printf *)

let failure = Erreurs.compiler_failure "allocation.ml" "Erreur d'allocation"

type cls = {cls_attr : int Smap.t;
            cls_args : int} (* nb d'arguments *)

type root = {mutable root_vars : SSet.t;
             mutable root_func : SSet.t;
             mutable classes   : cls Smap.t} 
             (* class name -> attr name -> offset in class obj *)

type level = {
    mutable n_vars     : string Smap.t;
            n_parent   : namespace;
    mutable n_currsize : int;
    mutable n_argcount : int}

and namespace = 
  |Root of root
  |Level of level

let root = {root_vars = SSet.empty;
            root_func = SSet.empty;
            classes   = Smap.empty}
let namespace = ref(Root(root))

let rec add id = match !namespace with
   Root(r)  -> r.root_vars <- SSet.add id r.root_vars;
  |Level(n) -> n.n_currsize <- n.n_currsize + 1;
               let l = (sprintf "%d(%s)" (8*(-n.n_currsize)) rbp)
               in n.n_vars <- Smap.add id l n.n_vars

let add_func id = root.root_func <- SSet.add id root.root_func

let add_arg id = match !namespace with
   Root(r)  -> failure "add_arg" "Ajout d'un argument dans l'espace root" None
  |Level(n) -> n.n_argcount <- n.n_argcount + 1;
               let l = (sprintf "%d(%s)" (8*(n.n_argcount + 1)) rbp)
               in n.n_vars <- Smap.add id l n.n_vars

let sub_bloc () = 
   namespace := begin match !namespace with
    Root(r)       -> failure "sub_bloc" "Bloc not started..." None
   |Level(n) as p -> Level({n_vars     = Smap.empty;
                            n_parent   = p;
                            n_currsize = n.n_currsize;
                            n_argcount = n.n_argcount})
   end

let pop_bloc () = namespace := match !namespace with
   Root(r)  -> failure "pop_bloc" "Can't pop above root" None
  |Level(n) -> n.n_parent

let sub_root () =
   namespace := begin match !namespace with
    Root(r) as p -> Level({n_vars     = Smap.empty;
                           n_parent   = p;
                           n_currsize = 0;
                           n_argcount = 0})
   |Level(n)     -> failure "root_bloc" "Not started from root" None
   end


let rec find_in id = function
   Level(n) -> if Smap.mem id n.n_vars then
                   Smap.find id n.n_vars
               else
                   find_in id n.n_parent
  |Root(r)  -> if SSet.mem id r.root_vars || SSet.mem id r.root_func || Smap.mem id r.classes then
                   id
               else 
                   failure "find_in" (sprintf "Variable \"%s\" inexistante" id) None

let find id = find_in id (!namespace)

let rec call_type_rec id = function
   Level(n) -> if Smap.mem id n.n_vars then 
                   -1
               else 
                   call_type_rec id n.n_parent
  |Root(r)  -> if SSet.mem id root.root_func then
                   0
               else if Smap.mem id root.classes then
                   1
               else if SSet.mem id root.root_vars then
                   -1
               else
                   failure "call_type_rec" (sprintf "Undefined identifier %s" id) None

let call_type id = call_type_rec id (!namespace)

let rec is_global_rec id = function
   Level(n) -> if Smap.mem id n.n_vars then false else is_global_rec id n.n_parent
  |Root(r)  -> (SSet.mem id root.root_func) ||
               (Smap.mem id root.classes)   ||
               (SSet.mem id root.root_vars) ||
               (failure "is_global_rec" (sprintf "Undefined identifier %s" id) None)

let is_global id = is_global_rec id (!namespace)

let frame_size () = match !namespace with
   Level(n) -> n.n_currsize
  |Root(r)  -> failure "namespace_size" "No size for root space..." None

let attr_offset typ attr = 
  let cls = match typ with
      Tp_Class(n, _)
    | Tp_Nullable(Tp_Class(n, _), _) -> n
    | _ -> failure "attr_offset" "Type invalide" None in
  try 
      Smap.find attr (Smap.find cls root.classes).cls_attr
  with Not_found -> failure 
                    "attr_offset" 
                    (sprintf "Attribut %s ou class %s non trouve..." attr cls)
                    None

let get_name = function
  | P_Var(n, _)
  | P_Val(n, _) -> n

let add_class cls =
  let i = ref (-8) in
  let nms = {n_vars     = Smap.singleton "this" "%rbx"; 
            (* this n'est accessible que du constructeur, a.this invalide en petit kotlin *)
             n_parent   = (!namespace);
             n_currsize = 0;
             n_argcount = 0} in
  namespace := Level(nms);
  root.classes <- Smap.add cls.tc_name {cls_attr = 
                    (List.fold_left (fun map x -> i := !i + 8;
                           let n = match x with
                             | TV_Var(n, _, _)
                             | TV_Val(n, _, _) -> n in
                           nms.n_vars <- Smap.add n (sprintf "%d(%s)" !i rbx) nms.n_vars;
                           Smap.add n !i map)
                      (List.fold_left (fun map x -> i := !i + 8; (* adding params forts*)
                          let n = match x with
                            | P_Var(n, _)
                            | P_Val(n, _) -> n in 
                           nms.n_vars <- Smap.add n (sprintf "%d(%s)" !i rbx) nms.n_vars;
                           Smap.add n !i map) 
                        Smap.empty cls.tc_param)
                    cls.tc_vars);
                 cls_args = List.length cls.tc_param}                                  
                  root.classes

let class_size name =
  try
    Smap.cardinal (Smap.find name root.classes).cls_attr
  with Not_found -> failure "class_size" "undefined class" None

let class_args name = 
  try
    (Smap.find name root.classes).cls_args
  with Not_found -> failure "class_args" "undefined class" None

let add_closure free param =
  let len = List.length param in
  let i = ref (len+2) in
  let k = ref 0 in 
  namespace := Level(
    {n_vars = List.fold_left (fun map (x, _) -> decr i;
        Smap.add x (sprintf "%d(%s)" (8 * !i) rbp) map)
            (List.fold_left (fun map id -> k := !k + 8;
                Smap.add id (sprintf "%d(%s)" !k r12) map)
                Smap.empty
                free)
            param;
     n_parent   = (!namespace);
     n_currsize = 0;
     n_argcount = len})