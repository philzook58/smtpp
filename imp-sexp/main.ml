
open Core_kernel

module Imp = struct

  type expr = Sexp.t [@@deriving sexp]

  type stmt =
    | Skip
    | If of expr * stmt * stmt
    | Begin of stmt list [@sexp.list]
    | Set of Sexp.t * expr
    [@@deriving sexp]

end
  (*

  normalize to 3 address code or not
  Deal with memory or not.

  *)
module FlowChart = struct
(* partial evaluation has a flowchart langauge 
4.2.1 page 80

Make concrete syntax.
I'm never going to get primus lisp working in browser.

*)

type label = string [@@deriving sexp, compare, equal, hash]
type var = string [@@deriving sexp, compare, equal, hash]
type op = Sexp.t [@@deriving sexp, compare, equal, hash] (* Add | Suib .. etc *)
type expr = op * var * var [@@deriving sexp, compare, equal, hash]
type assign = var * expr [@@deriving sexp, compare, equal, hash]
type jmp = Goto of label | Ite of var * label * label | Return of var [@@deriving sexp, compare, equal, hash]
type blk = label * assign list * jmp [@@deriving sexp, compare, equal, hash]
type prog = blk list [@@deriving sexp, compare, equal, hash]

let to_bil _x = ()

end

(* First order functional language. *)
module FOF = struct
  type var = string
  type value = Bool of bool | Int of int
  type expr =
  | Const of value
  | Apply of string * expr list
  | Var of var
  (*  | Prim of op * expr list *)
  | Ite of expr * expr * expr
  type def = string * var list * expr 
end


(*
ppx would be cool. Write in a subset of ocaml
Or possibly coq parsing?
*)

module DLogic = Datalog.Default
module DParser = Datalog.Parser
module DLexer = Datalog.Lexer

(*

I could use souffle, z3 but that complicates the web compilation story.

fp = Z3.Fixedpoint.mk_fixedpoint ctx in
let queries = Z3.Fixedpoint.parse_string fp "(=>  (and yada)   )"


*)
let parse_file filename =
  Format.printf "%% parse file %s@." filename;
  let ic = In_channel.create filename in
  let lexbuf = Lexing.from_channel ic in
  try
    let clauses = DParser.parse_file DLexer.token lexbuf in
    In_channel.close ic;
    clauses
  with Parsing.Parse_error ->
    (* error, signal it and return no clause *)
    Format.eprintf "%% error parsing %s (%s)@." filename (DLexer.print_location lexbuf);
    []

let parse_string (s :string) : DLogic.clause list =
  let lexbuf = Lexing.from_string s in
  try
    let clauses = DParser.parse_file DLexer.token lexbuf in
    List.map ~f:DLogic.clause_of_ast clauses
  with Parsing.Parse_error ->
    (* error, signal it and return no clause *)
    Format.eprintf "%% error parsing %s (%s)@." s (DLexer.print_location lexbuf);
    []

let clauses = parse_file "edge.dl"
let clauses = List.map ~f:DLogic.clause_of_ast clauses
let () = List.iter ~f:(Format.printf "  clause @[<h>%a@]@." DLogic.pp_clause) clauses

let ex = "
edge(1,2).
edge(2,3).
path(X,Y) :- edge(X,Y).
path(X,Z) :- edge(X,Y), edge(Y,Z).
"

let parse_query q_str =
  try
    let lexbuf = Lexing.from_string q_str in
    let ast = DParser.parse_query DLexer.token lexbuf in
    DLogic.query_of_ast ast
  with Parsing.Parse_error ->
    failwith ("could not parse query string " ^ q_str)

    (* 
    
    datalog_cli
    https://github.com/c-cube/datalog/blob/master/src/bottom_up_cli/datalog_cli.ml
    *)
let clauses = parse_string ex
let db = DLogic.db_create ()
let () = List.iter ~f:(fun c -> DLogic.db_add db c) clauses
let q = let vars,pos,_negs = parse_query "(X,Y) :- path(X,Y)" in
        DLogic.Query.ask db vars pos
 let l = DLogic.Query.to_list q
let () = DLogic.Query.iter q (fun fact -> Format.printf "%a\n" (Format.pp_print_list DLogic.pp_term) (Array.to_list fact))

(*

I would probably do a packed column
type packed_col = {witness : 'a Key.t; data : 'a list}
We only have to pay packing cost once then. Isn't that nice?
type table = packed_column list
type db = table String.Map.t


But actually we might like a chained thing. yeesh.



*)


(* 
module D = Datalog.BottomUp.S
module D = Datalog.Default

module Symbol = D.StringSymbol

let foo = Symbol.make "foo"
D.mk_var 0
D.mk_const foo

mk_clause head [body]

"
bar() :- 

yada yada

"

D.db_create ()
let () = List.map ~f:D.clause_of_ast ( Parser.parse_file datalog_prog) |> List.iter ~f:(D.db_add_clause db) 
D.clause_of_ast 
Parser.


module CI = Datalog_caml_interface
let edge = CI.Rel2.create ~k1:CI.Univ.int ~k2:CI.Univ.int "edge";;
let db = CI.Logic.DB.create();;
CI.Rel2.add_list db edge [1,2; 2,3; 3,4];;

*)

(* type method_ = { name : string, args : list string ; returns : string list ;   }
*)
(* https://github.com/ranjitjhala/sprite-lang/blob/master/src/Language/Sprite/L1/Types.hs
*)
(* 
type pred = Sexp.t [@@deriving sexp]

type base_typ = Int [@@deriving sexp] (* Maybe base types should just be smtlib types. Makes sense. *)
type var = string [@@deriving sexp] 
type typ = Base of base_typ * var * pred | Pi of var * typ * typ [@@deriving sexp]
type expr = The of expr * typ | Lambda of var * expr | App of expr * var | Let of var | Const of Sexp.t | Var of var [@@deriving sexp]

type kind = Base | Star
*)


(* I don't think kind plays a role here? *)
(*

let well_formed gamma (typ : typ) = 

  (* Why do I know that only Refine types show up? *)
let rec entail gamma c = match gamma with
   | [] -> c
   | ( x , Refine (b, x', p)) :: gamma -> entail gamma (forall x b (impl p  c))
   | _ -> failwith "no entail case matches"

let subtype gamma t1 t2 =
  match t1, t2 with 
  | Pi (x1, s1, t1), Pi (x2, s2, t2) ->
  | Base(b1, v1, p1), Base(b2, v2, p2) -> if b1 = b2 then
                                          Some (entail gamma (forall v1 b (impl p1 (let_ v1 v2 p2)))) 
                                          else None
  | _, _ -> None


let rec synth gamma e = match e with
        | Var x -> lookup x gamma
        | Const c -> (prim c) ????? (* Is this a huge problem? I could require all constants be checked? *)
        | The e t -> let* () = well_formed t in 
                     let* () = check gamma e t in
                     Some t
and check gamma e t = 
*)

(* let test_parse s : unit = Format.printf "%a\n" Sexp.pp_hum  (sexp_of_stmt (stmt_of_sexp (Sexp.of_string s)))
let%expect_test _ =
  print_endline "Hello, world!";
  [%expect{|
    Hello, world!
  |}]

let%expect_test _ = test_parse "(Begin Skip Skip)";
      [%expect{|
      (Begin Skip Skip)
      |}]

      let%expect_test _ = test_parse "(If (> x 7) (Set x 3) Skip)";
        [%expect {| (If (> x 7) (Set x 3) Skip) |}]
*)
(*
Module language

Take a look at rackets module language for syntactic queues.
Common lisp modules?
Leroy paper.
*)

(*
type module_sig = {}
type module_ = Module of {name : string; args : (  ) list;  signature : module_sig ; body :  } =
*)
(*

Datalog combinators

type 

let rel1 name x = 
let rel2 name x y = 
let rel3 name x y z = 

*)

(*
ocaml combinators? For printing out cstruct for example

*)