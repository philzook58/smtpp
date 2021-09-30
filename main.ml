open Core

(* args-equal and retvals-equal *)
let def_pto_stack env =
  let pointer_sort = get_pointer_sort env in
  let x = Z3.Expr.mk_fresh_const (Env.get_context env) "x" pointer_sort in
  define_fun "pto-stack"  [sexp_of_expr x, sexp_of_sort pointer_sort] (sexp_of_expr (Env.in_stack env x))
  let mem_sort dom cod = Sexp.List [Sexp.Atom "Array"; dom ; cod ]
  let bv_sort_of_var (v : Var.t) : Sexp.t =
    let typ = Var.typ v in
    match typ with
    | Imm w -> bv_sort w
    | Type.Mem (i_size, w_size) -> mem_sort
                        (bv_sort (Size.in_bits i_size))
                        (bv_sort (Size.in_bits w_size))
    | Unk -> failwith "bv_sort_of_var: Unrecognized sort type"

    let bv_sort width = Sexp.List [
      Sexp.Atom "_";
      Sexp.Atom "BitVec";
      Sexp.Atom (Int.to_string width);
      ]
    
let bv_sort (width : int) : Sexp.t =  Sexp.List
  [
    Sexp.Atom "_";
    Sexp.Atom "BitVec";
    Sexp.Atom (Int.to_string width);
  ]

type macro = Sexp.t list -> (Sexp.t, string) Result.t

(*
Post order expansion or preorder expansion?
Lazy vs eager?
Hmm. I guess eager really needs to call back into the expander system
*)
(*
let macro_run (macros : macro String.Map.t) (sexp : Sexp.t) : (Sexp.t,string) Result.t =
  let rec worker sexp =
    match sexp with
    | Sexp.Atom _ -> sexp
    | Sexp.List t -> (
        let sexp = List.map ~f:worker t in
        match sexp with
        | Sexp.Atom f :: args -> (
            match String.Map.find macros f with
            | None -> Sexp.List sexp
            | Some f ->
                f args
                (* Hmm recurse here? We don't persay have to  - worker (f args) *)
            )
        | _ -> Sexp.List sexp )
  in
  worker sexp
*)
let macro_run (macros : macro String.Map.t) (sexp : Sexp.t) :
    (Sexp.t, string) Result.t =
  let ( let* ) x f = Result.bind x ~f in
  let rec worker (sexp : Sexp.t) : (Sexp.t, string) result =
    match sexp with
    | Sexp.Atom _ -> Ok sexp
    | Sexp.List [] -> Ok sexp
    | Sexp.List (f_sexp :: args) -> (
        let* f_sexp = worker f_sexp in
        match f_sexp with
        | Sexp.Atom f_str -> (
            match String.Map.find macros f_str with
            | Some f_fun ->
                let* sexp' = f_fun args in
                worker sexp'
            | None ->
                let* args = List.map ~f:worker args |> Result.all in
                Ok (Sexp.List (f_sexp :: args)) )
        | _ ->
            let* args = List.map ~f:worker args |> Result.all in
            Ok (Sexp.List (f_sexp :: args)) )
  in
  worker sexp

let binop (op_str : string) : (string, string) Result.t =
  match op_str with
  | "+" -> Ok "bvadd"
  | "-" -> Ok "bvsub"
  | "*" -> Ok "bvmul"
  | "<<" -> Ok "bvshl"
  | ">>" -> Ok "bvshr"
  | "&" -> Ok "bvand"
  | "|" -> Ok "bvor"
  | "%" -> Ok "bv"
  | "u<" -> Ok "bvult"
  | "<" ->
      Error
        "Please specify the signedness of the '<' operator as either 'u<' or \
         's<'"
  | _ -> Error (sprintf "Unrecognized operation %s" op_str)

(* macro0 might as well go in the prelude file as defines. Uh.. is that true?
 Maybe there asre some things that can be done
*)
let macro0 name (sexp_str : string) : string * macro =
  ( name,
    fun args ->
      match args with
      | [] -> Ok (Sexp.of_string sexp_str)
      | _ -> Error (sprintf "Macro %s expected no arguments" name) )

(* This might as well not be a macro. *)


let bounded_forall_macro var start end' body =
  let let_form i =
    Sexp.List
      [ Sexp.Atom "let"; Sexp.List [ var; Sexp.Atom (Int.to_string i) ]; body ]
  in
  let l = List.map ~f:let_form (List.range start end') in
  Sexp.List (Sexp.Atom "and" :: l)

type field_info = { id : string; offset : int; size : int }

type struct_info = { id : string; fields : field_info list }


let binop op a b = Sexp.List [ Sexp.Atom op; a; b ]

let bvadd a b = binop "bvadd" a b 

let select mem k = binop "select" mem k
let store mem k v = Sexp.List [ Sexp.Atom "store"; mem; k ; v]

let concat (xs : Sexp.t list) : Sexp.t = Sexp.List (Sexp.Atom "concat" :: xs)

let bv width val' =
  Sexp.List
    [
      Sexp.Atom "_";
      Sexp.Atom (sprintf "bv%n" val');
      Sexp.Atom (Int.to_string width);
    ]
let define_fun name args ret body = 
  Sexp.List [Sexp.Atom name; Sexp.List args; ret; body] 
  
let bv_sort width =  Sexp.List
[
  Sexp.Atom "_";
  Sexp.Atom "BitVec";
  Sexp.Atom (Int.to_string width);
]
(* TODO: concat full size. init mod, orig *)
let struct_accessors (si : struct_info) : Sexp.t list =
  List.map si.fields ~f:(fun fi ->
    let name = sprintf "%s-%s" si.id fi.id in
    let args = [Sexp.of_string "(p pointer)"] in
    let body = select (Sexp.Atom "mem") (bvadd (Sexp.Atom "p") (bv fi.offset 64)) in
    define_fun name args (bv_sort 8) body
    )

let struct_setters (si : struct_info) : Sexp.t list =
  List.map si.fields ~f:(fun fi ->
    let name = sprintf "%s-%s!" si.id fi.id in
    let args = [Sexp.of_string "(p pointer)"; Sexp.of_string "(v bv8)"; Sexp.of_string "(mem memsort)"] in
    let body = store (Sexp.Atom "mem") (bvadd (Sexp.Atom "p") (bv fi.offset 64)) in
    define_fun name args (bv_sort 8) body
    )


(*
An alternative
(color-red-offset p)
(color-red p) = (p + offset) == &(p->color_red) in C syntax.
Is it possible for a sturct to be in registers?
(store mem (color-red p) 0x8)
Then this works with mem vals
*)
(*
(mem_orig_vals  
  (p1 v1)
  (p2 v2)
  (p3 v3)
  ()
) ---> (and (= (select mem p1) v1 ) (select) (  select ))
(store* mem () () () () ()) -> store (store (store (store )))
(concat-select mem p1 p2 p3 p4) --> (concat (select mem p1) (select mem p2) () )

vararg functions basically need to be macros.
Well. I could enumerate up to some number
concat-select3


let multi_store_mem_macro ptr_vals = 
  ptrvals = List.map ~f(fun sexp -> match sexp with
                          | Sexp.List [ptr;va] -> Ok (ptr,val)
                          | _ -> Error "In struct macro key value pair mismatch" ) |> Result.all
  List.fold ~init:(Sexp.Atom "mem") ptrvals ~f:(fun acc (p,v) ->
      store acc p v
  )

*)

(* 
let struct_macro (si : struct_info) : (string * macro) list =
  List.map si.fields ~f:(fun fi ->
    let macro_name = sprintf "%s-%s" si.id fi.id in
    let macro args = 
      match args with
      | [var; value] -> Sexp.List [Sexp.Atom "bvadd"   Sexp.Atom fi.offset  ]
      | _ -> Err (sprintf "Field accessor macro %s expected two values. Received (bv %a)" macro_name args)
    in
    macro_name, macro
  )
*)
let bv_mode = ()

(*
   Can I locally overload all operators using a let? let ((< bvlute))
   alternatively:
   run_macro [bv_macros]

   This is seperate in principle from infix mode.
*)

let c_expr = ()

(*


 accepts a string (since c won't parse as a sexp)
 run frontc on it.
 parse_expr
 (cexpr    )
 (cblock)
 (cstmt )
 (cmode
 '
   x = 7;
   a[2] = 3;
   a[4] == a[5]
  ') --->
    (proc
    (x := 7)
    (a(3) := 3)
    ((select mem (bvadd a 4)) )
    )

    We'll need variable to type mappings.
    Are we duplicating effort here? Hmmmmm.


*)

let macros = String.Map.of_alist_exn [ (*  mem_equiv_start; *) ]

let run filename =
  let prelude_chan = In_channel.create "test.smt2" in
  let prelude_sexp = Sexp.input_sexps prelude_chan in
  let file = In_channel.create filename in
  let sexp = Sexp.input_sexp file in
  match macro_run macros sexp with
  | Error s -> Format.printf "Error: %s" s
  | Ok sexp ->
      Format.printf "%a\n%a"
        (Format.pp_print_list Sexp.pp_hum)
        prelude_sexp Sexp.pp_hum sexp

let filename_param =
  let open Command.Param in
  anon ("filename" %: string)

let command =
  Command.basic ~summary:"Apply Macros to an SMTLIB file"
    ~readme:(fun () -> "More detailed information")
    (Command.Param.map filename_param ~f:(fun filename () -> run filename))

let () = Command.run ~version:"1.0" ~build_info:"RWO" command

(* 





let infix_macro (args : Sexp.t list) : (Sexp.t, string) result =

let bv_const (args : Sexp.t list) : (Sexp.t, string) result =
  match args with
  | [Sexp.Atom bitwidth; Sexp,Atom value] -> 
        let* bw =  to_int bitwidth in
        let* value = to_int64 value in
        sprintf "#x%0" yada yada
  | _ -> Err (sprintf "Bitvector macro expected two constant values. Received (bv %a)" args)

let array

(* a proc or begin macro? vs the more specialized array macro

*)
let array_macro (args : Sexp.t list) : (Sexp.t, string) result =
  



(*
lisp style struct access
(setf (ship-x-position ship2) 100) 
(type-field foo) ==> ()



*)

let stdlib_macros =[
  "@array", begin_macro;
  "@bv", bv_const;


]
*)
