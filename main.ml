open Core

module SexpA = Sexp.Annotated

type expr = Sexp.t [@@deriving sexp]

type stmt =
  | Skip
  | Assume of expr
  | Assert of expr
  | If of expr * stmt * stmt
  | Begin of stmt list [@sexp.list]
  | Set of Sexp.t * expr
  [@@deriving sexp]


module SmtLib = struct
  let fun1 name = fun x -> Sexp.List [Sexp.Atom name; x]
  let fun2 name = fun x y -> Sexp.List [Sexp.Atom name; x; y]
  let fun3 name = fun x y z -> Sexp.List [Sexp.Atom name; x; y; z]

  let not_ = fun1 "not"
  let and_ = fun2 "and"
  let impl = fun2 "=>"
  let ite = fun3 "ite"
  let false_ = Sexp.Atom "false"
  let true_ = Sexp.Atom "true"
  let assert_ = fun1 "assert"
  let forall = fun2 "forall"
  
  let command c = Sexp.List [Sexp.Atom c]
  let push = command "push"
  let pop = command "pop"
  let check_sat = command "check-sat"
  let echo = fun1 "echo"

  let let_ v e body = 
    Sexp.List [ Sexp.Atom "let"; Sexp.List [Sexp.List [ v; e ]] ; body ]
end

open SmtLib

module Method = struct

  type method_props = Declare of {pre : Sexp.t option [@sexp.option]; post : Sexp.t option [@sexp.option] } [@@deriving sexp]
  (* type args = (string * Sexp.t) list *)
  type t = DefMethod of string * Sexp.t * method_props * stmt list [@sexp.list] [@@deriving sexp]
  let get_name meth =  match meth with
                      | DefMethod(name, _, _ , _) -> name

  (* Method registry is populated for the use of calling methods frtom other methods. *)
  let registry : (string, t) Hashtbl.t = Hashtbl.create (module String)
  let register meth : unit = Hashtbl.set ~key:(get_name meth) ~data:meth registry


end



module GenSym = struct

(** [symbol_set] returns a set of every string in the input sexp. This is useful to implement a gensym that is guaranteed the have fresh
    symbols *)
let symbol_set (s : Sexp.t) : String.Set.t =
  let rec worker symset sexp =
    match sexp with
    | Sexp.List sexps -> List.fold sexps ~init:symset ~f:(fun acc sexp -> worker acc sexp)
    | Sexp.Atom a -> String.Set.add symset a
  in
  worker String.Set.empty s

let symbols : string Hash_set.t = Hash_set.create (module String)

let rec add_sexp s =
    match s with
      | Sexp.List sexps -> List.iter sexps ~f:add_sexp
      | Sexp.Atom a -> Hash_set.add symbols a

let rec gensym name : string = 
  if Hash_set.mem symbols name then
    gensym (name ^ "#")
  else
    begin
      Hash_set.add symbols name;
      name
    end

(* Freshening can be done by felayed substitution via a let. *)
let freshen (x : string) (s : Sexp.t) =
  let x' = gensym x in
  let_ (Sexp.Atom x') (Sexp.Atom x) s

end

(*
Could have stmt ast and elaborated stmt ast?
*)

let rec wp_stmt (s : stmt) : Sexp.t -> Sexp.t =
  fun post ->
  match s with
  | Skip -> post
  (* | Var(v,typ) -> forall (Sexp.List vars) post *)
  | Set(v, e) -> let_ v e post
  | Assert(e) -> and_ e post
  | Assume(e) -> impl e post
  | If(c,t,e) -> ite c (wp_stmt t post) (wp_stmt e post)
  | Begin stmts -> wp_stmts stmts post
and wp_stmts stmts = fun post -> List.fold_right stmts ~init:post ~f:(fun s post -> wp_stmt s post)


let define_method (meth : Method.t) : Sexp.t list =
  match meth with
  | DefMethod(name, vars, props, body) ->
    (* let () = register_method name ~args ~pre ~post in *)
    let post' = wp_stmts body (Option.value ~default:true_ props.post) in
    let pre = Option.value ~default:true_ props.pre in
    let vc = (forall vars (impl pre post')) in
    [
      echo (Sexp.Atom (sprintf "verifying %s" name));
      push;
      assert_ vc;
      check_sat;
      pop;
    ]

let process_method sexp = 
  let sexp = SexpA.get_sexp sexp in
  match sexp with
  | Sexp.List (Sexp.Atom "DefMethod" :: _) -> 
        let meth = Method.t_of_sexp sexp in
        define_method meth
  | _ -> [sexp]

let run filename =
  let file = In_channel.create filename in
  let sexps = Sexp.Annotated.input_sexps file in
  List.iter sexps ~f:(fun s -> SexpA.get_sexp s |> GenSym.add_sexp);
  let res = List.concat_map ~f:process_method sexps in
  Format.printf "%a\n" (Format.pp_print_list ~pp_sep:Format.pp_print_newline Sexp.pp_hum) res

let filename_param =
  let open Command.Param in
  anon ("filename" %: string)

let command =
  Command.basic ~summary:"Apply Macros to an SMTLIB file"
    ~readme:(fun () -> "More detailed information")
    (Command.Param.map filename_param ~f:(fun filename () -> run filename))

let () = Command.run ~version:"1.0" ~build_info:"RWO" command

