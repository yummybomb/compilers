type heap = (string * Syntax.value) list [@@deriving show]
type vardecl = { loc : Syntax.location option; ty: Syntax.ty }
let pp_vardecl = fun fmt vd -> Syntax.pp_ty fmt vd.ty
type heap_ty = (string * vardecl) list [@@deriving show]

exception EvaluationError of Syntax.location option * string
exception AssertionError of Syntax.location option

(** A module for parsing arguments to our IMP verification tool. *)
module Args : sig
  (** Whether to verify the program or run it.*)
  type mode = Verify | Interpret [@@deriving show]

  (** Options to the HW3 executable. *)
  type t =
    { mode : mode  (** whether to verify the program or run it *)
    ; filename : string option  (** where to get the program: from a filename (Some filename) or from stdin (None) *)
    ; heap : heap  (** if running the program, the initial heap *)
    } [@@deriving show]

  (** Parse the command-line arguments and return the resulting record t. *)
  val parse_args : unit -> t
end = struct
  (* You can ignore everything until the next 'end' keyword if you want. *)

  type mode = Verify | Interpret [@@deriving show]
  type t_internal =
    { mutable mode : mode
    ; mutable filename : string option
    ; mutable heap : heap
    }

  type t =
    { mode : mode
    ; filename : string option
    ; heap : heap
    } [@@deriving show]

  let parse_args () =
    let args : t_internal =
      { mode = Verify
      ; filename = None
      ; heap = []
      }
    in
    let option_or o1 o2 =
      match o1 with
      | None -> o2
      | Some _ -> o1
    in
    let int_array_of_string_res (s : string) : (int list, string) result =
      let n = String.length s in
      if n < 2 then Error "string too short"
      else if String.get s 0 <> '[' || String.get s (n - 1) <> ']' then Error (Printf.sprintf "string %s does not start and end with square brackets" s)
      else if n = 2 then Ok []
      else
        String.sub s 1 (n - 2)
        |> String.split_on_char ';'
        |> List.fold_left (fun acc x ->
               match int_of_string_opt x, acc with
               | Some y, Some acc -> Some (y :: acc)
               | _, _ -> None)
             (Some [])
        |> Option.map List.rev
        |> Option.fold ~none:(Error "some number failed to parse") ~some:(fun l -> Ok l)
    in
    let parse_value_opt (s : string) : Syntax.value option =
      let array = int_array_of_string_res s in
      match
      List.fold_left option_or None
        [ bool_of_string_opt s |> Option.map (fun b -> Syntax.VBool b)
        ; int_of_string_opt s |> Option.map (fun n -> Syntax.VInt n)
        (* TODO: if you choose to do the array extra credit, uncomment this line after adding the corresponding line to syntax.ml *)
        ; (* (match array with Error _ -> None | Ok v -> Some (Syntax.VArray (List.map (fun n -> Syntax.VInt n) v))) *)
        ],
      array
      with
      | None, Error msg -> failwith msg
      | x, _ -> x
    in
    let set_heap s =
      let assignments = String.split_on_char ',' s |> List.map (String.split_on_char '=') in
      let h = List.map (fun l ->
          if List.length l <> 2 then raise (Arg.Bad (Printf.sprintf "Could not make sense of heap assignment '%s'" (String.concat "=" l)));
          let key = List.hd l in
          let str_value = List.nth l 1  in
          match parse_value_opt str_value with
          | None -> raise (Arg.Bad (Printf.sprintf "Could not parse value '%s' for heap variable '%s'" str_value key))
          | Some v -> (key, v)) assignments
      in args.heap <- h
    in
    let set_verify () = args.mode <- Verify in
    let set_interpret () = args.mode <- Interpret in
    let speclist = [("-heap", Arg.String set_heap, ""); ("-verify", Arg.Unit set_verify, ""); ("-interpret", Arg.Unit set_interpret, "")]
    and set_filename s = args.filename <- Some s
    and usage_str = ""
    in
    Arg.parse speclist set_filename usage_str;
    { mode = args.mode
    ; filename = args.filename
    ; heap = args.heap
    }
end

let with_loc (loc: Syntax.location option) (x: 'a) : 'a Syntax.located =
  { loc = loc; Syntax.value = x}

let with_no_loc (x: 'a) : 'a Syntax.located =
  { loc = None; Syntax.value = x}

let rec nth_set_opt l i v =
  match l, i with
  | [], _ -> None
  | _ :: xs, 0 -> Some (v :: xs)
  | x :: xs, i ->
     match nth_set_opt xs (i - 1) v with
     | None -> None
     | Some l' -> Some (x :: l')

let rec eval_expr (h : heap) (e : Syntax.expr) : Syntax.value =
  match e.value with
  | Literal v -> v
  | Var x -> begin
      match List.assoc_opt x h with
      | None -> raise (EvaluationError (e.loc, Printf.sprintf "Unknown variable %s" x))
      | Some v -> v
    end
  | Unop (Not, e) -> Syntax.VBool (not (eval_expr_bool h e))
  | Unop (Neg, e) -> Syntax.VInt (- (eval_expr_int h e))
  | Binop (e1, Add, e2) -> Syntax.VInt (eval_expr_int h e1 + eval_expr_int h e2)
  | Binop (e1, Sub, e2) -> Syntax.VInt (eval_expr_int h e1 - eval_expr_int h e2)
  | Binop (e1, Mul, e2) -> Syntax.VInt (eval_expr_int h e1 * eval_expr_int h e2)
  | Binop (e1, And, e2) -> Syntax.VBool (eval_expr_bool h e1 && eval_expr_bool h e2)
  | Binop (e1, Or, e2) -> Syntax.VBool (eval_expr_bool h e1 || eval_expr_bool h e2)
  | Binop (e1, Implies, e2) -> Syntax.VBool (not (eval_expr_bool h e1) || eval_expr_bool h e2)
  | Binop (e1, Eq, e2) -> Syntax.VBool (eval_expr h e1 = eval_expr h e2)
  | Binop (e1, Neq, e2) -> Syntax.VBool (eval_expr h e1 != eval_expr h e2)
  | Binop (e1, Lt, e2) -> Syntax.VBool (eval_expr_int h e1 < eval_expr_int h e2)
  | Binop (e1, Le, e2) -> Syntax.VBool (eval_expr_int h e1 <= eval_expr_int h e2)
  | Binop (e1, Gt, e2) -> Syntax.VBool (eval_expr_int h e1 > eval_expr_int h e2)
  | Binop (e1, Ge, e2) -> Syntax.VBool (eval_expr_int h e1 >= eval_expr_int h e2)
and construct_err_msg (v : Syntax.value) (ty_str : string) : string =
  Printf.sprintf
    "evaluated to value %s, but %s was expected"
    (Syntax.show_value v)
    ty_str
and eval_expr_int (h : heap) (e : Syntax.expr) : int =
  match eval_expr h e with
  | VInt n -> n
  | v -> raise (EvaluationError (e.loc, construct_err_msg v "an int"))
and eval_expr_bool (h : heap) (e : Syntax.expr) : bool =
  match eval_expr h e with
  | VBool b -> b
  | v -> raise (EvaluationError (e.loc, construct_err_msg v "a bool"))

let rec eval_stmt (h : heap) (s : Syntax.stmt) : heap =
  match s.value with
  | Skip -> h
  | Assign (x, e) -> (x, eval_expr h e) :: List.remove_assoc x h
  | Assert e ->
     if not (eval_expr_bool h e)
     then raise (AssertionError s.loc);
     h
  | Assume e ->
     if not (eval_expr_bool h e)
     then Printf.printf "%s: warning: this assumption was violated at run time\n%!" (Syntax.string_of_loc e.loc);
     h
  | Seq (s1, s2) -> denote_seq h s1 s2
  | If (e, s1, s2) ->
     if eval_expr_bool h e
     then eval_stmt h s1
     else eval_stmt h s2
  | While (e, invs, body) ->
     List.iter (fun inv -> if not (eval_expr_bool h inv)
                           then Printf.printf "%s: this loop invariant was violated at run time\n%!" (Syntax.string_of_loc inv.loc))
       invs;
     if eval_expr_bool h e
     then denote_seq h body s
     else h
and denote_seq h s1 s2 =
  eval_stmt (eval_stmt h s1) s2

exception TypeError of Syntax.location option * string

let rec type_infer_expr (sigma : heap_ty) (e : Syntax.expr) : Syntax.ty =
  match e.value with
  | Literal (VInt _) -> TInt
  | Literal (VBool _) -> TBool
  (*  (* TODO: Uncomment these lines if you do the array extra credit. *)
  | Literal (VArray _) ->
     raise (TypeError (e.loc, "Array literals are not supported by the typechecker"))
   *)
  | Var x -> check_var e.loc sigma x
  | Unop (Not, e) -> type_check_expr sigma e Syntax.TBool; Syntax.TBool
  | Unop (Neg, e) -> type_check_expr sigma e Syntax.TInt; Syntax.TInt
  | Binop (e1, (Add | Sub | Mul), e2) ->
     type_check_expr sigma e1 Syntax.TInt;
     type_check_expr sigma e2 Syntax.TInt;
     TInt
  | Binop (e1, (And | Or | Implies), e2) ->
     type_check_expr sigma e1 Syntax.TBool;
     type_check_expr sigma e2 Syntax.TBool;
     TBool
  | Binop (e1, (Eq | Neq), e2) ->
     let t1 = type_infer_expr sigma e1 in
     type_check_expr sigma e2 t1;
     TBool
  | Binop (e1, (Lt | Le | Gt | Ge), e2) ->
     type_check_expr sigma e1 Syntax.TInt;
     type_check_expr sigma e2 Syntax.TInt;
     TBool
and check_var loc sigma x =
  match List.assoc_opt x sigma with
  | None -> raise (TypeError (loc, Printf.sprintf "Unknown variable %s" x))
  | Some v -> v.ty
and type_check_expr (sigma : heap_ty) (e : Syntax.expr) (ty_expect : Syntax.ty) : unit =
  let ty_actual = type_infer_expr sigma e in
  if ty_actual <> ty_expect
  then raise (TypeError
         (e.loc, Printf.sprintf
            "expected type %s but got type %s"
            (Syntax.show_ty ty_expect)
            (Syntax.show_ty ty_actual)))

let rec type_check_stmt (sigma : heap_ty) (s : Syntax.stmt) : heap_ty =
  match s.value with
  | Skip -> sigma
  | Assign (x, e) -> begin
      match List.assoc_opt x sigma with
      | None -> (x, { loc = s.loc; ty = type_infer_expr sigma e}) :: sigma
      | Some d -> type_check_expr sigma e d.ty; sigma
    end
  | Assert e | Assume e -> type_check_expr sigma e TBool; sigma
  | Seq (s1, s2) ->
     type_check_stmt (type_check_stmt sigma s1) s2
  | If (e, s1, s2) ->
     type_check_expr sigma e TBool;
     type_check_stmt (type_check_stmt sigma s1) s2
  | While (e, invs, body) ->
     type_check_expr sigma e TBool;
     List.iter (fun e -> type_check_expr sigma e TBool) invs;
     type_check_stmt sigma body

module StringSet = Set.Make(String)

let get_lexbuf name chan =
  let lexbuf = Lexing.from_channel chan in
  let () = lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with Lexing.pos_fname = name } in
  lexbuf

let initial_heap_to_heap_typing h =
  List.map
    (fun (k,v) ->
      k, { loc = None;
           ty = match v with
                | Syntax.VBool _ -> Syntax.TBool
                | Syntax.VInt _ -> Syntax.TInt
                (* TODO: Uncomment this line if you do the array extra credit. *)
               (* | Syntax.VArray _ -> Syntax.TArray TInt *)
    })
    h

let () =
  let args = Args.parse_args () in
  let in_chan =
    match args.filename with
    | None -> stdin
    | Some fname -> open_in fname
  in
  let lexbuf = get_lexbuf (match args.filename with Some x -> x | None -> "<stdin>") in_chan in
  let stmt = try
      Parser.main Lexer.token lexbuf
    with
    | Lexer.Error (pos, msg) -> Printf.printf "%s: lexical error: %s\n%!" (Syntax.string_of_lex_pos pos) msg; exit 1
    | Parser.Error -> Printf.printf "%s: parse error while looking at %s\n%!" (Syntax.string_of_lex_pos (Lexing.lexeme_start_p lexbuf)) (Lexing.lexeme lexbuf); exit 1
  in
  (* print_endline (Syntax.show_stmt stmt); *)
  let h = args.heap in
  let sigma = initial_heap_to_heap_typing h in
  let sigma2 =
    try
      type_check_stmt sigma stmt
    with
      TypeError (loc, msg) -> Printf.printf "%s: type error: %s\n%!" (Syntax.string_of_loc loc) msg; exit 1
  in
  try
    match args.mode with
    | Verify -> begin
        (* TODO: delete the following lines and write you implementation here *)
        ignore(sigma2);
        failwith "verification not yet implemented"
      end
    | Interpret ->
       print_endline (show_heap h);
       let h2 = eval_stmt h stmt in
       print_endline (show_heap h2)
  with
  | EvaluationError (loc, msg) as e -> Printf.printf "%s: evaluation error: %s\n%!" (Syntax.string_of_loc loc) msg; raise e
  | AssertionError loc as e -> Printf.printf "%s: assertion error\n%!" (Syntax.string_of_loc loc); raise e
