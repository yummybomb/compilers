(* puzzle/puzzle.ml *)
type t = {
  k: int;
  board: int array array
}

exception ParseError of string

let from_channel s =
  let get_int () =
    try
      Scanf.bscanf s " %d" (fun n -> n)
    with
      Scanf.Scan_failure msg ->
      raise (ParseError (Printf.sprintf "failed to read integer: %s" msg))
    | End_of_file ->
      raise (ParseError "Unexpected end of file")
  in
  let k = get_int () in
  if k <= 0 then raise (ParseError (Printf.sprintf "k must be > 0, but got %d" k));
  if k > 100 then raise (ParseError (Printf.sprintf "k must be <= 100, got %d" k));
  
  let n = k * k in
  let board = Array.make_matrix n n 0 in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      let value = get_int () in
      if value < 0 || value > n then
        raise (ParseError (Printf.sprintf "value must be between 0 and %d, got %d" n value));
      board.(i).(j) <- value
    done
  done;
  { k; board }

let show p =
  let n = p.k * p.k in
  let buf = Buffer.create 256 in
  Buffer.add_string buf (string_of_int p.k);
  Buffer.add_char buf '\n';
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      if j > 0 then Buffer.add_char buf ' ';
      Buffer.add_string buf (string_of_int p.board.(i).(j))
    done;
    Buffer.add_char buf '\n'
  done;
  Buffer.contents buf