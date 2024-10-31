(* sudoku.ml *)
let generate_constraints z3 (puzzle : Puzzle.t) =
  let n = puzzle.Puzzle.k * puzzle.Puzzle.k in
  let board = puzzle.Puzzle.board in
  
  (* Declare variables *)
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      Z3.raw_send z3 (Printf.sprintf "(declare-const x_%d_%d Int)" i j)
    done
  done;

  (* Each cell must be between 1 and n *)
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      Z3.raw_send z3 (Printf.sprintf "(assert (and (>= x_%d_%d 1) (<= x_%d_%d %d)))" i j i j n)
    done
  done;

  (* Initial board constraints *)
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      if board.(i).(j) > 0 then
        Z3.raw_send z3 (Printf.sprintf "(assert (= x_%d_%d %d))" i j board.(i).(j))
    done
  done;

  (* Row constraints *)
  for i = 0 to n - 1 do
    let vars = List.init n (fun j -> Printf.sprintf "x_%d_%d" i j) in
    Z3.raw_send z3 (Printf.sprintf "(assert (distinct %s))" (String.concat " " vars))
  done;

  (* Column constraints *)
  for j = 0 to n - 1 do
    let vars = List.init n (fun i -> Printf.sprintf "x_%d_%d" i j) in
    Z3.raw_send z3 (Printf.sprintf "(assert (distinct %s))" (String.concat " " vars))
  done;

  (* Box constraints *)
  let k = puzzle.Puzzle.k in
  for box_i = 0 to k - 1 do
    for box_j = 0 to k - 1 do
      let vars = ref [] in
      for i = 0 to k - 1 do
        for j = 0 to k - 1 do
          vars := Printf.sprintf "x_%d_%d" (box_i * k + i) (box_j * k + j) :: !vars
        done
      done;
      Z3.raw_send z3 (Printf.sprintf "(assert (distinct %s))" (String.concat " " !vars))
    done
  done

let get_solution z3 n =
  let board = Array.make_matrix n n 0 in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      Z3.raw_send z3 (Printf.sprintf "(eval x_%d_%d)" i j);
      let value = int_of_string (Z3.raw_read_line z3) in
      board.(i).(j) <- value
    done
  done;
  board

let add_blocking_constraint z3 board n =
  let clause = ref [] in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      clause := Printf.sprintf "(not (= x_%d_%d %d))" i j board.(i).(j) :: !clause
    done
  done;
  Z3.raw_send z3 (Printf.sprintf "(assert (or %s))" (String.concat " " !clause))

let () =
  let in_chan = Scanf.Scanning.stdin in
  let puzzle = try
      Puzzle.from_channel in_chan
    with Puzzle.ParseError msg -> 
      print_endline (Printf.sprintf "parse error: %s" msg);
      exit 1
  in
  
  let z3 = Z3.init () in
  
  (* Generate initial constraints *)
  generate_constraints z3 puzzle;
  
  (* Check if there's a solution *)
  Z3.raw_send z3 "(check-sat)";
  let result = match Z3.raw_read_line z3 with
  | "unsat" -> 
      print_endline ("unsat");
      `Unsat
  | "sat" ->
      (* Get the first solution *)
      let n = puzzle.Puzzle.k * puzzle.Puzzle.k in
      let solution1 = get_solution z3 n in
      let solution1_puzzle = { Puzzle.k = puzzle.Puzzle.k; Puzzle.board = solution1 } in
      print_string (Puzzle.show solution1_puzzle);
      
      (* Add blocking constraint and check for another solution *)
      add_blocking_constraint z3 solution1 n;
      Z3.raw_send z3 "(check-sat)";
      begin match Z3.raw_read_line z3 with
      | "unsat" -> 
          print_endline "puzzle solution is unique";
          `Sat
      | "sat" ->
          print_endline "found more than one solution!";
          let solution2 = get_solution z3 n in
          let solution2_puzzle = { Puzzle.k = puzzle.Puzzle.k; Puzzle.board = solution2 } in
          print_string (Puzzle.show solution2_puzzle);
          `Sat
      | _ -> `Error "unexpected Z3 response"
      end
  | _ -> `Error "unexpected Z3 response"
  in
  Z3.close z3;
  match result with
  | `Error msg -> failwith msg
  | _ -> ()