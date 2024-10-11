open MicroCamlTypes

(*******************************************************************|
|**********************   Environment   ****************************|
|*******************************************************************|
| - The environment is a map that holds type information of         |
|   variables                                                       |
|*******************************************************************)
type environment = (var * typeScheme) list

exception OccursCheckException

exception UndefinedVar

exception TypeError

type substitutions = (string * typeScheme) list

let type_variable = ref (Char.code 'a')

(* generates a new unknown type placeholder.
   returns T(string) of the generated alphabet *)
let gen_new_type () =
  let c1 = !type_variable in
  incr type_variable;
  T(Char.escaped (Char.chr c1))
;;

let rec ftv_type (t: typeScheme): string list =
  match t with
  | TNum | TBool | TStr -> []
  | T(x) -> [x]
  | TFun(t1, t2) -> List.append (ftv_type t1) (ftv_type t2)
  | TPoly(vars, t_inner) ->
      let ftv_inner = ftv_type t_inner in
      List.filter (fun x -> not (List.mem x vars)) ftv_inner
;;

let ftv_env (env: environment): string list =
  List.fold_left (fun acc (_, t) ->
    let ftv_t = ftv_type t in
    List.append acc ftv_t
  ) [] env
;;

let generalize (env: environment) (t: typeScheme): typeScheme =
  let env_vars = ftv_env env in
  let t_vars = ftv_type t in
  let vars = List.filter (fun x -> not (List.mem x env_vars)) t_vars in
  if vars = [] then t else TPoly(vars, t)
;;


let instantiate (t: typeScheme): typeScheme =
  let rec inst subst t =
    match t with
    | TNum | TBool | TStr -> t
    | T x -> (try List.assoc x subst with Not_found -> t)
    | TFun(t1, t2) -> TFun(inst subst t1, inst subst t2)
    | TPoly(vars, t_inner) ->
        let subst = List.fold_left (fun acc var -> (var, gen_new_type ())::acc) subst vars in
        inst subst t_inner
  in
  inst [] t
;;

(*********************************************************************|
|******************   Annotate Expressions   *************************|
|*********************************************************************|
| Arguments:                                                          |
|   env -> A typing environment                                       |
|   e -> An expression that has to be annotated                       |
|*********************************************************************|
| Returns:                                                            |
|   returns an annotated expression of type aexpr that holds          |
|   type information for the given expression e.                      |
|   and the type of e                                                 |
|   and a list of typing constraints.                                 |
|*********************************************************************|
| - This method takes every expression/sub-expression in the          |
|   program and assigns some type information to it.                  |
| - This type information maybe something concrete like a TNum        |
|   or it could be a unique parameterized type(placeholder) such      |
|   as 'a.                                                            |
| - Concrete types are usually assigned when you encounter            |
|   simple literals like 10, true and "hello"                         |
| - Whereas, a random type placeholder is assigned when no            |
|   explicit information is available.                                |
| - The algorithm not only infers types of variables and              |
|   functions defined by user but also of every expression and        |
|   sub-expression since most of the inference happens from           |
|   analyzing these expressions only.                                 |
| - A constraint is a tuple of two typeSchemes. A strict equality     |
|   is being imposed on the two types.                                |
| - Constraints are generated from the expresssion being analyzed,    |
|   for e.g. for the expression ABinop(x, Add, y, t) we can constrain |
|   the types of x, y, and t to be TNum.                              |
| - In short, most of the type checking rules will be added here in   |
|   the form of constraints.                                          |
| - Further, if an expression contains sub-expressions, then          |
|   constraints need to be obtained recursively from the              |
|   subexpressions as well.                                           |
| - Lastly, constraints obtained from sub-expressions should be to    |
|   the left of the constraints obtained from the current expression  |
|   since constraints obtained from current expression holds more     |
|   information than constraints from subexpressions and also later   |
|   on we will be working with these constraints from right to left.  |
|*********************************************************************)
let rec gen (env: environment) (e: expr): aexpr * typeScheme * (typeScheme * typeScheme) list =
  match e with
  | Int n -> AInt(n, TNum), TNum, []
  | Bool b -> ABool(b, TBool), TBool, []
  | String s -> AString(s, TStr), TStr, []
  | ID x ->
    if List.mem_assoc x env
    then AID(x, List.assoc x env), List.assoc x env, []
    else raise UndefinedVar
  | Fun(id, e) ->
    let tid = gen_new_type () in
    let rty = gen_new_type () in
    let env' = (id, tid)::env in
    let ae, t, q = gen env' e in
    (*let t = List.assoc id env in
    let _ = List.iter (fun k v -> print_string k; print_string " "; print_string (string_of_type v); print_string "\n") env in
    let _ = print_string id; print_string " "; print_string (string_of_type t); print_string ("\n") in*)
    let q' = [(t, rty)] in
    AFun(id, ae, TFun(tid, rty)), TFun(tid, rty), q @ q'
  | Not e ->
    let ae, t1, q = gen env e in
    ANot(ae, TBool), TBool, q @ [(t1, TBool)]
  | Binop(op, e1, e2) ->
    let et1, t1, q1 = gen env e1
    and et2, t2, q2 = gen env e2 in
    (* impose constraints based on binary operator *)
    let opc, t = match op with
      | Add | Sub | Mult | Div -> [(t1, TNum); (t2, TNum)], TNum
      | Concat -> [(t1, TStr); (t2, TStr)], TStr
      (* we return et1, et2 since these are generic operators *)
      | Greater | Less | GreaterEqual | LessEqual | Equal | NotEqual -> [(t1, t2)], TBool
      | And | Or -> [(t1, TBool); (t2, TBool)], TBool
    in
    (* opc appended at the rightmost since we apply substitutions right to left *)
    ABinop(op, et1, et2, t), t, q1 @ q2 @ opc
  | If (e1, e2, e3) ->
    let ae1, t1, q1 = gen env e1 in
    let ae2, t2, q2 = gen env e2 in
    let ae3, t3, q3 = gen env e3 in
    AIf (ae1, ae2, ae3, t2), t2, q1 @ q2 @ q3 @ [(t1, TBool); (t2, t3)]
  | FunctionCall(fn, arg) ->
    let afn, fnty, fnq = gen env fn in
    let aarg, argty, argq = gen env arg in
    let t = gen_new_type () in
    let q = fnq @ argq @ [(fnty, TFun(argty, t))] in
    AFunctionCall(afn, aarg, t), t, q
  | Let (id, b, e1, e2) ->
    let ae1, t1, q1 =
      if b then
        let tid = gen_new_type () in
        let env' = (id, tid)::env in
        gen env' e1
      else
        gen env e1 in
    let env' = (id, t1)::env in
    let ae2, t2, q2 = gen env' e2 in
    ALet (id, b, ae1, ae2, t2), t2, q1 @ q2


(******************************************************************|
|**********************   Unification   ***************************|
|**********************    Algorithm    ***************************|
|******************************************************************)


(******************************************************************|
|**********************   Substitute   ****************************|
|******************************************************************|
|Arguments:                                                        |
|   t -> type in which substitutions have to be made.              |
|   (x, u) -> (type placeholder, resolved substitution)            |
|******************************************************************|
|Returns:                                                          |
|   returns a valid substitution for t if present, else t as it is.|
|******************************************************************|
|- In this method we are given a substitution rule that asks us to |
|  replace all occurrences of type placeholder x with u, in t.     |
|- We are required to apply this substitution to t recursively, so |
|  if t is a composite type that contains multiple occurrences of  |
|  x then at every position of x, a u is to be substituted.        |
*******************************************************************)
let rec substitute (u: typeScheme) (x: string) (t: typeScheme) : typeScheme =
  match t with
  | TNum | TBool | TStr -> t
  | T(y) -> if y = x then u else t
  | TFun(t1, t2) -> TFun(substitute u x t1, substitute u x t2)
  | TPoly(vars, t_inner) ->
      if List.mem x vars then
        t  (* x is bound, no substitution inside *)
      else
        TPoly(vars, substitute u x t_inner)
;;

(******************************************************************|
|*************************    Apply    ****************************|
|******************************************************************|
| Arguments:                                                       |
|   subs -> list of substitution rules.                            |
|   t -> type in which substitutions have to be made.              |
|******************************************************************|
| Returns:                                                         |
|   returns t after all the substitutions have been made in it     |
|   given by all the substitution rules in subs.                   |
|******************************************************************|
| - Works from right to left                                       |
| - Effectively what this function does is that it uses            |
|   substitution rules generated from the unification algorithm and|
|   applies it to t. Internally it calls the substitute function   |
|   which does the actual substitution and returns the resultant   |
|   type after substitutions.                                      |
| - Substitution rules: (type placeholder, typeScheme), where we   |
|   have to replace each occurrence of the type placeholder with   |
|   the given type t.                                              |
|******************************************************************)
let apply (subs: substitutions) (t: typeScheme) : typeScheme =
  List.fold_right (fun (x, u) t -> substitute u x t) subs t
;;

let rec occurs (x: string) (t: typeScheme) : bool =
  match t with
  | TNum | TBool | TStr -> false
  | T(y) -> x = y
  | TFun(t1, t2) -> occurs x t1 || occurs x t2
  | TPoly(vars, t_inner) ->
      not (List.mem x vars) && occurs x t_inner
;;

let substitute_in_constraints x t constraints =
  List.map (fun (l, r) -> (substitute t x l, substitute t x r)) constraints
;;
  
(******************************************************************|
|***************************   Unify   ****************************|
|******************************************************************|
| Arguments:                                                       |
|   constraints -> list of constraints (tuple of 2 types)          |
|******************************************************************|
| Returns:                                                         |
|   returns a list of substitutions                                |
|******************************************************************|
| - The unify function takes a bunch of constraints it obtained    |
|   from the collect method and turns them into substitutions.     |
| - In the end we get a complete list of substitutions that helps  |
|   resolve types of all expressions in our program.               |
|******************************************************************)
let rec unify (constraints: (typeScheme * typeScheme) list) : substitutions =
  match constraints with
  | [] -> []
  | (t1, t2) :: rest ->
      let t1 = match t1 with
        | TPoly(_, _) -> instantiate t1
        | _ -> t1
      in
      let t2 = match t2 with
        | TPoly(_, _) -> instantiate t2
        | _ -> t2
      in
      if t1 = t2 then
        unify rest
      else
        match t1, t2 with
        | T(x), t | t, T(x) ->
            if occurs x t then
              raise OccursCheckException
            else
              let rest' = substitute_in_constraints x t rest in
              let subs = unify rest' in
              let t' = apply subs t in
              let subs' = (x, t') :: List.map (fun (y, u) -> (y, substitute t' x u)) subs in
              subs'
        | TFun(a1, b1), TFun(a2, b2) ->
            unify ((a1, a2) :: (b1, b2) :: rest)
        | _ -> raise TypeError
;;


(* applies a final set of substitutions on the annotated expr *)
let rec apply_expr (subs: substitutions) (ae: aexpr): aexpr =
  match ae with
  | ABool(b, t) -> ABool(b, apply subs t)
  | AInt(n, t) -> AInt(n, apply subs t)
  | AString(s, t) -> AString(s, apply subs t)
  | AID(s, t) -> AID(s, apply subs t)
  | AFun(id, e, t) -> AFun(id, apply_expr subs e, apply subs t)
  | ANot(e, t) -> ANot(apply_expr subs e, apply subs t)
  | ABinop(op, e1, e2, t) -> ABinop(op, apply_expr subs e1, apply_expr subs e2, apply subs t)
  | AIf(e1, e2, e3, t) -> AIf(apply_expr subs e1, apply_expr subs e2, apply_expr subs e3, apply subs t)
  | AFunctionCall(fn, arg, t) -> AFunctionCall(apply_expr subs fn, apply_expr subs arg, apply subs t)
  | ALet(id, b, e1, e2, t) -> ALet(id, b, apply_expr subs e1, apply_expr subs e2, apply subs t)
;;

(* 1. annotate expression with placeholder types and generate constraints
   2. unify types based on constraints *)
   let infer (e: expr): typeScheme =
    let rec infer_expr (env: environment) (e: expr): aexpr * typeScheme * (typeScheme * typeScheme) list =
      match e with
      | Int n -> AInt(n, TNum), TNum, []
      | Bool b -> ABool(b, TBool), TBool, []
      | String s -> AString(s, TStr), TStr, []
      | ID x ->
          if List.mem_assoc x env then
            let t = List.assoc x env in
            let inst_t = instantiate t in
            AID(x, inst_t), inst_t, []
          else
            raise UndefinedVar
      | Fun(id, e) ->
          let tid = gen_new_type () in
          let env' = (id, tid)::env in
          let ae, t_body, constraints = infer_expr env' e in
          AFun(id, ae, TFun(tid, t_body)), TFun(tid, t_body), constraints
      | Let(id, false, e1, e2) ->
          let ae1, t1, constraints1 = infer_expr env e1 in
          let subs1 = unify constraints1 in
          let t1' = apply subs1 t1 in
          let env_subst = List.map (fun (v, t) -> (v, apply subs1 t)) env in
          let gen_t1 = generalize env_subst t1' in
          let env' = (id, gen_t1)::env_subst in
          let ae2, t2, constraints2 = infer_expr env' e2 in
          let constraints2' = List.map (fun (l, r) -> (apply subs1 l, apply subs1 r)) constraints2 in
          let subs2 = unify constraints2' in
          let t2' = apply subs2 t2 in
          ALet(id, false, ae1, ae2, t2'), t2', []
      | Let(id, true, e1, e2) ->
          let tid = gen_new_type () in
          let env' = (id, tid)::env in
          let ae1, t1, constraints1 = infer_expr env' e1 in
          let subs1 = unify constraints1 in
          let t1' = apply subs1 t1 in
          let env_subst = List.map (fun (v, t) -> (v, apply subs1 t)) env in
          let gen_t1 = generalize env_subst t1' in
          let env'' = (id, gen_t1)::env_subst in
          let ae2, t2, constraints2 = infer_expr env'' e2 in
          let constraints2' = List.map (fun (l, r) -> (apply subs1 l, apply subs1 r)) constraints2 in
          let subs2 = unify constraints2' in
          let t2' = apply subs2 t2 in
          ALet(id, true, ae1, ae2, t2'), t2', []
      | Not e ->
          let ae, t1, constraints = infer_expr env e in
          let constraints' = (t1, TBool)::constraints in
          ANot(ae, TBool), TBool, constraints'
      | Binop(op, e1, e2) ->
          let ae1, t1, constraints1 = infer_expr env e1 in
          let ae2, t2, constraints2 = infer_expr env e2 in
          let opc, t = match op with
            | Add | Sub | Mult | Div -> [(t1, TNum); (t2, TNum)], TNum
            | Concat -> [(t1, TStr); (t2, TStr)], TStr
            | Greater | Less | GreaterEqual | LessEqual | Equal | NotEqual -> [(t1, t2)], TBool
            | And | Or -> [(t1, TBool); (t2, TBool)], TBool
          in
          let constraints = constraints1 @ constraints2 @ opc in
          ABinop(op, ae1, ae2, t), t, constraints
      | If(e1, e2, e3) ->
          let ae1, t1, constraints1 = infer_expr env e1 in
          let ae2, t2, constraints2 = infer_expr env e2 in
          let ae3, t3, constraints3 = infer_expr env e3 in
          let constraints = constraints1 @ constraints2 @ constraints3 @ [(t1, TBool); (t2, t3)] in
          AIf(ae1, ae2, ae3, t2), t2, constraints
      | FunctionCall(fn, arg) ->
          let afn, t_fn, constraints_fn = infer_expr env fn in
          let aarg, t_arg, constraints_arg = infer_expr env arg in
          let t_res = gen_new_type () in
          let constraints = constraints_fn @ constraints_arg @ [(t_fn, TFun(t_arg, t_res))] in
          AFunctionCall(afn, aarg, t_res), t_res, constraints
    in
    let ae, t, constraints = infer_expr [] e in
    let subs = unify constraints in
    let t_final = apply subs t in
    type_variable := (Char.code 'a');  (* Reset type variable counter *)
    t_final
  ;;
