# Homework 3a: Dafny Part (10 pts)

In this part of the assignment, you will verify several programs using Dafny.
Please refer to hw3.dfy for detailed instructions.

# Homework 3b: Programming Part (20 pts)

In this part, you will implement Hoare logic for IMP language.  As covered in
class, this language has the following statements: skip, assignment, if, assume,
assertios, sequential compositions (';'), and while loops.

The goal is to verify IMP programs using Hoare logic. You will be graded on the
correctness of your implementation of weakest precondition generation (wp) and
your use of z3.


## Part 1: Implementing Hoare logic

### Getting acquainted with the starter code

In the starter code, the lexer/parser/syntax support statements and expressions.
For statements, there is a special `assume` statement, described below, as well
as an if statement. The syntax of while loops includes invariant annotations,
using the `invariant` keyword.

Our test program is given in `test.input`, and includes an example of an
assume statement and a few loop invariants.

`hw3.ml` contains an interpreter and typechecker for the starter code, as well
as code to parse command line arguments. The CLI to `hw3.ml` consists of two
"modes": interpretation mode and verification mode. Each mode also requires the
`-heap` flag, which specifies the initial values in the heap for any "input"
variables (such as `n` in our test program).

When passing command-line arguments via dune, you need to be careful to use `--`
to separate the dune command line from the program's command line, like this:

    dune exec ./hw3.exe -- -interpret -heap n=10 test.input

which should print

    [("n", (Syntax.VInt 10))]
    [("y", (Syntax.VInt 0)); ("x", (Syntax.VInt 10)); ("n", (Syntax.VInt 10))]

showing the initial and final heaps. In general, the syntax of the `-heap`
argument is a list of assignments, separated by commas, where an assignment
consists of a variable name followed by an equals sign, followed by an integer
or boolean value, something like this:

    -heap foo=3,bar=false,baz=17

All of this is parsed for you an exported by the `Args.parse_args` function in a
record of type `Args.t`.

The main binding of `hw3.ml` uses `parse_args` to get the mode and heap. It uses
the heap to construct an initial heap typing, typechecks the program, and then
matches on what mode it's in. Mode `interpret` is implemented for you, and just
calls the evaluator on the initial heap, printing the resulting final heap.

Your job is to implement verification mode `verify` (the -heap option is still
needed though the verifier would ignore it as it verifies the program over all
possible inputs).

    dune exec ./hw3.exe -- -verify -heap n=10 test.input

First, an interlude about assume statements.

### Assume statements

NOTE(assume-statements): There is one kind of IMP statement that will be useful
for expressing preconditions of programs. The statement `assume e` has somewhat
strange operational semantics. It steps to a special kind of error state,
perhaps called `assumption_violated`, which is then stuck. The idea is that in
theorems about the Hoare logic, we always assume (!) that execution never
reaches this state. So, the Hoare logic does *not* need to rule out these
violations, in contrast to assertions. You should handle assume statements as
follows:

  - During evaluation: evaluate the expression. If it is true, do nothing and
    return the current heap. Otherwise, if it is false, print a warning that an
    assumption was violated, and then return the current heap. (This part is
    done for you in the starter code.)

  - During verification: implement the following rule for computing `wp` for an
    `assume` statement.

        wp(assume e, Q) = (e ==> Q, \emptyset)

    where `==>` is implication. This captures the idea that in order to prove
    `Q` will be true after an assume statement, it suffices to "assume" (!) `e`
    is true now, and then prove `Q` is true now.

End of NOTE(assume-statements).

### Implementing verification mode

Here is a walkthrough.

- You will need your z3 library from hw2.

- You are going to implement an OCaml function

      wp : (* s *) Syntax.stmt -> (* Q *) Syntax.expr -> Syntax.expr * Syntax.expr list

  that takes a statement `s` and a postcondition `Q` and returns `(P, C)`, where
  P is its weakest precondition and `C` is a set (list) of side conditions.

  Keep the following in mind:

    - Look at the [accompanying slides](Automating Hoare Loigc.pdf) to help you review
      what has been covered in the lecture about program verification.

    - As an example, use the following rule to generate the wp of if statements.

        wp(if e { s1 } else { s2 }, Q) =
          let (P1, C1) = wp(s1, Q)
          let (P2, C2) = wp(s2, Q)
          ((e ==> P1) && (!e ==> P2), C1 \cup C2)

    - In order to implement the assignment case of `wp`, you'll need to
      implement substitution on expressions. Since no expressions bind
      variables, this is relatively straightforward.

    - For the while statement, remember that the `invariant` keyword can appear
      multiple times, just like in Dafny. "The" loop invariant is obtained by
      adding all of these expressions together.

To check correctness of a whole program, we compute its `wp`, and then we want
to check that both the weakest precondition and all the side conditions are
valid using Z3. For that, we need a translator.

- Write a function

      z3_of_expr : expr -> string

  that transforms an IMP expression into a string that represents that
  expression in Z3's SMT input syntax.

  Keep the following in mind.

    - Variables translate to themselves. Literals translate to a string
      representing their value (use the OCaml standard library).

    - Unary operators should translate to a string of the form "(op arg)"
      where "op" is the Z3 symbol representing the operation and "arg" is
      an SMT syntax string representing the recursive translation of the argument.

    - Binary operators are similar.

    - All the operators in the starter code have a direct translation to a Z3
      operator. (See the documentation linked below, especially the Core theory and
      Int theory links.) EXCEPT: `!=` (not equal) does not have a direct translation
      to an operator. You will need to expand it to `!(... = ...)` and then translate
      that.

      [Core boolean SMT theory](http://smtlib.cs.uiowa.edu/theories-Core.shtml)

      [Int SMT theory](http://smtlib.cs.uiowa.edu/theories-Ints.shtml)

      [SMT2 Lib standard]() (See Figure 3.6 on page 45 for a list of standardized
      commands accepted by compliant solvers.)


- In the main binding of `hw3.ml`, find the TODO under the Verify branch of the
  `match` on the mode. Delete what's there and verify the program, as follows.

    - Compute `wp` of the input program, call it `(p, cs)`.

    - For `p` and every element of `cs`, use Z3 to prove the expression valid,
      as follows:

        - compute the variables used in the expression and declare them as Z3
          constants with the type computed by the starter code in `sigma2`.

        - assert the negation of the translation of the expression

        - check whether it is satisfiable

        - if it is satisfiable, print the value of each variable in the counter
          example and report the verification error to the user.

        - if it unsatisfiable, silently succeed.

    - You may find the `(push)` and `(pop)` commands helpful to manage the
      solver. In particular, it is important that each expression is proved
      valid *indpendently*, so you cannot just keep asserting things without
      properly popping the previous assertion off.

    - You will make your life easier for Part 2 by making your error messages as
      helpful as possible. Include the counterexample from Z3, and, if possible
      also a brief description of "why" `wp` wanted this fact to be true. (For
      example, "because the loop invariant should be true on entry to the loop".)

Debug your code by trying to prove the provided `test.input` program correct. Also
check that if you delete any of the provided invariants, your verification tool
reports an error.

## Part 2: Using Hoare logic

Prove the following programs correct by finding `invariant` annotations for
their loops.

### Slow multiplication

Turn in an annotated copy of the following program as a file called `slow-mult.input`
with loop invariants such that its assertion passes.

    // compute a * b using repeated addition, and place the result in ans
    assume a >= 0 && b >= 0;
    ans := 0;
    n := a;
    while n > 0
      invariant true // TODO: add invariants so that the assertion below passes
    {
      ans := ans + b;
      n := n - 1
    };
    assert ans = a * b

### Really slow multiplication

Turn in an annotated copy of the following program as a file called `really-slow-mult.input`
with loop invariants such that its assertion passes.

    // compute a * b using repeated addition (where addition is computed by repeated incrementing), and place the result in ans
    assume a >= 0 && b >= 0;
    ans := 0;
    i := a;
    while i > 0
      invariant true // TODO: add invariants so that the assertion below passes
    {
      j := b;
      while j > 0
        invariant true // TODO: add invariants so that the assertion below passes
      {
        ans := ans + 1;
        j := j - 1
      };
      i := i - 1
    };
    assert ans = a * b

### Write another integer program

Write and verify a program that computes something interesting over integers and
has a nontrivial specification.
