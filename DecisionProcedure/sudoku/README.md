# Sudoku solver

In this part, you will write a Sudoku solver by translating a puzzle to a
logical formula that can be solved by the SMT solver Z3. Your program will
accept a puzzle on standard input and write its solution (if it has one) on
standard output.

## Dependencies

- Install [Dune](https://dune.build/) for code testing
- Install [Z3](https://github.com/Z3Prover/z3/releases)
- Ensure that if you open a new terminal and type `z3`, you get a message like this:

      $ z3
      Error: input file was not specified.
      For usage information: z3 -h

  if you instead get something about z3 not being found, debug your
  installation or ask for help.
- `opam install ppx_deriving ppx_inline_test ppx_expect` to get the testing framework.
- Make sure you can use `dune runtest -f` to execute our testcases.

## Getting oriented

Start by looking at `test.input` and `test.expected`, which contain an example
of the input and output format we will use. A puzzle's "concrete syntax" is a
sequence of integers separated by whitespace. The format begins with a single
number that we will call `k` that describes the size of the puzzle. A standard
puzzle has `k = 3`, but any nonnegative value is possible. Let `n = k * k`, then
`n` is the the height and width of the board. There are `n * n`
(that is, `k * k * k * k`) cells on a board. After `k`, the concrete format
for puzzle input and output consists of `n * n` cells separated by whitespace.
Each cell has a number in the range `1 .. n`, or 0, which represents a blank cell.
0 is only used in input puzzles; solved puzzles do not have blanks.

Now take a look at `sudoku.ml`. Its main binding calls `Puzzle.from_channel` (which
you will implement) to read a puzzle from standard input. Next, it prints
the puzzle using `Puzzle.show` (which you will implement). After that,
it spins up a Z3 process and uses it to print "hello from z3" before exiting.

Next, look at `puzzle/puzzle.mli` and `puzzle/puzzle.ml`. The `.mli` file shows
the types of the functions we want to export for use by the main file.  The
`.ml` file contains some incomplete starter code for parsing a puzzle, and an
empty implementation for printing a puzzle. It also contains an "inline test"
(commented out) that tests whether the module can correctly parse and print a
small example puzzle.

Now look at `z3/z3.mli` and `z3/z3.ml`, which contain a small library to talk to
Z3 over a Unix pipe. You can call `Z3.raw_send` to send Z3 a message, and
`Z3.raw_read_line` to read one line of response from Z3.

We have split this part of the project into separate subdirectories for two
different reasons. `Puzzle` is in its own directory so that we can use inline
expectation tests, which are not supported by Dune when building
an executable directly, only on standalone libraries in their own directory.
`Z3` is in its own directory because we will reuse this module on future
homeworks.


## More precise specification

Now that we have an idea what the files are, let's be more precise about the
possible outputs from your program. There are three possibilities

- The input puzzle has no solution. In that case, print "unsat" and exit. See
  `test-nosol.input` and `test-nosol.expected`.

- The input puzzle has exactly one solution. In that case, print the solution in
  the format described above, and then print the string "puzzle solution is
  unique" on a line by itself.

- The input puzzle has more than one solution. In that case, print one solution,
  followed by the string "found more than one solution!" on a line by itself,
  followed by a second solution.

Your program should be able to translate puzzles with any value of `k` in the
range 2 to 100. However, Z3 might not be able to solve them for large k. That's
ok.

## Walkthrough

At this point, you may be able to complete Part 1 without further direction.
Feel free to give it a shot! Here is a bit more of a walkthrough in case you
get stuck.

- Read a little bit about Sudoku if you're not familiar. Make sure you
  understand what it means for a Sudoku board to be a valid solution.

- Make a plan for expressing the constraints of Sudoku in logic. We recommend
  using integer variables, one per cell of the board. But other encodings
  (even to SAT, without integers!) are possible, and you are free to use them
  if you wish. Write down your planned constraints for the `test-small.input`
  puzzle on a piece of paper. (No need to turn in.)

- Translate your logical constraints to Z3 syntax. Debug your encoding by
  making a `.smt2` file and writing the constraints for `test-small.input`.
  Make sure that Z3 returns "sat" and that the values obtained by `get-value`
  correspond to the expected solution in `test-small.expected`. You may find
  the Z3 `distinct` expression useful in your encoding.

- Design a data structure to represent a Sudoku board in OCaml. There are many
  choices here, but keep it simple and think about what kind of access to the
  data you are going to want. Write your data structure as a type definition
  on the first line of `puzzle.ml`.

- Implement `Puzzle.from_channel` and `Puzzle.show`. Follow the instruction in
  the comment at the bottom of `Puzzle.from_channel`. Uncomment the testing code
  at the bottom of the file, and run `dune runtest` *from inside the directory
  `puzzle`* and ensure it passes. (By running Dune in the subdirectory, you
  avoid running the tests from the parent directary, which you're not ready for
  yet.)

- Write a function in `sudoku.ml` that prints out the constraints you designed
  to standard output. To do this, you will either need to expose your data
  structure type definition in `puzzle.mli` or add functions to the interface
  that allow accessing the data you need. Either approach is fine.

  Make sure your constraint-generating function prints exactly the same
  constraints that you wrote by hand in your `.smt2` file. Also make sure Z3
  still finds "sat" and that the values look reasonable.

- Use `Z3.raw_send` to send your constraints to Z3 instead of standard out.  If
  you need to debug, you can uncomment the line in `z3.ml` inside `raw_send` so
  that you can confirm the correct strings are being sent. Check that Z3
  still finds "sat" and that the values look reasonable. We recommend printing
  the result from "(check-sat)" and from "(get-value (...))" to standard out
  so you can see what's going on.

- Parse the output of the "(check-sat)" and "(get-value (...))" to fill in
  the solved board. You will find it useful to make a fresh board object
  for the solved board, because in the next part, you will need access to
  both the unsolved and solved board.

  For parsing, your life will be easier if you use "(eval ...)" rather than
  `get-value`, since `eval` doesn't print the expression being evaluated
  or any parentheses.

  Once you've got this part working, print the solved board to standard out
  using `Puzzle.show`. Confirm that `test-small.input` and `test.input` pass.

- If "(check-sat)" gives back "unsat", print that to standard out and exit.

- Go back to your piece of paper and figure out how to extend your logical
  constraints to check that a solution is unique. Extend your manual `.smt2`
  file to check that the solution to `test-small.input` is unique.

- Implement your uniqueness check. You may have to refactor your existing
  code to make it more flexible. Try to share as much code as possible
  between the part that solves the puzzle and the uniqueness check.

  If you find that the puzzle is unique, print the required message.

  Otherwise, if the puzzle is not unique, print the required message,
  construct the second solved board, and print it.


## What to turn in

Zip up the files you edited, along with the dune files, and upload to Canvas.

- sudoku.ml
- dune
- puzzle/puzzle.mli
- puzzle/puzzle.ml
- puzzle/dune
- z3/z3.ml
- z3/z3.mli
- z3/dune

## Extension

- Describe a search strategy to solve Sudoku without using Z3. Your idea must be
  "inspried by" CDCL (ie, it should learn conflicts and backtrack nonchronologically).
  Submit your description in a file named sudoku-cdcl.txt.
