
/** Assignment 3 Hoare Logic (Dafny part)
    This assignment helps you strengthen your understanding of using Hoare Logic to verify programs.

    We will use Dafny to prove the correctness of a program.

    Please install Dafny by https://dafny.org/dafny/Installation

    To learn more about Dafny, check out the tutorial https://dafny-lang.github.io/dafny/OnlineTutorial/guide.html
    The examples in the tutorial are also available at https://github.com/RU-Automated-Reasoning-Group/CS515/blob/master/Dafny/example.dfy
**/




/* Question 1 (0.5 points). Provide an implementation that satisfies the given post-condition */
method M1 (x0 : int) returns (x : int)
  ensures (x0 < 3 ==> x == 1) && (x0 >= 3 ==> x < x0);
{
  if x0 < 3 {
        return 1;
    } else {
        return x0 - 1;
    }
}




/* Question 2 (0.5 points). Provide an implementation that satisfies the given post-condition */
method M2(a : int, b : int, c : int) returns (m : int)
  ensures (m == a || m == b || m == c);
  ensures (m <= a && m <= b && m <= c) ;
{
   m := a;
    if b < m {
        m := b;
    }
    if c < m {
        m := c;
    }
}




/* Question 3 (1 points). Provide a loop invariant to prove the given assertion in the function */
method M0 (n : int, d : int) returns (r : int)
    requires (n >= 0 && d > 0)
{
    var q := 0;
    r := n;
    while r >= d
      invariant n == q * d + r && r >= 0
    {
        q := q + 1;
        r := r - d;
    }
    assert (n == q * d + r && 0 <= r && r < d);
}




/* Question 4 (1 points). Supply meaningful loop invariants to verify
   the iterative factorial_iterative implementation below.
   */

// 0! = 1
// n! = n * (n - 1)!
// A recursive factorial function implementation
function factorial(n: int): int
  requires n >= 0;
  ensures factorial(0) == 1;
	ensures n > 0 ==> factorial(n) == n * factorial(n - 1);
{
	if n == 0 then
		1
	else
		n * factorial(n - 1)
}
/* factorial_iterative has a postcondition which expresses that it behaves the
   same as factorial. While the implementation is correct, Dafny is unable to
   prove that the implementation satisfies the postcondition fully automatically.
   For this part of the assignment, you'll need to add loop invariants and possibly
   assertions to prove the code correct. You should not change the preconditions,
   postconditions, or signature of factorial_iterative or factorial.

   The loop specifically starts with a low number and then goes high. This enables
   the proof to effectively start with a statement like “the factorial of this low
   number is equal to my result so far”, and then incrementally increase this number
   until you're at the end target of n.

   It is possible to prove this correct using only two loop invariants.
 */
method factorial_iterative(n: int) returns (result: int)
	requires n >= 0;
	ensures result == factorial(n);
{
	result := 1;
	var index := 1;
	while (index <= n)    
    invariant result == factorial(index - 1)
    invariant 1 <= index <= n + 1
	{
    assert index >= 1 && index <= n + 1;
		assert result == factorial(index - 1);
		result := result * index;
		index := index + 1;
	}
}




/* Question 5 (1 points). Supply meaningful post-conditions and loop invariants to verify
   the unique sequence implementation (see the comment below).
   You may find the following documentation of Dafny sequences useful:
   https://dafny-lang.github.io/dafny/OnlineTutorial/Sequences.html 

 Take a sequence of integers and return the unique elements of that 
 list. There is no requirement on the ordering of the returned 
 values. */
method Unique(a: seq<int>) returns (b: seq<int>)
{
  var index := 0;
  b := [];
  while index < |a|
  {
    if (a[index] !in b) {
      b := b + [a[index]];
    }
    assert a[index] in b;
    index := index + 1;
  }
  return b;
}




/* Question 6 (2 points). Supply meaningful post-conditions and loop invariants to verify
   the array insertion sort implementation */
method insertionSort(a : array<int>)
    requires a != null
    modifies a
{
    var i := 0;
    while (i < a.Length)
    {
        var j := i - 1;
        while (j >= 0 && a[j] > a[j + 1])
        {
            a[j], a[j + 1] := a[j + 1], a[j];
            j := j - 1;
        }
        i := i + 1;
    }
}




function max(a: int, b: int): int
{
    if a > b then a else b
}




/* Question 7 (2 points). Supply meaningful loop invariants to verify
   the LeftPad implementation below */

/* Given a padding character, a string, and a total length,
   return the string padded to that length with that character.
   If length is less than the length of the string, do nothing. */
method LeftPad(c: char, n: int, s: seq<char>) returns (v: seq<char>)
  ensures |v| == max(n, |s|)

{
    var pad, i := max(n - |s|, 0), 0;
    v := s;
    while i < pad
        invariant 0 <= i <= pad
        invariant |v| == |s| + i
        invariant (forall j :: 0 <= j < i ==> v[j] == c)
    {
        v := [c] + v;
        i := i + 1;
    }
}




/* Question 8 (2 points). Supply meaningful loop invariants to verify
   the implementation below.

   This code contains a splice_in method, which is used to stitch one array into
   another, returning the result. Tthe implementation you've been provided is correct.
   You'll need to add in invariant and assert statements to get this code working.
   You may change the implementation if you desire, but you may not change the signature
   of splice_in, or the preconditions and postconditions of splice_in.

   The provided implementation can be divided into three parts:
   1. Copy elements from a until index into the result array.
   2. Copy the contents of add_these into the result array.
   3. Copy the remaining elements in a into the result array.
   Each one of these parts has its own corresponding postcondition on splice_in.

   With this in mind, there are two major things you need to do for each of the three parts:
   1. Show that the given part satisfies its given postcondition. You'll need to
   show that the part's loop incrementally gets closer to the part's postcondition.
   It's recommended to put an assert statement immediately after the part which simply
   asserts the corresponding postcondition, which will give you a quick indication of
   whether or not you've actually satisfied the postcondition (this assertion will
   be true if you satisfied the postcondition, and it will be false if you didn't).

   2. Show that the given part doesn't jeopardize the postconditions from any previous
   parts. Related to the previous point, you should start with assert statements which
   merely repeat the other postconditions. If these fail, you'll need to provide Dafny
   further information that you haven't broken another postcondition. You may need to
   add prior proven postconditions as loop invariants later (i.e., it's invariant that
   this loop doesn't break a previously-proven postcondition.
 */
method splice_in(a: array<int>, index: int, add_these: array<int>) returns (result: array<int>)
    requires a != null;
    requires add_these != null;
    requires 0 <= index < a.Length;
    ensures result != null
	ensures result.Length == a.Length + add_these.Length;
	ensures forall i :: 0 <= i < index ==> a[i] == result[i];
	ensures forall i :: 0 <= i < add_these.Length ==> add_these[i] == result[i + index];
	ensures forall i :: index <= i < a.Length ==> a[i] == result[i + add_these.Length];
{
	result := new int[a.Length + add_these.Length];

	// copy first part in
	var pos := 0;
	while (pos < index)
        invariant 0 <= pos <= index
        invariant forall i :: 0 <= i < pos ==> result[i] == a[i]
	{
		result[pos] := a[pos];
		pos := pos + 1;
	}

	// copy in the addition
	pos := 0;
	while (pos < add_these.Length)
        invariant 0 <= pos <= add_these.Length
        invariant forall i :: 0 <= i < pos ==> result[index + i] == add_these[i]
        invariant forall i :: 0 <= i < index ==> result[i] == a[i]
	{
		result[index + pos] := add_these[pos];
		pos := pos + 1;
	}

	// copy the last part in
	pos := index;
	while (pos < a.Length)
        invariant index <= pos <= a.Length
        invariant forall i :: index <= i < pos ==> result[i + add_these.Length] == a[i]
        invariant forall i :: 0 <= i < index ==> result[i] == a[i]
        invariant forall i :: 0 <= i < add_these.Length ==> result[index + i] == add_these[i]
	{
		result[pos + add_these.Length] := a[pos];
		pos := pos + 1;
	}
}
