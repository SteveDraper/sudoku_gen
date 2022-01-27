# sudoku_gen
Generalized Sudoku solver(s)

## Intro

Recently a friend sent me a message encoded in a 16X16 Sudoku, with cells acting as keys for letters in the message.  A little
time spent with pen and peper showed that 16 X 16 sudokus are sufficiently much harded than 9 X 9 ones for humans to do!

Consequently I decided this was a good time to do a small learning project to blow the cobwebs of my Haskell (which I have
used before, but never on a substantial proiject and not recently).

After completing this it seemed like a natural task to use for new languages from time to time, so I have now added a RUST
implementation.

In general I'm striving for reasonably idiomatic implementations in the target language, while also making some effort to be
performant.

## Algorithm used

In all cases I've taken a simple constraint-solver approach as per the following pseudo-code:

* If all cells are filled in then return the state as the solution
* Choose the cell whose legal values are most constrained by the cells that are filled in
* For each choice (until a solution is found):
  * Set the chosen cell to the enumerated choice value and recurse
* If no solution was found return the non-existance of a solution

This doesn't guarantee that the solution is unique (but we could easily do that by simply running this twice with the
enumeration order of choices for a given cell reversed, and checking the result is the same).  However, it does guarantee
to find a solution if one exists, and the most-constrained cell ordering is a fairly efficient ordering on the search
space.

## Haskell solver

This attempts to be fairly idiomatic:

* Parser-combinator approach to parsing input files
* Use of sum types to handle errors and potential non-existance
* Everything immutable

### Build and run

Assuming you have `GHC` and `stack` installed (see https://docs.haskellstack.org/en/stable/README/ otherwise):

```
cd sudokuHask
stack build
stack run -- -i ../test_data/test16solvable.txt
```

You should see something like this:
```
N R H S C P A O I L M E W Y T D
L C M A N T R W D H Y P O E S I
E P O W I D S Y C A T R N H L M
D T I Y H L E M N S O W P A C R
M S L R T N C A E I P H D W Y O
Y H A E P W L S R C D O T M I N
W O P D R E Y I T N L M S C H A
I N T C M O H D S W A Y L R P E
R L E O Y S N C H P I A M D W T
S I W T A R P H M D E N C L O Y
C A Y H L M D T O R W S I N E P
P D N M O I W E Y T C L A S R H
A E R L S C M N P O H I Y T D W
T Y S P D A I R W E N C H O M L
O M C N W H T P L Y R D E I A S
H W D I E Y O L A M S T R P N C
Solution visited 46810 states
Total time: 35.022938s
```

Seems kinda slow.  This could certainly be significantly optimized at the expense of losing the idiomatic nature of the
implementation.  Most of the time associated with the large churn on the heap of short-lifetime objects.  For this task
especially this adds a lot of overhead and makes it not very CPU-cache friendly.

Sometime I may try to create a version using `IORef` and operating on a singleton mutable state.  However, that's not really
the point of the exercise

## Rust solver

Being idiomatic here (I think - this was my first Rust code) means embracing the mutable state and going for a representation
that minimizes boxing and heap access.  So:

* State is held as a struct primarily contraining mutable vectors for:
  * cell-state - encoded as a numeric between 0 and <board-size>-1, or -1 for unoccupied
  * open-choice sets for each row, column, and sub-square
* Choice sets implemented as bit-sets in 32-bit words, which means the set operations all amount to very direct bitwise
  operation on u32's

### Buid and run

Assuming you have `cargo` installed (see https://www.rust-lang.org/tools/install otherwise):

```
cd sudokuRust
cargo run --release -- ../test_data/test16solvable.txt
```

You should see something like this:

```
Solution found in 465 milliseconds
N R H S C P A O I L M E W Y T D
L C M A N T R W D H Y P O E S I
E P O W I D S Y C A T R N H L M
D T I Y H L E M N S O W P A C R
M S L R T N C A E I P H D W Y O
Y H A E P W L S R C D O T M I N
W O P D R E Y I T N L M S C H A
I N T C M O H D S W A Y L R P E
R L E O Y S N C H P I A M D W T
S I W T A R P H M D E N C L O Y
C A Y H L M D T O R W S I N E P
P D N M O I W E Y T C L A S R H
A E R L S C M N P O H I Y T D W
T Y S P D A I R W E N C H O M L
O M C N W H T P L Y R D E I A S
H W D I E Y O L A M S T R P N C
```

Much faster!  Note that this is not a comment on the relative merits of the languages in general, but I *do* think Rust
is the more natural choice for this particular task.

### Updated Rust solver

As of May 25th, I have now committed a slightly more optimized version of the Rust solver, which brings the solution time down to 150ms.

## Python Solver

Just for comparison I have added a python version using the same algorithm.  Although
this is only mildly optimized, it isn't doing anything stupid in terms of object allocation/lifetime.
Like the Rust version it's just mutating in-place (using a numpy array as the underlying board structure),
though it relies on default python `set` instances, which could be optimized as bitsets.

None-the-less, the execution time of the same reference puzzle is around 85 seconds - nearly
3 orders of magnitude slower than the Rust implementation

## NodeJS solver

Since I need to learn some Javascript/node, I decided to port into this environment
also.  Execution time is around 10 seconds.
