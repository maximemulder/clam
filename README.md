# What is this ?

This is  Clam ! A small statically typed functional programming language with many classic but interesting features.

# Example

A Clam program is a sequence of type and expression definitions, declared respectively with the keywords `type` and `def`. The order of these definitions is unimportant.

When a program is executed, the `main` expression definition is evaluated.

Printing is done through the (impure) `print` function, which can be chained with other expressions using a semicolon.

```
def fibonacci: (Int) -> Int = (n) ->
    if n == 0 | n == 1 then
        n
    else
        fibonacci(n - 2) + fibonacci(n - 1)

def main =
    print(fibonacci(2));
    print(fibonacci(5));
    unit
```

More examples of Clam code can be found in the `tests` directory.

# How to build

You can build this project on Linux using Dune.

In a terminal, enter `dune build` to build the project or `dune exec main example.clam` to run the interpreter with the code file `example.clam` as its input.

# Features

Clam aims to be a small experimental functional programming language with an easily readable syntax and many features borrowed from System $F^ω_{<:}$ and its derivatives to experiment with.

## Literals

Clam provides literal expressions for a few basic data types, namely `Bool`, `Int` and `String`.

```
def yes: Bool = true
def zero: Int = 0
def hello: String = "Hello world !"
```

## Functions

Clam features functions, whose parameters and arguments are declared using parentheses. Functions (or any expression currently) can be recursive, and are decomposed in their unary form to enable currying.

```
def add = (a: Int, b: Int) -> a + b
def step = add(1)
def three = step(2)
```

## Product types

Clam features tuples and records, whose types and values are declared using curly braces. Tuple fields are indexed by their order and record fields are indexed by their labels. Fields are accessed using the dot operator `.` and the empty product `{}` is considered to be a record.

```
type Tuple = {Int, Int}
type Record = {x: Int, y: Int}

def tuple: Tuple = {0, 1}
def record: Record = {x = 2, y = 3}

def zero = tuple.0
def two = record.x
```

## Structural subtyping

Clam features structural subtyping, which means that two structurally similar types are considered to be equal. Records are extensible by subtyping but tuples are not.

```
type Double = {x: Int, y: Int}
type Triple = {x: Int, y: Int, z: Int}

def triple: Triple = {x = 0, y = 0, z = 0}
def double: Double = triple
```

## Unit type

Clam has a default unit type named `Unit`, whose only value is `unit`.

```
def u: Unit = unit
```

## Top type

Clam has a top type named `Top`, which contains all values and is therefore a supertype of all proper types.

```
def a: Top = 0
def b: Top = "Hello world !"
```

## Bottom type

Clam has a bottom type named `Bot`, which contains no value and is therefore a subtype of all types.

```
def foo = (bot: Bot) ->
    var a: Unit = bot;
    var b: Bot  = bot(0).1.foo;
    unit
```

## Union and intersection types

Clam features union and intersection types, with a support for distribution, joins and meets.

```
type Union = {foo: Int} | {foo: String}
type Inter = {bar: Int} & {baz: String}

def union: Union = {foo = 1}
def inter: Inter = {bar = 2, baz = "World"}

def foo: Int | String = union.foo

def distributivity = [A, B, C] -> (developed: (A & B) | (A & C)) ->
    var factorized: A & (B | C) = developed;
    unit
```

## Universal types

Clam features universal types, which allow to abstract over an expression using types.

```
type Iter = [T] -> (Int, T, (T) -> T) -> T

def iter: Iter = [T] -> (n, v, f) ->
    if n == 0 then
        v
    else
        iter[T](n - 1, f(v), f)

def eight = iter[Int](3, 1, (i) -> i * 2)
```

## Type constructors

Clam features type constructors, which allow to abstract over a type using other types. Type parameters have an upper bound, which is `Top` by default.

```
type Pair = [T] => {T, T}

def pair: Pair[Int] = {0, 0}
```

## Higher-kinded types

Clam features higher-kinded types, which allow type constructors to abstract over other type constructors. Type parameters are invariant with regards to subtyping.

```
type Monad = [M: [T] => Top, A] => {
    return: (A) -> M[A],
    bind: [B] -> (M[A], (A) -> M[B]) -> M[B]
}

type State = [S, T] => (S) -> {T, S}

def state_monad: [S, A] -> Monad[State[S], A] = [S, A] -> {
    return = (a, s) -> {a, s},
    bind = [B] -> (m, f, s) ->
        var bs = m(s);
        f(bs.0, bs.1)
}
```

## Bidirectional type inference

Clam features a simple bidirectional type inference system, which allows to eliminate many type annotations when they are not needed.

```
type Make = [T] -> (T) -> {T, T}

def make: Make = [T] -> (p) -> {p, p}

def main =
    var pair = make[Int](0);
    unit
```

## Recursive types

Clam does not feature recursive types yet, which is quite limiting.

# Correctness

Does this interpreter, and particularly its type checking, actually work ?

It should. I do not have a mathematical proof of correctness (I hope I will one day !), but I have a decent amount of tests and have tried to design the code carefully. What I can ensure is that all the examples in this file and in the `tests` directory work.

There may be a few bugs remaining with higher-kinded types, especially when interacting with other features such as unions and intersections. However, I want to eventually simplify the representation of these types in the future so I will wait until I do so to polish them more thoroughly.

# Roadmap

Here are a few features I would like to eventually work on in the future:
1. Improve the implementation (simplify the representation of normalized types)
2. Better error handling (possibly monadic ?) and reporting
4. Add recursive types
6. Add tagged values and their types
5. Add negation types
6. Add pattern matching using types
7. Add function totality checking

# Notes

Clam is simply a pet project of mine, it is not intended to be a full-blown programming language. I created it during my master's thesis evaluation to learn OCaml, practice functional programming and apply some of the knowledge I had gained on types and type theory.

There is no guarantee on how much I will work on this language. At the time of writing these lines, I just finished a pretty intense rush, so I will take a little pause now.
