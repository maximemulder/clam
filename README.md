# What is this ?

This is  Clam ! A small statically typed functional programming language with many classic but interesting type features.

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

In a terminal, enter `dune build` to build the project or `dune exec main example.clam` to run the interpreter with the code file `example.clam` as an input.

# Features

Most of the interesting things about Clam are in its type system, which while not very original, incorporates many ideas notably of System $F^ω_{<:}$.

## Product types

Product types and values in Clam are defined with curly braces, and can either be tuples or records. Tuple fields are indexed by their order and record fields are indexed by their labels. Fields are accessed using the dot operator `.` and the empty product `{}` is considered to be a record.

```
type Tuple = {Int, Int}
type Record = {x: Int, y: Int}

def tuple: Tuple = {0, 1}
def record: Record = {x = 2, y = 3}

def zero = tuple.1
def two = record.x
```

## Structural subtyping

Clam features structural subtyping, meaning that two structurally equivalent types are considered to be equal. Records are considered extensible by subtyping while tuples are not.

```
type 2D = {x = Int, y = Int}
type 3D = {x = Int, y = Int, z = Int}

def 3d: 3D = {x = 0, y = 0, z = 0}
def 2d: 2D = 3d
```

## Unit type

Clam has a default unit type named `Unit`, whose only value is `unit`.

```
def u: Unit = unit
```

## Top type

Clam has a top type named `Top`, which is a supertype of all types.

```
def a: Top = 0
def b: Top = "Hello world !"
```

## Bottom type

Clam has a bottom type named `Bot`, which is a subtype of all types.

```
def foo = (bot: Bot) ->
    var a: Unit   = bot;
    var b: String = bot;
    unit
```

## Type operators

Clam features type operators, which allow to abstract over a type using other types. Type parameters have a bound, which is `Top` by default.

```
type Pair = [T] => {T, T}

def pair: Pair[Int] = {0, 0}
```

## Universal types

Clam features universal types, which allow to abstract over an expression using types.

```
def map_pair
    : [T, U] -> ((T, T), (T) -> U) -> (U, U)
    = [T, U] -> (p, f) -> {f(p.0), f(p.1)}

def pair = {1, 2}
def double = map_pair[Int, Int](pair, (x) -> x * 2)
```

## Union and intersection types

Clam features union and intersection types, with integrated distribution, joins and meets.

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

## Bidirectional type inference

Clam features bidirectional type inference, which allows to eliminate many type annotations when they are not needed.

```
type Make = [T] -> (T) -> {T, T}

def make: Make = [T] -> (p) -> {p, p}

def main =
    var pair = make[Int](0);
    unit
```

## Recursive types

Clam does not feature recursive types yet, which is quite limiting.

# Roadmap

Here are a few features I would like to eventually work on in the future:
1. Finish unions, intersections and the bottom type
2. Improve code quality (monadic error handling and better testing)
3. Add higher-order types
4. Add recursive types
5. Add negation types
6. Add pattern matching using types
7. Add function totality checking

# Notes

Clam is simply a pet project of mine, it is not intended to be a full-blown programming language. I created it during my master's thesis evaluation to learn OCaml, practice functional programming and apply some of the knowledge I had gained on types and type theory.

As said in the roadmap, some features are not complete yet, although the examples should work.
