# What is this ?

This is Clam ! A small statically typed functional language with many interesting type features.

I developped this interpreter while my master's thesis was being evaluated. I made it to learn OCaml, practice functional programming, and apply some of the knowledge I gained about types and type theory.

# Examples

A Clam program is a sequence of type definitions (declared with `type`) and expression definitions (declared with `def`). The order of these definitions is unimportant.

When a program is executed, the `main` expression definition is evaluated. Printing can be done through the `print` function, which can be chained with other expressions in a block.

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

You can build this project on Linux using Dune !

Simply enter `dune build` to build to build the project or `dune exec main example.clam` to run the interpreter with the code file `example.clam` as an input.

# Features

Most of the interesting things about Clam are in its type system, which while not very original, incorporates many ideas notably of System $F^ω_{<:}$.

## Structural typing

Clam features a structural type system, meaning that two structurally equivalent types are considered the same.

```
type A = {Int, Int}
type B = {Int, Int}

def main =
    var a: A = {0, 0};
    var b: B = a;
    unit
```

## Subtyping

Clam features subtyping, meaning that a value may belong to several types and that some types may be considered to be more specific versions of other types.

```
type A = {x: Int, y: Int}

def main =
    var a: A = {x = 0, y = 0, z = 0};
    unit
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

def main =
    var a: Pair[Int] = {0, 0};
    unit
```

Type parameters cannot be of a higher kind yet.

## Universal types

Clam features universal types, which allow to abstract over an expression using types.

```
def map_pair = [T, U] -> (p: (T, T), f: (T) -> U) ->
    {f(p.0), f(p.1)}

def main =
    var pair = {2, 3};
    var double = map_pair[Int, Int](pair, (x) -> x * 2);
    unit
```

## Union and intersection types

Clam features union and intersection types.

```
type A = {a: Int} | {a: String}
type B = {a: Int} & {b: String}

def a: A = {a = 1}
def b: B = {a = 2, b = "World"}

def distributivity = [A, B, C] -> (developed: (A & B) | (A & C)) ->
    var factorized: A & (B | C) = developed;
    unit
```

## Bidirectional type inference

Clam features bidirectional type inference, which allows to eliminate many type annotations when they are not needed.

```
type Pair = [T] => {T, T}
type Make = [T] -> (T) -> Pair[T]

def make: Make = [T] -> (p) -> {p, p}

def main =
    var pair = make[Int](0);
    unit
```

## Recursive types

Clam does not feature recursive types yet, which is kind of a bummer.

# Notes

Clam is not intended to be a usable programming language. It is just a pet project of mine.

Although the examples provided should work, some features are not yet complete, notably unions and intersections and the bottom type. I want to finish them and improve my testing framework before adding new features.
