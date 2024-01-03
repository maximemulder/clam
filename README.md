# What is this ?

This is  Clam ! A small statically typed functional programming language with a relatively advanced type system. It implements many features of System $F^ω_{<:}$ and its extensions such as [structrual typing](#structural-typing), [subtyping](#subtyping), [unions and intersections](#union-and-intersection-types), [bounded polymorphism](#universal-types), [higher-kinded types](#higher-kinded-types) and [constraint-based type inference](#type-inference).

The formal (but currently incomplete) semantics of the language are available in the `semantics.pdf` document available [here](https://raw.githubusercontent.com/MaximeMulder/Clam/main/semantics/semantics.pdf).

# Example

A Clam program is a sequence of type and expression definitions, declared respectively with the keywords `type` and `def`. The order of these definitions is unimportant.

When a program is executed, the `main` expression definition is evaluated.

Printing is done through the (impure) `print` function, which can be chained with other expressions using a semicolon.

```
def fibonacci = (n) ->
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

## Literals

Clam provides literal expressions for a few basic data types, namely `Bool`, `Int` and `String`.

```
def yes: Bool = true
def zero: Int = 0
def hello: String = "Hello world !"
```

## Functions

Clam features functions, whose parameters and arguments are delimited by parentheses. Functions (or any expression currently) can be recursive, and are desugared in their unary form to enable currying.

```
def add = (a: Int, b: Int) -> a + b
def step = add(1)
def three = step(2)
```

## Product types

Clam features product types in the form of tuples and records, whose types and values are delimited by curly braces. Tuple fields are indexed by their order of declaration and record fields are indexed by their labels. Fields are accessed using the dot operator `.` and the empty product `{}` is considered to be a record.

```
type Tuple = {Int, Int}
type Record = {x: Int, y: Int}

def tuple: Tuple = {0, 1}
def record: Record = {x = 2, y = 3}

def zero = tuple.0
def two = record.x
```

## Structural typing

Clam features structural typing, which means that types are defined by their structure, and that two types with the same structure are considered equivalent.

```
type Ball = {diameter: Int}
type Sphere = {diameter: Int}

def ball: Ball = {diameter = 10}
def sphere: Sphere = ball
```

## Subtyping

Clam features structural subtyping, which means that there exists a partial order between types where some types can be considered more specific or more general versions of others. Records notably support both width and depth subtyping, while tuples only support the latter.

```
type Double = {x: Int, y: Int}
type Triple = {x: Int, y: Int, z: Int}

def triple: Triple = {x = 0, y = 0, z = 0}
def double: Double = triple
```

*\* Tuples and their subtyping will be reworked in the future.*

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

Clam has a bottom type named `Bot`, which contains no value and is therefore a subtype of all proper types.

```
def foo = (bot: Bot) ->
    var a: Unit = bot;
    var b: Bot  = bot(0).1.foo;
    unit
```

## Union and intersection types

Clam features union and intersection types, with a support for distributivity, joins and meets.

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

Clam features universal types, which allow to abstract over an expression using types. Type parameters have an upper bound, which is `Top` by default.

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

Clam features type constructors, which allow to abstract over a type using other types.

```
type Pair = [T] => {T, T}

def pair: Pair[Int] = {0, 0}
```

## Higher-kinded types

Clam features higher-kinded types, which allow type constructors to abstract over other type constructors. Higher-kinded types are currently invariant with regards to their parameters.

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

## Type inference

Clam features a constraint-based type inference algorithm capable of inferring almost all the types of closed expressions.

<table>
    <tr>
        <td>Declared expressions</td>
        <td>Inferred types</td>
    </tr>
    <tr>
<td><pre><code>def two = 1 + 1
def id = (p) -> p
def max = (l, r) -> if l > r then l else r
def inf = (p) -> inf(p)
def foo = (f) -> {f(123), f("Hello")}
def bar = f(id)
def is_even = (n) -> !is_odd(n)
def is_odd = (n) -> if n == 0 then false else !is_even(n - 1)</code></pre></td>
<td><pre><code>two: Int
id: [T] -> (T) -> T
max: [T: Int, U: Int] -> (T, U) -> (T | U)
inf: (Top) -> Bot
foo: [T, U] -> (((Int) -> T) & ((String) -> U)) -> {T, U}
bar: {Int, String}
is_even: (Int) -> Bool
is_odd: (Int) -> Bool</code></pre></td>
    </tr>
</table>

Some of these examples, as well as inspirations for the algorithm, were shamelessly stolen from TACO Lab's [SuperF](https://hkust-taco.github.io/superf/) and [MLScript](https://hkust-taco.github.io/mlscript/) languages

*\* The algorithm for type inference was developped hastily and is currently extremely dirty. There are likely a few bugs remaining, notably with recursive types which Clam does not support yet. However, most cases as well all the provided examples work, which I think is a nice achievement.*

*\*\* Type inference for tuples is currently quite limited. However, this is intentional as row types without width subtyping are fundamentally incompatible with type inference for closed expressions. As a result, Clam's tuples will be reworked in the future.*

*\*\*\* The types shown by the interpreter currently do not look as good as these examples as the printing function is quite basic. However, the types shown by the interpeter are still the same as these ones.*

## Recursive types

Clam does not feature recursive types yet, which is quite limiting.

# Correctness

There is currently no mathematical proof of correctness for this language or interpeter. But I hope I will be able to write one one day. While some code (notably for type inference) is still rough at the time of writing these lines, all the examples provided work, as well as all the unit tests and the sample programs found in the `tests` directory.

**IMPORTANT: The above statement is not true if you see this sentence but it should be in the next week**

# Roadmap

I will have less time to work on this project in the short- to medium-term future, however, here is my roadmap for Clam:
- Polish the interpreter's code and algorithms.
- Improve error handling and user experience.
- Add recursive types.
- Add negation types.
- Add pattern matching through disjoint switches.
- Add other cool stuff (function totality checking, dependent types... unfortunately very far away)

# Notes

Clam is a pet project of mine I created during my master's thesis evaluation to apply the knowledge I gained on programming language theory and practice functional programming. It has since gained in size and functionality but is still not intended to be a used for serious programming, especially since the user experience is currently quite rough.
