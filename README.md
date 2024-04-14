# What is this ?

This is  Clam ! A small statically typed functional programming language with a relatively advanced type system. It implements many features of System $F^ω_{<:}$ and its extensions such as [structrual typing](#structural-typing), [subtyping](#subtyping), [unions and intersections](#union-and-intersection-types), [bounded polymorphism](#universal-types), [higher-kinded types](#higher-kinded-types) and a state-of-the-art [constraint-based type inference algorithm](#type-inference).

The formal (but currently incomplete) semantics of the language are described in the `semantics.pdf` document available [here](https://raw.githubusercontent.com/MaximeMulder/Clam/main/semantics/semantics.pdf).

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
    var three = fibonacci(4);
    print(three);
    unit
```

More examples of Clam code can be found in the `tests` directory.

# How to build

You can build this project on Linux using Dune. In a terminal, enter `dune build` to build the project, which outputs the binary in `_build/install/default/bin`. You can then use `clam example.clam` to interpret the file `example.clam` or use `clam --help` to get more information on the interpreter.

# Features

## Literals

Clam provides literal expressions for a few basic data types such as `Bool`, `Int` and `String`.

```
def yes: Bool = true
def zero: Int = 0
def hello: String = "Hello world !"
```

## Functions

Clam features functions, whose parameters and arguments are delimited by parentheses. Functions (or any expression) can be recursive, and are desugared into their unary form to enable currying.

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

Clam features structural subtyping, which means that some types can be considered more specific or more general versions of others. Records notably support both width and depth subtyping, while tuples only support the latter.

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

Clam has a top type named `Top`, which is a supertype of all proper types and contains all values.

```
def a: Top = 0
def b: Top = "Hello world !"
```

## Bottom type

Clam has a bottom type named `Bot`, which is a subtype of all proper types and contains no values.

```
def foo = (bot: Bot) ->
    var a: Unit = bot;
    var b: Bot  = bot(0).1.foo;
    unit
```

## Union and intersection types

Clam features union and intersection types, which allow to represent set-theoric operations between types.

```
type Union = {foo: Int} | {foo: String}
type Inter = {bar: Int} & {baz: String}

def union: Union = {foo = 1}
def inter: Inter = {bar = 2, baz = "World"}

def foo: Int | String = union.foo
```

Clam's union and intersections also support distributivity, joins and meets.

```
def distrib = [A, B, C] -> (p: (A & B) | (A & C)) ->
    var foo: A & (B | C) = p;
    unit

def meet = [A, B, C] -> (p: ((A) -> C) & ((B) -> C)) ->
    var bar: (A | B) -> C = p;
    unit
```

## Universal types

Clam features universal types, which allow expressions to abstract over types.

```
type ApplyTwice = [T] -> ((T) -> T, T) -> T

def apply_twice: ApplyTwice = [T] -> (f, v) ->
    f(f(v))

def eight = apply_twice[Int]((i) -> i * 2, 2)
```

Type parameters have a lower and an upper bound, which are declared as an interval using `..` and are respectively `Bot` and `Top` by default. The bounds of a type parameter must be consistent, that is, the lower bound must be a subtype of the upper bound, and universal types are currently invariant with regards to these bounds.

```
def max = [T: .. Int] -> (a: T, b: T) ->
    if a > b then a else b
```

Clam also features higher-rank polymorphism subtyping, which allow to use universal types in a more flexible way.

```
def id: [T] -> (T) -> T = (p) -> p
def int_id: (Int) -> Int = id
def single: [T] -> Int = 0
```

## Type constructors

Clam features type constructors, which allow types to abstract over other types.

```
type Pair = [T] => {T, T}

def pair: Pair[Int] = {0, 0}
```

## Higher-kinded types

Clam features higher-kinded types, which allow type constructors to abstract over other type constructors.

```
type Monad = [M: .. [T] => Top, A] => {
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

Whenever a type parameter only has one declared bound, the other bound is inferred to the corresponding extremal type of the same kind. Also, since a type can only have one kind, all the types that compose a union or intersection must be of the same kind. Finally, like univeral types, higher-kinded types are currently invariant with regards to their parameter bounds.

## Type inference

Clam features a constraint-based type inference algorithm capable of inferring principal types for most expressions.

<table>
    <tr>
        <td>Declared expressions</td>
        <td>Inferred types</td>
    </tr>
    <tr>
<td><pre><code>def two = 1 + 1
def id = (p) -> p
def foo = (f) -> {f(123), f("Hello")}
def bar = foo(id)
def max = (l, r) -> if l > r then l else r
def infinite = (p) -> infinite(p)
def impredicative = id(id)
def recursive = (p) -> recursive
def not_ml = {id_bis = (p) -> p, z = 0}
def is_even = (n) -> !is_odd(n)
def is_odd = (n) -> if n == 0
             then false else !is_even(n - 1)</code></pre></td>
<td><pre><code>two: Int
id: ['A] -> ('A) -> 'A
foo: ['A, 'B] -> (((Int) -> 'A) & ((String) -> 'B)) -> {'A, 'B}
bar: {Int, String}
max: ['A: .. Int] -> ('A, 'A) -> 'A
infinite: (Top) -> Bot
impredicative: ['A] -> ('A) -> 'A
recursive: 'A. (Top) -> 'A
not_ml: {id_bis: ['A] -> ('A) -> 'A, z: Int}
is_even: (Int) -> Bool
is_odd: (Int) -> Bool
​</code></pre></td>
    </tr>
</table>

As shown by these examples, Clam's type inference algorithm supports both impredicative polymorphism, subtyping, and recursive types. Although it has not been formalized yet and would benefit from a little code polishing, practical tests show that it peforms similarly to the world's most advanced algorithms, although its type simplification could be improved. Comparisons against the [SuperF paper](https://dl.acm.org/doi/10.1145/3632890) (from which this algorithm took some inspiration from) can be found in the `tests/infer/compare.clam` test.

## Recursive types

Although Clam's type inference algorithm is capable of infering recursive types, Clam does not support this feature yet.

# Correctness

There is currently no mathematical proof of correctness for this language or interpeter, but I wish to write one one day. While the code may still be rough in some places, all the examples provided work, as well as all the unit tests and the sample programs found in the `tests` directory.

# Roadmap

I will have less time to work on this project in the short- to medium-term future, however, here is my roadmap for Clam:
- Polish the interpreter's code and algorithms.
- Improve error handling and user experience.
- Add recursive types.
- Add negation types.
- Add pattern matching through disjoint switches.
- Add other cool stuff (function totality checking, dependent types... unfortunately very far away)

# Notes

Clam is a pet project of mine I created during my master's thesis evaluation to apply the knowledge I gained on programming language theory and practice functional programming. It has since gained in size and functionality but is still not intended to be a used for serious programming.
