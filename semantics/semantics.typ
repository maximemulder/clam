#import "template.typ": *
#import "@preview/curryst:0.2.0"

#show: project.with(
  title: "Clam semantics",
  authors: (
    "Maxime Mulder",
  ),
)

#show link: underline

#show heading: title => [ #title \ ]

#let logic(body) = par(justify: false, align(center, body))

#let defrule(name, conclusion, ..args) = curryst.rule(name: smallcaps(name), conclusion, ..args)

#let tree(rule) = curryst.proof-tree(rule)

#let axiom(name, ..args) = box(pad(rest: 8pt, tree(
  defrule(name, ..args))
))

#let rule(name) = smallcaps(name)

#let grammar(content) = box(pad(rest: 8pt, content))

#let Unit   = raw("Unit")
#let Bool   = raw("Bool")
#let Int    = raw("Int")
#let String = raw("String")

#let interval(lower, upper) = $lower space .. space upper$

#let tuniv(bind, lower, upper, ret) = $forall bind : interval(lower, upper). space ret$

#let tabs(bind, lower, upper, body) = $Lambda bind : interval(lower, upper). space body$

= Introduction

This document is the work-in-progress formalization of the Clam programming language. It covers the semantics of the core calculus used by Clam, which is based on System $F^omega_(<:)$ enriched with a few basic data types, unions and intersections.

This document is not yet complete. For now, it only specifies declarative rules and lacks associated theorems and proofs.

This document has been written using #link("https://typst.app")[Typst].

#outline()

#pagebreak()

= Grammar

The abstract syntax of Clam is given by the following grammar:

#logic[
  #grammar($u ::= & #raw("unit")$)
  #grammar($b ::= & #raw("true") | #raw("false")$)
  #grammar($n ::= & 0 | 1 | ...$)
  #grammar($s ::= & \"...\"$)
  \
  #grammar($
    "Expression"
    e ::= & u && space.quad "unit" \
        | & b && space.quad "boolean"\
        | & n && space.quad "integer"\
        | & s && space.quad "string" \
        | & x && space.quad "variable" \
        | & angle.l e_1, ..., e_n angle.r && space.quad "tuple" \
        | & e.n && space.quad "tuple projection" \
        | & angle.l l_1 = e_1, ..., l_n = e_n angle.r && space.quad "record" \
        | & e.l_n && space.quad "record projection" \
        | & lambda x : tau. space e && space.quad "lambda abstraction" \
        | & e(e) && space.quad "lambda application" \
        | & lambda t : interval(tau, tau). space e && space.quad "universal abstraction" \
        | & e[tau] && space.quad "universal application" \
        | & e : tau && space.quad "type ascription" \
  \ \
    "Type"
    tau ::= & top && space.quad "top" \
          | & bot && space.quad "bottom" \
          | & tau union tau && space.quad "union" \
          | & tau sect tau && space.quad "intersection" \
          | & #Unit && space.quad "unit" \
          | & #Bool && space.quad "boolean" \
          | & #Int && space.quad "integer" \
          | & #String && space.quad "string" \
          | & t && space.quad "variable"\
          | & angle.l tau_1, ..., tau_n angle.r && space.quad "tuple" \
          | & angle.l l_1: tau_1, ..., l_n: tau_n angle.r && space.quad "record" \
          | & tau -> tau && space.quad "lambda abstraction" \
          | & tuniv(t, tau, tau, tau) && space.quad "universal abstraction" \
          | & tabs(t, tau, tau, tau) && space.quad "type abstraction" \
          | & tau[tau] && space.quad "type application" $)
]

#pagebreak()

= Kinding

The syntax of kinds is given by the following grammar:

#logic[
  #grammar($
    "Kind"
    k ::= & *                       && space.quad "type" \
        | & interval(tau, tau) -> k && space.quad "arrow" $)
]

The kinding judgement is of the form $Delta tack.r tau :: k$. The hypotheses of $Delta$ are of the form $t: interval(tau_1, tau_2)$, where $tau_1$ and $tau_2$ represent respectively the lower and the upper bounds of a type variable.

#logic[
  #axiom("K-Unit",
    $Unit :: *$)
  #axiom("K-Bool",
    $#Bool :: *$)
  #axiom("K-Int",
    $#Int :: *$)
  #axiom("K-String",
    $#String :: *$)
  \
  #axiom("K-Top",
    $top :: *$)
  #axiom("K-Bot",
    $bot :: *$)
  #axiom("K-Var",
    $Delta tack.r t :: k$,
    $t : interval(tau_1, tau_2) in Delta$,
    $Delta tack.r tau_1 :: k$)
  \
  #axiom("K-Tuple",
    $Delta tack.r angle.l tau_1, ..., tau_n angle.r :: *$,
    $Delta tack.r tau_1 :: * space.quad ... space.quad Delta tack.r tau_n :: *$)
  #axiom("K-Record",
    $Delta tack.r angle.l l_1 : tau_1, ..., l_n : tau_n angle.r :: *$,
    $Delta tack.r tau_1 :: * space.quad ... space.quad Delta tack.r tau_n :: *$)
  \
  #axiom("K-Lam",
    $Delta tack.r tau_1 -> tau_2 :: *$,
    $Delta tack.r tau_1 :: *$,
    $Delta tack.r tau_2 :: *$)
  #axiom("K-Univ",
    $Delta tack.r tuniv(t, tau_1, tau_2, tau_3) :: *$,
    $Delta tack.r tau_1 <= tau_2$,
    $Delta, t : interval(tau_1, tau_2) tack.r tau_3 :: *$)
  \
  #axiom("K-Abs",
    $Delta tack.r tabs(t, tau_1, tau_2, tau_3) :: interval(tau_1, tau_2) -> k$,
    $Delta tack.r tau_1 <= tau_2$,
    $Delta, t : interval(tau_1, tau_2) tack.r tau_3 :: k$)
  \
  #axiom("K-App",
    $Delta tack.r tau_1[tau_4] :: k$,
    $Delta tack.r tau_1 :: interval(tau_2, tau_3) -> k$,
    $Delta tack.r tau_2 <= tau_4$,
    $Delta tack.r tau_4 <= tau_3$)
  \
  #axiom("K-Union",
    $Delta tack.r tau_1 union tau_2 :: k$,
    $Delta tack.r tau_1 :: k$,
    $Delta tack.r tau_2 :: k$)
  #axiom("K-Inter",
    $Delta tack.r tau_1 sect tau_2 :: k$,
    $Delta tack.r tau_1 :: k$,
    $Delta tack.r tau_2 :: k$)
]

Rules #rule("K-Univ") and #rule("K-Abs") prohibit inconsistent bounds in universal types and type abstractions.

#pagebreak()

= Type equivalence

The type equivalence judgement is of the form $Delta tack.r tau equiv tau'$.  The following rules describe the reflexivity, symmetry and transitivity properties of the type equivalence relation.

#logic[
  #axiom("E-Refl",
    $tau equiv tau$)
  #axiom("E-Symm",
    $Delta tack.r tau_2 equiv tau_1$,
    $Delta tack.r tau_1 equiv tau_2$)
  #axiom("E-Trans",
    $Delta tack.r tau_1 equiv tau_3$,
    $Delta tack.r tau_1 equiv tau_2$,
    $Delta tack.r tau_2 equiv tau_3$)
]

The following rules describe type equivalence for composite data types.

#logic[
  #axiom("E-Tuple",
    $Delta tack.r angle.l tau_1, ..., tau_n angle.r equiv angle.l tau'_1, ..., tau'_n angle.r$,
    $Delta tack.r tau_1 equiv tau'_1 space.quad ... space.quad Delta tack.r tau_n equiv tau'_n$)
  \
  #axiom("E-Record",
    $Delta tack.r angle.l l_1 : tau_1, ..., l_n : tau_n angle.r equiv angle.l l_1 : tau'_1, ..., l_n : tau'_n angle.r$,
    $Delta tack.r tau_1 equiv tau'_1 space.quad ... space.quad Delta tack.r tau_n equiv tau'_n$)
  \
  #axiom("E-Abs",
    $Delta tack.r tau_1 -> tau_2 equiv tau'_1 -> tau'_2$,
    $Delta tack.r tau_1 equiv tau'_1$,
    $Delta tack.r tau_2 equiv tau'_2$)
  #axiom("E-Univ",
    $Delta tack.r tuniv(t, tau_1, tau_2, tau_3) equiv tuniv(t, tau'_1, tau'_2, tau'_3)$,
    $Delta tack.r tau_1 equiv tau'_1$,
    $Delta tack.r tau_2 equiv tau'_2$,
    $Delta tack.r tau_3 equiv tau'_3$)
  \
  #axiom("E-TAbs",
    $Delta tack.r tabs(t, tau_1, tau_2, tau_3) equiv tabs(t, tau'_1, tau'_2, tau'_3)$,
    $Delta tack.r tau_1 equiv tau'_1$,
    $Delta tack.r tau_2 equiv tau'_2$,
    $Delta tack.r tau_3 equiv tau'_3$)
  #axiom("E-TApp",
    $Delta tack.r tau_1[tau_2] equiv tau'_1[tau'_2]$,
    $Delta tack.r tau_1 equiv tau'_1$,
    $Delta tack.r tau_2 equiv tau'_2$)
  #axiom("E-TAbsApp",
    $(tabs(t, tau_1, tau_2, tau_3))[tau_4] equiv [t slash tau_3]tau_2$)
]

#let note = [#"" What about the absorbtion law ?]

The following rules describe the commutativity, associativity, distribution and inclusion properties of union and intersection types#footnote(note).

#logic[
  #axiom("E-UnionComm",
    $tau_1 union tau_2 equiv tau_2 union tau_1$)
  #axiom("E-InterComm",
    $tau_1 sect tau_2 equiv tau_2 sect tau_1$)
  \
  #axiom("E-UnionAssoc",
    $(tau_1 union tau_2) union tau_3 equiv tau_1 union (tau_2 union tau_3)$)
  #axiom("E-InterAssoc",
    $(tau_1 sect tau_2) sect tau_3 equiv tau_1 sect (tau_2 sect tau_3)$)
  \
  #axiom("E-UnionDistrib",
    $tau_1 union (tau_2 sect tau_3) equiv (tau_1 union tau_2) sect (tau_1 union tau_3)$)
  #axiom("E-InterDistrib",
    $tau_1 sect (tau_2 union tau_3) equiv (tau_1 sect tau_2) union (tau_1 sect tau_3)$)
  \
  #axiom("E-UnionIncl",
    $Delta tack.r tau_1 union tau_2 equiv tau_2$,
    $Delta tack.r tau_1 <= tau_2$)
  #axiom("E-InterIncl",
    $Delta tack.r tau_1 equiv tau_1 sect tau_2$,
    $Delta tack.r tau_1 <= tau_2$)
]

#pagebreak()

The following rules describe the distributivity of intersection types over composite data types.

#logic[
  #axiom("E-MeetTuple",
    $angle.l tau_1, ..., tau_n angle.r sect angle.l tau'_1, ..., tau'_n angle.r equiv angle.l tau_1 sect tau'_1, ..., tau_n sect tau'_n angle.r$)
  \
  #axiom("E-MeetRecord",
    $ angle.l l_1 : tau_1, ..., l_n : tau_n, l''_1 : tau''_1, ..., l''_n : tau''_n angle.r sect angle.l l_1 : tau'_1, ..., l_n : tau'_n, l'''_1 : tau'''_1, ..., l'''_n : tau'''_n angle.r \ equiv \ angle.l l_1 : tau_1 sect tau'_1, ..., l_n : tau_n sect tau'_n, l''_1 : tau''_1, ..., l''_n : tau''_n, l'''_1 : tau'''_1, ..., l'''_n : tau'''_n angle.r $)
  #axiom("E-MeetAbs",
    $ (tau_1 -> tau_2) sect (tau'_1 -> tau'_2) equiv tau_1 union tau'_1 -> tau_2 sect tau'_2 $)
  \
  #axiom("E-MeetUniv",
    $Delta tack.r (tuniv(t, tau_1, tau_2, tau_3)) sect (tuniv(t, tau'_1, tau'_2, tau'_3)) equiv tuniv(t, tau_1, tau_2, tau_3) sect tau'_3$,
    $ Delta tack.r tau_1 equiv tau'_1 $,
    $ Delta tack.r tau_2 equiv tau'_2 $,
    $ Delta tack.r tau_3 equiv tau'_3 $)
  #axiom("E-MeetTAbs",
    $Delta tack.r (tabs(t, tau_1, tau_2, tau_3)) sect (tabs(t, tau'_1, tau'_2, tau'_3)) equiv tabs(t, tau_1, tau_2, tau_3 sect tau'_3)$,
    $ Delta tack.r tau_1 equiv tau'_1 $,
    $ Delta tack.r tau_2 equiv tau'_2 $,
    $ Delta tack.r tau_3 equiv tau'_3 $)
]

TODO: Check meet rules.

#pagebreak()

= Subtyping

The subtyping judgement is of the form $Delta tack.r tau <= tau'$.

#logic[
  #axiom("S-Eq",
    $Delta tack.r tau_1 <= tau_2$,
    $Delta tack.r tau_1 equiv tau_2$)
  #axiom("S-Trans",
    $Delta tack.r tau_1 <= tau_3$,
    $Delta tack.r tau_1 <= tau_2$,
    $Delta tack.r tau_2 <= tau_3$)
  \
  #axiom("S-VarSub",
    $Delta tack.r tau_1 <= t $,
    $t: interval(tau_1, tau_2) in Delta$)
  #axiom("S-VarSup",
    $Delta tack.r t <= tau_2$,
    $t: interval(tau_1, tau_2) in Delta$)
  \
  #axiom("S-Top",
    $Delta tack.r tau <= top$,
    $Delta tack.r tau :: *$)
  #axiom("S-Bot",
    $Delta tack.r bot <= tau$,
    $Delta tack.r tau :: *$)
  \
  #axiom("S-Tuple",
    $Delta tack.r angle.l tau_1, ..., tau_n angle.r <= angle.l tau'_1, ..., tau'_n angle.r$,
    $Delta tack.r tau_1 <= tau'_1 space.quad ... space.quad Delta tack.r tau_n <= tau'_n$)
  \
  #axiom("S-Record",
    $Delta tack.r angle.l l_1 : tau_1, ..., l_m : tau_m angle.r <= angle.l l_1 : tau'_1, ..., l_n : tau'_n angle.r$,
    $Delta tack.r tau_1 <= tau'_1 space.quad ... space.quad Delta tack.r tau_n <= tau'_n$,
    $n lt.eq.slant m$)
  \
  #axiom("S-Abs",
    $Delta tack.r tau_1 -> tau_2 <: tau'_1 -> tau'_2$,
    $Delta tack.r tau'_1 <= tau_1$,
    $Delta tack.r tau_2 <= tau'_2$)
  \
  #axiom("S-UnivL",
    $Delta tack.r tuniv(t, tau_1, tau_2, tau_3) <= tau_4$,
    $Delta tack.r tau_1 <= tau$,
    $Delta tack.r tau <= tau_2$,
    $Delta tack.r [t slash tau]tau_3 <= tau_4$)
  #axiom("S-UnivR",
    $Delta tack.r tau <= tuniv(t,tau_1, tau_2, tau_3)$,
    $Delta, t: interval(tau_1, tau_2) tack.r tau <=  tau_3$)
  \
  #axiom("S-TAbs",
    $Delta tack.r tabs(t, tau_1, tau_2, tau_3) <= tabs(t, tau'_1, tau'_2, tau'_3)$,
    $Delta tack.r interval(tau_1, tau_2) equiv interval(tau'_1, tau'_2)$,
    $Delta tack.r tau_3 <= tau'_3$)
  #axiom("S-TApp",
    $Delta tack.r tau_1[tau_2] <= tau'_1[tau'_2]$,
    $Delta tack.r tau_1 <= tau'_1$,
    $Delta tack.r tau_2 equiv tau'_2$)
  \
  #axiom("S-Union1",
    $Delta tack.r tau <= tau_1 union tau_2$,
    $Delta tack.r tau <= tau_1$)
  #axiom("S-Inter2",
    $Delta tack.r tau_1 sect tau_2 <= tau$,
    $Delta tack.r tau_1 <= tau$)
  \
  #axiom("S-Union1",
    $Delta tack.r tau_1 union tau_2 <= tau$,
    $Delta tack.r tau_1 <= tau$, $Delta tack.r tau_2 <= tau$)
  #axiom("S-Inter2",
    $Delta tack.r tau <= tau_1 sect tau_2$,
    $Delta tack.r tau <= tau_1$,
    $Delta tack.r tau <= tau_2$)
]

Rules #rule("S-UnivL") and #rule("S-UnivR") correspond to the higher-rank polymrohism subtyping relation by Odersky and Läufer @OderskyLaufer1996.

#pagebreak()

= Typing

The typing judgement is of the form $Delta space Gamma tack.r e : tau$. The hypotheses of $Gamma$ are of the form $x: tau$.

#logic[
  #axiom("T-Var",
    $Gamma tack.r x : tau$,
    $x : tau in Gamma$)
  #axiom("T-Sub",
    $Delta space Gamma tack.r e : tau'$,
    $Delta space Gamma tack.r e : tau$,
    $Delta tack.r tau <= tau'$)
  \
  #axiom("T-Unit",
    $u : #Unit$)
  #axiom("T-Bool",
    $b : #Bool$)
  #axiom("T-Int",
    $i : #Int$)
  #axiom("T-String",
    $s : #String$)
  \
  #axiom("T-Tuple",
    $Delta space Gamma tack.r angle.l e_1, ... e_n angle.r : angle.l tau_1, ..., tau_n angle.r$,
    $Delta space Gamma tack.r e_1 : tau_1 space.quad ... space.quad Delta space Gamma tack.r e_n : tau_n$)
  #axiom("T-TupleProj",
    $Delta space Gamma tack.r e.i : tau_i$,
    $Delta space Gamma tack.r e: angle.l tau_1, ..., tau_n angle.r$)
  \
  #axiom("T-Record",
    $Delta space Gamma tack.r angle.l l_1= e_1, ...,  l_n = e_n angle.r : angle.l l_1 : tau_1, ..., l_n : tau_n angle.r$,
    $Delta space Gamma tack.r e_1 : tau_1 space.quad ... space.quad Delta space Gamma tack.r e_n : tau_n$)
  \
  #axiom("T-RecordProj",
    $Delta space Gamma tack.r e.l_i : tau_i$,
    $Delta space Gamma tack.r e: angle.l l_1 : tau_1, ..., l_n : tau_n angle.r$)
  \
  #axiom("T-Abs",
    $Delta space Gamma tack.r lambda x : tau_1. space e : tau_1 -> tau_2$,
    $Delta tack.r tau_1 :: *$,
    $Delta space Gamma, x : tau_1 tack.r e : tau_2$)
  #axiom("T-AbsApp",
    $Delta space Gamma tack.r e_1(e_2) : tau_2$,
    $Delta space Gamma tack.r e_1 : tau_1 -> tau_2$,
    $Delta space Gamma tack.r e_2 : tau_1$)
  \
  #axiom("T-Univ",
    $Delta space Gamma tack.r lambda t : interval(tau_1, tau_2). space e : tuniv(t, tau_1, tau_2, tau_3)$,
    $Delta tack.r tau_1 <= tau_2$,
    $Delta, t : interval(tau_1, tau_2) space Gamma tack.r e : tau_3$)
  #axiom("T-UnivApp",
    $Delta space Gamma tack.r e[tau_3] : [t slash tau]tau_3$,
    $Delta space Gamma tack.r e : tuniv(t, tau_1, tau_2, tau_3)$,
    $Delta tack.r tau_1 <= tau$,
    $Delta tack.r tau <= tau_2$)
  \
  #axiom("T-Ascr",
    $Delta space Gamma tack.r e : tau : tau$,
    $Delta tack.r tau :: *$,
    $Delta space Gamma tack.r e : tau$)
]

TODO: Fix #rule("T-UnivApp").

#pagebreak()

= Evaluation

The syntax of values is given by the following grammar:

#logic[
  #grammar($
    "Value"
    v ::= & u && space.quad "unit" \
        | & b && space.quad "boolean" \
        | & i && space.quad "integer" \
        | & s && space.quad "string" \
        | & angle.l v_1, ..., v_n angle.r && space.quad "tuple" \
        | & angle.l l_1 = v_1, ..., l_n = v_n angle.r && space.quad "record" \
        | & lambda x. space e && space.quad "lambda abstraction" $)
]

The evaluation judgement is of the form $e arrow.b.double v$.

#logic[
  #axiom("V-Unit",
    $u arrow.b.double u$)
  #axiom("V-Bool",
    $b arrow.b.double b$)
  #axiom("V-Int",
    $n arrow.b.double i$,
    $n = i$)
  #axiom("V-String",
    $s arrow.b.double s$)
  \
  #axiom("V-Tuple",
    $angle.l e_1, ... e_n angle.r arrow.b.double angle.l v_1, ..., v_n angle.r$,
    $e_1 arrow.b.double v_1 space.quad ... space.quad e_n arrow.b.double v_n$)
  #axiom("V-TupleProj",
    $e.i arrow.b.double v_i$,
    $e arrow.b.double angle.l v_1, ..., v_n angle.r$)
  \
  #axiom("V-Record",
    $angle.l l_1 = e_1, ...,  l_n = e_n angle.r arrow.b.double angle.l l_1 = v_1, ..., l_n = v_n angle.r$,
    $e_1 arrow.b.double v_1 space.quad ... space.quad e_n arrow.b.double v_n$)
  \
  #axiom("V-RecordProj",
    $e.l_i arrow.b.double v_i$,
    $e arrow.b.double angle.l l_1 = v_1, ..., l_n = v_n angle.r$)
  \
  #axiom("V-Abs",
    $lambda x : tau. space e arrow.b.double lambda x. space e$)
  #axiom("V-AbsApp",
    $e_1(e_2) arrow.b.double v$,
    $e_1 arrow.b.double lambda x. space e_3$,
    $[e_2 slash x]e_3 arrow.b.double v$)
  \
  #axiom("V-Univ",
    $lambda t : interval(tau_1, tau_2). space e arrow.b.double v$,
    $e arrow.b.double v$)
  #axiom("V-UnivApp",
    $e[tau] arrow.b.double v$,
    $e arrow.b.double v$)
  \
  #axiom("V-Ascr",
    $e : tau arrow.b.double v$,
    $e arrow.b.double v$)
]

#pagebreak()

= Primitives

The types of the primitive values of Clam are given by the following table:

#let note = [#"" `if` is not implemented as a primitive value yet.]

#align(center, [
  #table(
    columns: (auto, auto),
    stroke: none,
    align: left,
    column-gutter: 4pt,
    [*Primitive*], [*Type*],
    [`+` #math.italic("(unary)")], $#Int -> #Int$,
    [`-` #math.italic("(unary)")], $#Int -> #Int$,
    `!`, $#Bool -> #Bool$,
    [`+` #math.italic("(binary)")], $#Int -> #Int -> #Int$,
    [`-` #math.italic("(binary)")], $#Int -> #Int -> #Int$,
    `*`, $#Int -> #Int -> #Int$,
    `/`, $#Int -> #Int -> #Int$,
    `%`, $#Int -> #Int -> #Int$,
    `++`, $#String -> #String -> #String$,
    `==`, $top -> top -> #Bool$,
    `!=`, $top -> top -> #Bool$,
    `<`, $#Int -> #Int -> #Bool$,
    `>`, $#Int -> #Int -> #Bool$,
    `<=`, $#Int -> #Int -> #Bool$,
    `>=`, $#Int -> #Int -> #Bool$,
    `|`, $#Bool -> #Bool -> #Bool$,
    `&`, $#Bool -> #Bool -> #Bool$,
    [`if`#footnote(note)], $forall t. space #Bool -> t -> t -> t$,
  )
])

#bibliography("bibliography.bib")
