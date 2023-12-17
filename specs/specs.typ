#import "template.typ": *
#import "@preview/curryst:0.1.0"

#show: project.with(
  title: "Clam semantics",
  authors: (
    "Maxime Mulder",
  ),
)

#show link: underline

#show heading: title => [ #title \ ]

#let rules(body) = par(justify: false, align(center, body))

#let rule(name, ..args) = box(pad(rest: 8pt, curryst.proof-tree(
  curryst.rule(name: smallcaps(name), ..args))
))

#let grammar(content) = box(pad(rest: 8pt, content))

#let Unit   = raw("Unit")
#let Bool   = raw("Bool")
#let Int    = raw("Int")
#let String = raw("String")

= Introduction

This document is the work-in-progress specification of the Clam programming language semantics. It is mostly based on the semantics of System $F^omega_(<:)$ and captures most of the behaviours of Clam. However, it is not yet complete and lacks proofs. It could also benefit from more elegant evaluation semantics.

This document has been written using #link("https://typst.app")[Typst].

#outline()

#pagebreak()

= Grammar

The abstract syntax of Clam is given by the following grammar:

#rules([
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
        | & e.l && space.quad "record projection" \
        | & lambda x : tau. space e && space.quad "lambda abstraction" \
        | & e(e) && space.quad "lambda application" \
        | & lambda T <: tau. space e && space.quad "universal abstraction" \
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
          | & forall t <: tau. space tau && space.quad "universal abstraction" \
          | & Lambda t <: tau. space tau && space.quad "type abstraction" \
          | & tau[tau] && space.quad "type application" $)
])

#pagebreak()

= Kinding

The syntax of kinds is given by the following grammar:

#rules([
  #grammar($
    "Kind"
    k ::= & * && space.quad "type" \
        | & k -> k && space.quad "arrow" $)
])

The kinding judgement is of the form $Delta tack.r tau :: k$. The hypotheses of $Delta$ are of the form $t <: tau$.

#rules([
  #rule("K-Unit",
    $Unit :: *$)
  #rule("K-Bool",
    $#Bool :: *$)
  #rule("K-Int",
    $#Int :: *$)
  #rule("K-String",
    $#String :: *$)
  \
  #rule("K-Top",
    $top :: *$)
  #rule("K-Bot",
    $bot :: *$)
  #rule("K-Var",
    $Delta tack.r t :: K$,
    $t <: tau in Delta$,
    $Delta tack.r tau :: K$)
  \
  #rule("K-Tuple",
    $Delta tack.r angle.l tau_1, ..., tau_n angle.r :: *$,
    $Delta tack.r tau_1 :: * space.quad ... space.quad Delta tack.r tau_n :: *$)
  #rule("K-Record",
    $Delta tack.r angle.l l_1 : tau_1, ..., l_n : tau_n angle.r :: *$,
    $Delta tack.r tau_1 :: * space.quad ... space.quad Delta tack.r tau_n :: *$)
  \
  #rule("K-Abs",
    $Delta tack.r tau_1 -> tau_2 :: *$,
    $Delta tack.r tau_1 :: *$,
    $Delta tack.r tau_2 :: *$)
  #rule("K-Univ",
    $Delta tack.r forall t <: tau_1. space tau_2 :: *$,
    $Delta, t <: tau_1 tack.r tau_2 :: *$)
  \
  #rule("K-TAbs",
    $Delta tack.r Lambda T <: tau_1. space tau_2 :: k_1 -> k_2$,
    $Delta tack.r tau_1 :: k_1$,
    $Delta, T <: tau_1 tack.r tau_2 :: k_2$)
  \
  #rule("K-TAbsApp",
    $Delta tack.r tau_1[tau_4] :: k$,
    $Delta tack.r tau_1 <: Lambda t <: tau_2. -> tau_3$,
    $Delta tack.r tau_3 :: k$,
    $Delta tack.r tau_4 <: tau_2$)
  \
  #rule("K-Union",
    $Delta tack.r tau_1 union tau_2 :: k$,
    $Delta tack.r tau_1 :: k$,
    $Delta tack.r tau_2 :: k$)
  #rule("K-Inter",
    $Delta tack.r tau_1 sect tau_2 :: k$,
    $Delta tack.r tau_1 :: k$,
    $Delta tack.r tau_2 :: k$)
])

#pagebreak()

= Type equivalence

The type equivalence judgement is of the form $Delta tack.r tau equiv tau'$. The following rules describe the reflexivity, symmetry and transitivity properties of the type equivalence relation.

#rules([
  #rule("E-Refl",
    $tau equiv tau$)
  #rule("E-Symm",
    $Delta tack.r tau_2 equiv tau_1$,
    $Delta tack.r tau_1 equiv tau_2$)
  #rule("E-Trans",
    $Delta tack.r tau_1 equiv tau_3$,
    $Delta tack.r tau_1 equiv tau_2$,
    $Delta tack.r tau_2 equiv tau_3$)
])

The following rules describe type equivalence for data types.

#rules([
  \
  #rule("E-Tuple",
    $Delta tack.r angle.l tau_1, ..., tau_n angle.r equiv angle.l tau'_1, ..., tau'_n angle.r$,
    $Delta tack.r tau_1 equiv tau'_1 space.quad ... space.quad Delta tack.r tau_n equiv tau'_n$)
  \
  #rule("E-Record",
    $Delta tack.r angle.l l_1 : tau_1, ..., l_n : tau_n angle.r equiv angle.l l_1 : tau'_1, ..., l_n : tau'_n angle.r$,
    $Delta tack.r tau_1 equiv tau'_1 space.quad ... space.quad Delta tack.r tau_n equiv tau'_n$)
  \
  #rule("E-Abs",
    $Delta tack.r tau_1 -> tau_2 equiv tau'_1 -> tau'_2$,
    $Delta tack.r tau_1 equiv tau'_1$,
    $Delta tack.r tau_2 equiv tau'_2$)
  #rule("E-Univ",
    $Delta tack.r forall t <: tau_1. space tau_2 equiv forall t <: tau'_1. space tau'_2$,
    $Delta tack.r tau_1 equiv tau'_1$,
    $Delta tack.r tau_2 equiv tau'_2$)
  \
  #rule("E-TAbs",
    $Delta tack.r Lambda T <: tau_1. space tau_2 equiv Lambda t <: tau'_1. space tau'_2$,
    $Delta tack.r tau_1 equiv tau'_1$,
    $Delta tack.r tau_2 equiv tau'_2$)
  #rule("E-TApp",
    $Delta tack.r tau_1[tau_2] equiv tau'_1[tau'_2]$,
    $Delta tack.r tau_1 equiv tau'_1$,
    $Delta tack.r tau_2 equiv tau'_2$)
  #rule("E-TAbsApp",
    $(Lambda t <: tau_1. space tau_2)[tau_3] equiv [t slash tau_3]tau_2$)
])

The following rules describe the commutativity, associativity, distribution and inclusion properties of union and intersection types.

#rules([
  #rule("E-UnionComm",
    $tau_1 union tau_2 equiv tau_2 union tau_1$)
  #rule("E-InterComm",
    $tau_1 sect tau_2 equiv tau_2 sect tau_1$)
  \
  #rule("E-UnionAssoc",
    $(tau_1 union tau_2) union tau_3 equiv tau_1 union (tau_2 union tau_3)$)
  #rule("E-InterAssoc",
    $(tau_1 sect tau_2) sect tau_3 equiv tau_1 sect (tau_2 sect tau_3)$)
  \
  #rule("E-UnionDistrib",
    $tau_1 union (tau_2 sect tau_3) equiv (tau_1 union tau_2) sect (tau_1 union tau_3)$)
  #rule("E-InterDistrib",
    $tau_1 sect (tau_2 union tau_3) equiv (tau_1 sect tau_2) union (tau_1 sect tau_3)$)
  \
  #rule("E-UnionIncl",
    $Delta tack.r tau_1 union tau_2 equiv tau_2$,
    $Delta tack.r tau_1 <: tau_2$)
  #rule("E-InterIncl",
    $Delta tack.r tau_1 equiv tau_1 sect tau_2$,
    $Delta tack.r tau_1 <: tau_2$)
  \
  Note: What about the absorbtion law ?
])

#pagebreak()

The following rules describe the distributivity of intersection types over data types.

#rules([
  #rule("E-MeetTuple",
    $angle.l tau_1, ..., tau_n angle.r sect angle.l tau'_1, ..., tau'_n angle.r equiv angle.l tau_1 sect tau'_1, ..., tau_n sect tau'_n angle.r$)
  \
  #rule("E-MeetRecord",
    $ angle.l l_1 : tau_1, ..., l_n : tau_n, l''_1 : tau''_1, ..., l''_n : tau''_n angle.r sect angle.l l_1 : tau'_1, ..., l_n : tau'_n, l'''_1 : tau'''_1, ..., l'''_n : tau'''_n angle.r \ equiv \ angle.l l_1 : tau_1 sect tau'_1, ..., l_n : tau_n sect tau'_n, l''_1 : tau''_1, ..., l''_n : tau''_n, l'''_1 : tau'''_1, ..., l'''_n : tau'''_n angle.r $)
  #rule("E-MeetAbs",
    $(tau_1 -> tau_2) sect (tau'_1 -> tau'_2) equiv tau_1 union tau'_1 -> tau_2 sect tau'_2$)
  \
  #rule("E-MeetUniv",
    $ Delta tack.r (forall t <: tau_1. space tau_2) sect (forall t <: tau_1. space tau'_2) equiv forall t <: tau_1. space tau_2 sect tau'_2 $,
    $Delta tack.r tau_1 equiv tau'_1$)
  #rule("E-MeetTAbs",
    $Delta tack.r (Lambda t <: tau_1. space tau_2) sect (Lambda t <: tau_1. space tau'_2) equiv Lambda t <: tau_1. space tau_2 sect tau'_2$,
    $ Delta tack.r tau_1 equiv tau'_1$)
])

#pagebreak()

= Subtyping

The subtyping judgement is of the form $Delta tack.r tau <: tau'$.

#rules([
  #rule("S-Eq",
    $Delta tack.r tau_1 <: tau_2$,
    $Delta tack.r tau_1 equiv tau_2$)
  #rule("S-Trans",
    $Delta tack.r tau_1 <: tau_3$,
    $Delta tack.r tau_1 <: tau_2$,
    $Delta tack.r tau_2 <: tau_3$)
  #rule("S-Var",
    $Delta tack.r t <: tau$,
    $t <: tau in Delta$)
  \
  #rule("S-Top",
    $Delta tack.r tau <: top$,
    $Delta tack.r tau :: *$)
  #rule("S-Bot",
    $Delta tack.r bot <: tau$,
    $Delta tack.r tau :: *$)
  \
  #rule("S-Tuple",
    $Delta tack.r angle.l tau_1, ..., tau_n angle.r <: angle.l tau'_1, ..., tau'_n angle.r$,
    $Delta tack.r tau_1 <: tau'_1 space.quad ... space.quad Delta tack.r tau_n <: tau'_n$)
  \
  #rule("S-Record",
    $Delta tack.r angle.l l_1 : tau_1, ..., l_m : tau_m angle.r <: angle.l l_1 : tau'_1, ..., l_n : tau'_n angle.r$,
    $Delta tack.r tau_1 <: tau'_1 space.quad ... space.quad Delta tack.r tau_n <: tau'_n$,
    $n lt.eq.slant m$)
  \
  #rule("S-Abs",
    $Delta tack.r tau_1 -> tau_2 <: tau'_1 -> tau'_2$,
    $Delta tack.r tau'_1 <: tau_1$,
    $Delta tack.r tau_2 <: tau'_2$)
  #rule("S-Univ",
    $Delta tack.r forall t <: tau_1. space tau_2 <: forall t <: tau'_1. space tau'_2$,
    $Delta tack.r tau_1 equiv tau'_1$,
    $Delta, t <: tau_1 tack.r tau_2 <: tau'_2$)
  \
  #rule("S-TAbs",
    $Delta tack.r Lambda t <: tau_1. space tau_2 <: Lambda t <: tau'_1. space tau'_2$,
    $Delta tack.r tau'_1 equiv tau_1$,
    $Delta tack.r tau_2 <: tau'_2$)
  #rule("S-TApp",
    $Delta tack.r tau_1[tau_2] <: tau'_1[tau'_2]$,
    $Delta tack.r tau'_1 <: tau_1$,
    $Delta tack.r tau_2 equiv tau'_2$)
  \
  #rule("S-Union1",
    $Delta tack.r tau <: tau_1 union tau_2$,
    $Delta tack.r tau <: tau_1$)
  #rule("S-Inter2",
    $Delta tack.r tau_1 sect tau_2 <: tau$,
    $Delta tack.r tau_1 <: tau$)
  \
  #rule("S-Union1",
    $Delta tack.r tau_1 union tau_2 <: tau$,
    $Delta tack.r tau_1 <: tau$, $Delta tack.r tau_2 <: tau$)
  #rule("S-Inter2",
    $Delta tack.r tau <: tau_1 sect tau_2$,
    $Delta tack.r tau <: tau_1$,
    $Delta tack.r tau <: tau_2$)
])

#pagebreak()

= Typing

The typing judgement is of the form $Delta space Gamma tack.r e : tau$. The hypotheses of $Gamma$ are of the form $x: tau$.

#rules([
  #rule("T-Var",
    $Gamma tack.r x : tau$,
    $x : tau in Gamma$)
  #rule("T-Sub",
    $Delta space Gamma tack.r e : tau'$,
    $Delta space Gamma tack.r e : tau$,
    $Delta tack.r tau <: tau'$)
  \
  #rule("T-Unit",
    $u : #Unit$)
  #rule("T-Bool",
    $b : #Bool$)
  #rule("T-Int",
    $i : #Int$)
  #rule("T-String",
    $s : #String$)
  \
  #rule("T-Tuple",
    $Delta space Gamma tack.r angle.l e_1, ... e_n angle.r : angle.l tau_1, ..., tau_n angle.r$,
    $Delta space Gamma tack.r e_1 : tau_1 space.quad ... space.quad Delta space Gamma tack.r e_n : tau_n$)
  #rule("T-TupleProj",
    $Delta space Gamma tack.r e.i : tau_i$,
    $Delta space Gamma tack.r e: angle.l tau_1, ..., tau_n angle.r$)
  \
  #rule("T-Record",
    $Delta space Gamma tack.r angle.l l_1= e_1, ...,  l_n = e_n angle.r : angle.l l_1 : tau_1, ..., l_n : tau_n angle.r$,
    $Delta space Gamma tack.r e_1 : tau_1 space.quad ... space.quad Delta space Gamma tack.r e_n : tau_n$)
  \
  #rule("T-RecordProj",
    $Delta space Gamma tack.r e.l_i : tau_i$,
    $Delta space Gamma tack.r e: angle.l l_1 : tau_1, ..., l_n : tau_n angle.r$)
  \
  #rule("T-Abs",
    $Delta space Gamma tack.r lambda x : tau_1. space e : tau_1 -> tau_2$,
    $Delta tack.r tau_1 :: *$,
    $Delta space Gamma, x : tau_1 tack.r e : tau_2$)
  #rule("T-AbsApp",
    $Delta space Gamma tack.r e_1(e_2) : tau_2$,
    $Delta space Gamma tack.r e_1 : tau_1 -> tau_2$,
    $Delta space Gamma tack.r e_2 : tau_1$)
  \
  #rule("T-Univ",
    $Delta space Gamma tack.r lambda t <: tau_1. space e : forall t <: tau_1. space tau_2$,
    $Delta, t <: tau_1 space Gamma tack.r e : tau_2$)
  #rule("T-UnivApp",
    $Delta space Gamma tack.r e[tau_3] : [t slash tau_3]tau_2$,
    $Delta space Gamma tack.r e : forall t <: tau_1. space tau_2$,
    $Delta tack.r tau_3 <: tau_1$)
  \
  #rule("T-Ascr",
    $Delta space Gamma tack.r e : tau : tau$,
    $Delta tack.r tau :: *$,
    $Delta space Gamma tack.r e : tau$)
])

#pagebreak()

= Primitives

The types of the primitive values of Clam are given by the following table:
\ \
#align(center, [
  #table(columns: (auto, auto),
    [*Symbol*], [*Type*],
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
    `if`, $forall T_1. space forall T_2. space #Bool -> T_1 -> T_2 -> T_1 union T_2$,
  )
  Note: `if` is not implemented as a primitive value yet.
])

/*
#rules([
  #grammar($
    "Primitive"
    p :: = & #raw("+") ("unary") && space.quad : #Int -> #Int \
         | & #raw("-") ("unary") && space.quad : #Int -> #Int$)
]) */

#pagebreak()

= Evaluation

The syntax of values is given by the following grammar:

#rules([
  #grammar($
    "Value"
    v ::= & u && space.quad "unit" \
        | & b && space.quad "boolean" \
        | & i && space.quad "integer" \
        | & s && space.quad "string" \
        | & angle.l v_1, ..., v_n angle.r && space.quad "tuple" \
        | & angle.l l_1 = v_1, ..., l_n = v_n angle.r && space.quad "record" \
        | & lambda x. space e && space.quad "abstraction" \
        | & lambda t. space e && space.quad "universal abstraction" $)
])

The evaluation judgement is of the form $e arrow.b.double v$.


#rules([
  #rule("V-Unit",
    $u arrow.b.double u$)
  #rule("V-Bool",
    $b arrow.b.double b$)
  #rule("V-Int",
    $n arrow.b.double i$,
    $n = i$)
  #rule("V-String",
    $s arrow.b.double s$)
  \
  #rule("V-Tuple",
    $angle.l e_1, ... e_n angle.r arrow.b.double angle.l v_1, ..., v_n angle.r$,
    $e_1 arrow.b.double v_1 space.quad ... space.quad e_n arrow.b.double v_n$)
  #rule("V-TupleProj",
    $e.i arrow.b.double v_i$,
    $e arrow.b.double angle.l v_1, ..., v_n angle.r$)
  \
  #rule("V-Record",
    $angle.l l_1 = e_1, ...,  l_n = e_n angle.r arrow.b.double angle.l l_1 = v_1, ..., l_n = v_n angle.r$,
    $e_1 arrow.b.double v_1 space.quad ... space.quad e_n arrow.b.double v_n$)
  \
  #rule("V-RecordProj",
    $e.l_i arrow.b.double v_i$,
    $e arrow.b.double angle.l l_1 = v_1, ..., l_n = v_n angle.r$)
  \
  #rule("V-Abs",
    $lambda x : tau. space e arrow.b.double lambda x. space e$)
  #rule("V-AbsApp",
    $e_1(e_2) arrow.b.double v$,
    $e_1 arrow.b.double lambda x. space e_3$,
    $[e_2 slash x]e_3 arrow.b.double v$)
  \
  #rule("V-Univ",
    $lambda t <: tau. space e arrow.b.double lambda t. space e$)
  #rule("V-UnivApp",
    $e_1[tau] arrow.b.double e_2$,
    $e_1 arrow.b.double lambda t. space e_2$)
  \
  #rule("V-Ascr",
    $e : tau arrow.b.double v$,
    $e arrow.b.double v $)
])
