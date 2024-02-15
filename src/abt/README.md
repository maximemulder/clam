# The Clam Abstract Binding Tree (ABT)

The abstract binding tree is a simplified representation of a program, produced after analysis and transformation of the abstract syntax tree. Compared to the AST, the ABT provides the following simplifications:
- String-based names are replaced by binds, which unambiguously refer to a related declaration (this does not extend to record labels).
- Syntaxic sugars (multi-parameter functions, statements, variables...) are desugared into their pure form.

The ABT still closely relates to the original program code, as it notably remembers the position of each node.
