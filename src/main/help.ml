let help =
"Description:

    The Clam CLI. Use this program to interpret a Clam file or enter the Clam
    REPL.

Usage:

    clam [--options] <file>

Options:

    -h --help         Display this message.
    -i --interactive  Open the Clam read-eval-print loop. If this option is
                       present, the interpreted file is optional.
    -a --show-ast     Display the program abstract syntax tree.
    -t --show-types   Display the type of all definitions.
    -v --show-values  Display the results of all directly evaluated
                       definitions.
       --eval-all     Evaluate all definitions. If this option is not present,
                       only the 'main' definition is evaluated.
       --no-std       Interpret the program without including the standard
                       library.
       --only-parse   Run the interpreter only up to parsing.
       --only-sugar   Run the interpreter only up to desugaring.
       --only-type    Run the interpreter only up to typing.
       --skip-typing  Evalute the program without type checking it.
"
