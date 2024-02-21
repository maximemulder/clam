let help =
"Description:

    The Clam CLI. Use this program to interpret a Clam file or enter the Clam
    REPL.

Usage:

    clam [--options] <file>

Options:

    -h --help         Display this message.
    -a --show-ast     Display the program abstract syntax tree.
    -t --show-types   Display the type of all definitions.
    -v --show-values  Display the results of all evaluated definitions.
"

(* CONSIDERED OTHER OPTIONS:

REPL -i --interactive
        --typing = static (default) | dynamic | none
        --eval = main | all
        --no-std
*)