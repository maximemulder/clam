let help =
"Description:

    The Clam CLI. Use this program to interpret a Clam file or enter the Clam
    REPL.

Usage:

    clam [--options] <file>

Options:

    -h --help         Display this message.
    -a --show-ast     Display the program abstract syntax tree.
    -k --show-kinds   Display the kinds of all type definitions.
    -t --show-types   Display the types of all expression definitions.
    -v --show-values  Display the results of evaluated expression definitions.
       --debug-infer  Display type inference debugging information.
"

(* CONSIDERED OTHER OPTIONS:

REPL -i --interactive
        --typing = static (default) | dynamic | none
        --eval = main | all
        --no-std
*)
