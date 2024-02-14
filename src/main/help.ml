let help =
"Description:

    The Clam CLI. Use this program to interpret a Clam file or enter the Clam
    REPL.

Usage:

    clam [--options] <file>

Options:

    -h   --help         Display this message.
    -i   --interactive  Open the Clam read-eval-print loop. If this option is
                         present, the interpreted file is optional.
    -ast --show-ast     Display the program abstract syntax tree.
    -t   --show-types   Display the type of all definitions.
    -v   --show-values  Evaluate and display the results of all definitions. If
                         this option is not present, only the 'main' definition
                         is evaluated.
"
