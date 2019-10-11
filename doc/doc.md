Interpreter for a simple expression language
============================================

You will write the evaluator for a simple language. The data types
for the abstract syntax tree are provided, as is a parser and the
code that drives the read-eval-print loop (REPL):

* `ast.sml`: data types for the abstract syntax tree
* `parse.sml`: the parser
* `simple.sml`: the tokenizer and main read-eval-print loop
* `eval.sml`: you should write the evaluator here

The grammar for the language is provided here:

```
input           ->  expression | fundef
fundef          ->  ( define function arglist expression )
arglist         ->  ( variable* )
expression      ->  value
                 |  variable
                 |  ( if expression expression expression )
                 |  ( while expression expression )
                 |  ( set variable expression )
                 |  ( begin expression+ )
                 |  ( optr expression* )
optr            ->  function | value-op
value           ->  integer
value-op        ->  + | - | * | / | = | < | > | print
function        ->  name
variable        ->  name
integer         ->  sequence of digits, possibly preceded by minus sign
name            ->  any sequence of characters not an integer, and not
                    containing a blank or any of the following characters:
                    ( ) ;.
```

A function cannot be one of the “keywords” define, if, while, begin,
or set, or any of the value-op's. Aside from this, names can use any
characters on the keyboard. Comments are introduced by the
character ';' and continue to the end of the line; this is why ';'
cannot occur within a name. A session is terminated by entering
“quit”; thus it is highly inadvisable to use this name for a
variable.


The details
-----------

You must write the `eval` function that evaluates an expression in
the simple language. It's type signature is:

    fn: (string * int) list * expression -> (string * int) list * int

The `eval` function accepts a pair containing an environment, i.e.,
the local variables (called *rho*), and the expression to evaluate.
It returns the updated environment (with any changes made during
evaluation) and the result value (an integer).

Expression evaluation depends on the specific kind of expression.
See `ast.sml` for the type definitions:

*   `ValExp`: if the input is a value, simply return the value (the
    integer stored inside the `ValExp` instance).

*   `VarExp`: if the input is a variable, look it up in the local
    environment and return the value found. If it is not present in
    the local environment, try the global environment. If it is not
    present there, signal an error by raising a RuntimeError with a
    suitable message.

*   `IfExp`: evaluate the condition. If the result is a true
    value (anything other than zero), evaluate and return the
    “then” branch. Otherwise, evaluate and return the “else”
    branch.

*   `WhileExp`: evaluate the condition. If the result is false
    (zero) then return zero, otherwise evaluate the body and
    repeat.

*   `SetExp`: evaluate the value. If the variable name is present in
    the local environment, then store the value there. If not, store
    it in the global environment. Return the value.

*   `BeginExp`: evaluate all of the expressions in order and return
    the result of evaluating the last one.

*   `BinaryBuiltinExp` and `UnaryBuiltinExp` primitive operations:
    
    *   `+`, `-`, `*`, `/`: evaluate the arguments and
        apply the appropriate operation on the resulting numbers and
        return the result.

    *   `=`, `>`, `<`: evaluate the two arguments and compare them
        as appropriate for the operator. Return one for true and
        zero for false.

    *   `print`: evaluate the single argument, convert it to a
        string, and print it followed by a newline. Return the value
        of the argument.

*   User-defined functions: get the named function from the
    global list of functions (it is an error if the function
    does not exist). Make sure the number of arguments passed in
    matches the number of formal parameters in the function
    definition.

    Create a new environment where the keys are the formal
    parameter names and the values are the evaluated arguments.
    Use this new environment when you evaluate the body of the
    function. Return the result.
