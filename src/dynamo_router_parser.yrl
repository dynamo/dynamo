Nonterminals expressions expression terminal.

Terminals slash literal.

Rootsymbol expressions.

expressions -> expression expressions : { cat, '$1', '$2' }.
expressions -> expression : '$1'.

expression -> terminal : setelement(1, '$1', terminal).
% expression -> group : '$1'.
% expression -> start : '$1'.

terminal -> slash : '$1'.
terminal -> literal : '$1'.

Erlang code.

