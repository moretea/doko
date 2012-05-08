Nonterminals query.
Terminals string '(' ')' '&' '|' '!'.
Rootsymbol query.

Unary 300 '!'.
Left 200 '&'.
Left 100 '|'.

query -> '(' query ')' : '$2'.
query -> query '&' query : {and_q, '$1', '$3'}.
query -> query '|' query : {or_q, '$1', '$3'}.
query -> '!' query : {not_q, '$2'}.
query -> string : {kw_q, '$1'}.
