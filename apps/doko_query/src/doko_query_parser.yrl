Nonterminals 
query id_list.

Terminals
string id '(' ')' '[' ',' ']' '&' '|' '!' 'in'.

Rootsymbol
query.

Unary 300 '!'.
Left  200 '&'.
Left  100 '|'.

query -> '(' query ')'                : '$2'.
query -> query '&' query              : {and_q,'$1','$3'}.
query -> query '|' query              : {or_q,'$1','$3'}.
query -> '!' query                    : {not_q,'$2'}.
query -> string                       : {term_q,'$1',[]}.
query -> string 'in' id               : {term_q,'$1',['$3']}.
query -> string 'in' '[' id_list  ']' : {term_q,'$1','$4'}.

id_list -> id             : ['$1'].
id_list -> id ',' id_list : ['$1'|'$3'].
