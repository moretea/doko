-include("../../doko_utf8/include/doko_utf8.hrl").

%%----------------------------------------------------------------------------
%% Type declarations
%%----------------------------------------------------------------------------

-type doc_id()  :: pos_integer().
-type zone_id() :: nonempty_string().
-type zone()    :: {zone_id(),utf8_string()}.

%%----------------------------------------------------------------------------
%% Record definitions
%%----------------------------------------------------------------------------

-record(doc, {id         :: doc_id(),
              zones      :: nonempty_list(zone()),
              raw = true :: boolean()}).

%% Local variables:
%% mode: erlang
%% fill-column: 78
%% coding: latin-1
%% End:
