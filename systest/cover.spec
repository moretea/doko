%% -*- mode: erlang; fill-column: 78; -*-

{incl_dirs_r,[
              "../apps/doko_cluster",
              "../apps/doko_doc",
              "../apps/doko_index",
              "../apps/doko_node",
              "../apps/doko_query"
             ]}.
{excl_mods,[
            doko_index_app,
            doko_query_parser
           ]}.
