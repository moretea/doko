%% -*- mode: erlang; fill-column: 78; -*-

{incl_dirs_r, [
               "../apps/dk_ii",
               "../apps/dk_in",
               "../apps/dk_meta",
               "../apps/dk_pp",
               "../apps/dk_q",
               "../apps/dk_ring",
               "../apps/doko"
              ]}.
{excl_mods, [
             dk_ii_app,
             dk_meta_app,
             dk_q_parser,
             dk_ring_app
            ]}.
