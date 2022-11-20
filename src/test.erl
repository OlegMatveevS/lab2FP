-module(test).
-author("олег").

%% API
-export([]).
-include_lib("eunit/include/eunit.hrl").


init_test_() -> [
  ?_assert(btree:initBT() =:= {})
].

add_test_() -> [
  ?_assert(btree:insertBT(btree:initBT(), 10) =:= {10, {}, {}, 1}),
  ?_assert((btree:insertBT(btree:insertBT(btree:initBT(), 10),35) =:= {10, {}, {35, {}, {}, 1}, 2}))
].