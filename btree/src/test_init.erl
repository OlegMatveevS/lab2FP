-module(test_init).

%% API
-include_lib("eunit/include/eunit.hrl").

init_test_() -> [
  ?_assert(btree:init_bt() =:= {})
].

foldl_test_() -> [
  ?_assert(
    btree:foldl(
      fun(X, Res) -> X + Res end, 0, btree:list_to_tree([1,2])
    ) =:= 3
  )
].

foldr_test_() -> [
  ?_assert(
    btree:foldr(
      fun(X, Res) -> X - Res*2 end, 0, btree:list_to_tree([1,2])
    ) =:= -3)
].

map_test_() -> [
  ?_assert(
    btree:btree_map(
      fun(X) -> X*2 end, btree:list_to_tree([1,2])
    ) =:= btree:list_to_tree([2,4])
  )
].

merge_test_() -> [
  ?_assert(
    btree:merge(btree:list_to_tree([2,4]),btree:list_to_tree([2,4])) =:= btree:list_to_tree([4,8])
  )
].

filter_test_() -> [
  ?_assert(
    btree:btree_filter(
      fun(X) -> case X of 2 -> false; _ -> true end end, btree:list_to_tree([1,2,3])
    ) =:= btree:list_to_tree([1,3])
  )
].

add_test_() -> [
  ?_assert(btree:insert_bt(btree:init_bt(), 10) =:= {10, {}, {}, 1}),
  ?_assert((btree:insert_bt(btree:insert_bt(btree:init_bt(), 10),35) =:= {10, {}, {35, {}, {}, 1}, 2}))
].

increase_test_() -> [
  ?_assert(btree:increase_bt(btree:insert_bt(btree:insert_bt(btree:init_bt(), 10), 20)) =:= {11, {}, {21, {}, {}, 1}, 2})
].