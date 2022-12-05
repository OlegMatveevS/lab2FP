-module(test_btree).

%% API
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(PROPERTY_TESTS_AMOUNT, 10).

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



get_property_test_result(Property) -> proper:quickcheck(Property, [{numtests, ?PROPERTY_TESTS_AMOUNT}]).

%%Prop tests

prop_merge_commutativity() ->
  ?FORALL(
    {L1, L2},
    {list(integer()), list(integer())},
    begin
      Tree1 = btree:list_to_tree(L1),
      Tree2 = btree:list_to_tree(L2),
      btree:equal_bt(btree:merge(Tree1, Tree2),btree:merge(Tree2, Tree1))
    end
  ).

prop_add_commutativity() ->
  ?FORALL(
    {L1, L2},
    {list(integer()), list(integer())},
    begin
      Tree1 = btree:list_to_tree(L1),
      Tree2 = btree:list_to_tree(L2),
      btree:sum_bt(btree:add_tree(Tree1, Tree2)) =:= btree:sum_bt(btree:add_tree(Tree2, Tree1))
    end
  ).

prop_associative_commutativity() ->
  ?FORALL(
    {L1, L2},
    {list(integer()), list(integer())},
    begin
      A = btree:list_to_tree(L1),
      B = btree:list_to_tree(L2),
      C = btree:list_to_tree([5,1,2,3,5,2,6,2,3,6,2,1,6,8]),
      btree:equal_bt(btree:merge(C,btree:merge(A, B)),btree:merge(B,btree:merge(A, C)))
    end
  ).

add_commutative_test() ->
  Property = prop_add_commutativity(),
  ?assert(get_property_test_result(Property)).

mull_commutative_test() ->
  Property = prop_merge_commutativity(),
  ?assert(get_property_test_result(Property)).

associative_commutative_test() ->
  Property = prop_associative_commutativity(),
  ?assert(get_property_test_result(Property)).
