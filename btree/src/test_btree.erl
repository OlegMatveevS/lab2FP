-module(test_btree).

%% API
-export([]).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(PROPERTY_TESTS_AMOUNT, 10).

init_test_() -> [
  ?_assert(btree:init_bt() =:= {})
].

add_test_() -> [
  ?_assert(btree:insert_bt(btree:init_bt(), 10) =:= {10, {}, {}, 1}),
  ?_assert((btree:insert_bt(btree:insert_bt(btree:init_bt(), 10),35) =:= {10, {}, {35, {}, {}, 1}, 2}))
].


filtration_test_() -> [
  ?_assert(btree:filtration_tree(fun(T) -> case T of 20 -> false; _ -> true end end, btree:insert_bt(btree:insert_bt(btree:init_bt(), 10), 20)) =:= {10, {}, {}, 1})
].

increase_test_() -> [
  ?_assert(btree:increase_bt(btree:insert_bt(btree:insert_bt(btree:init_bt(), 10), 20)) =:= {11, {}, {21, {}, {}, 1}, 2})
].



get_property_test_result(Property) -> proper:quickcheck(Property, [{numtests, ?PROPERTY_TESTS_AMOUNT}]).

%%Prop tests

prop_mull_commutativity() ->
  ?FORALL(
    {L1, L2},
    {list(integer()), list(integer())},
    begin
      Tree1 = btree:list_to_tree(L1),
      Tree2 = btree:list_to_tree(L2),
      btree:equal_bt(btree:multiply_bt(Tree1, Tree2),btree:multiply_bt(Tree2, Tree1))
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
    {L1, L2, L3},
    {list(integer()), list(integer()), list(integer())},
    begin
      HashSetA = btree:list_to_tree(L1),
      HashSetB = btree:list_to_tree(L2),
      HashSetC = btree:list_to_tree(L3),
      HashSetRes1 = btree:multiply_bt(btree:multiply_bt(HashSetB, HashSetC), btree:multiply_bt(HashSetA, HashSetB)),
      HashSetRes2 = btree:multiply_bt(btree:multiply_bt(HashSetA, HashSetB), btree:multiply_bt(HashSetB, HashSetC)),
      btree:equal_bt(HashSetRes1, HashSetRes2) == true
    end
  ).

add_commutative_test() ->
  Property = prop_add_commutativity(),
  ?assert(get_property_test_result(Property)).

mull_commutative_test() ->
  Property = prop_mull_commutativity(),
  ?assert(get_property_test_result(Property)).

associative_commutative_test() ->
  Property = prop_associative_commutativity(),
  ?assert(get_property_test_result(Property)).
