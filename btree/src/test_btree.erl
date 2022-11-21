-module(test_btree).

%% API
-export([]).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(PROPERTY_TESTS_AMOUNT, 10).

init_test_() -> [
  ?_assert(btree:initBT() =:= {})
].

add_test_() -> [
  ?_assert(btree:insertBT(btree:initBT(), 10) =:= {10, {}, {}, 1}),
  ?_assert((btree:insertBT(btree:insertBT(btree:initBT(), 10),35) =:= {10, {}, {35, {}, {}, 1}, 2}))
].


filtration_test_() -> [
  ?_assert(btree:filtrationTree(fun(T) -> case T of 20 -> false; _ -> true end end, btree:insertBT(btree:insertBT(btree:initBT(), 10), 20)) =:= {10, {}, {}, 1})
].

increase_test_() -> [
  ?_assert(btree:increaseBT(btree:insertBT(btree:insertBT(btree:initBT(), 10), 20)) =:= {11, {}, {21, {}, {}, 1}, 2})
].



get_property_test_result(Property) -> proper:quickcheck(Property, [{numtests, ?PROPERTY_TESTS_AMOUNT}]).

%%Prop tests

prop_mull_commutativity() ->
  ?FORALL(
    {L1, L2},
    {list(integer()), list(integer())},
    begin
      Tree1 = btree:listToTree(L1),
      Tree2 = btree:listToTree(L2),
      btree:equalBT(btree:multiplyBT(Tree1, Tree2),btree:multiplyBT(Tree2, Tree1))
    end
  ).

prop_add_commutativity() ->
  ?FORALL(
    {L1, L2},
    {list(integer()), list(integer())},
    begin
      Tree1 = btree:listToTree(L1),
      Tree2 = btree:listToTree(L2),
      btree:sumBT(btree:addTree(Tree1, Tree2)) =:= btree:sumBT(btree:addTree(Tree2, Tree1))
    end
  ).

prop_associative_commutativity() ->
  ?FORALL(
    {L1, L2, L3},
    {list(integer()), list(integer()), list(integer())},
    begin
      HashSetA = btree:listToTree(L1),
      HashSetB = btree:listToTree(L2),
      HashSetC = btree:listToTree(L3),
      HashSetRes1 = btree:multiplyBT(btree:multiplyBT(HashSetB, HashSetC), btree:multiplyBT(HashSetA, HashSetB)),
      HashSetRes2 = btree:multiplyBT(btree:multiplyBT(HashSetA, HashSetB), btree:multiplyBT(HashSetB, HashSetC)),
      btree:equalBT(HashSetRes1, HashSetRes2) == true
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
