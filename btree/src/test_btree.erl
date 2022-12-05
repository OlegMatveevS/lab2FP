-module(test_btree).

%% API
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(PROPERTY_TESTS_AMOUNT, 10).


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
