-module(test_btree).

%% API
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(PROPERTY_TESTS_AMOUNT, 10).


get_property_test_result(Property) -> proper:quickcheck(Property, [{numtests, ?PROPERTY_TESTS_AMOUNT}]).

%%Prop tests
prop_associative() ->
  ?FORALL({LeftList, MiddleList, RightList},
    {list(integer()), list(integer()), list(integer())},
    begin
      Left = btree:list_to_tree(LeftList),
      Middle = btree:list_to_tree(MiddleList),
      Right = btree:list_to_tree(RightList),
      btree:join_tree(btree:join_tree(Middle, Left),Right) == btree:join_tree(btree:join_tree(Left, Right),Middle)
    end
  ).

prop_add_neutral_element() ->
  ?FORALL(
    {L1},
    {list(integer())},
    begin
      TreeEmpty = btree:list_to_tree([]),
      Tree1 = btree:list_to_tree(L1),
      btree:add_tree(Tree1, TreeEmpty) == btree:add_tree(TreeEmpty, Tree1)
end
  ).

prop_addition_tree() ->
  ?FORALL(
    {L1, L2},
    {list(integer()), list(integer())},
    begin
      Tree1 = btree:list_to_tree(L1),
      Tree2 = btree:list_to_tree(L2),
      Result = btree:join_tree(Tree1, Tree2),
      InverseResult = btree:join_tree(Tree2, Tree1),
      Result == InverseResult
    end
  ).


add_neutral_test() ->
  Property = prop_add_neutral_element(),
  ?assert(get_property_test_result(Property)).

associative_test() ->
  Property = prop_associative(),
  ?assert(get_property_test_result(Property)).

addition_test() ->
  Property = prop_addition_tree(),
  ?assert(get_property_test_result(Property)).



