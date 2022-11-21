-module(test_btree).
-author("олег").

%% API
-export([]).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(PROPERTY_TESTS_AMOUNT, 10).

get_property_test_result(Property) -> proper:quickcheck(Property, [{numtests, ?PROPERTY_TESTS_AMOUNT}]).

%%Prop tests
prop_add_commutativity() ->
  ?FORALL(
    {L1, L2},
    {list(integer()), list(integer())},
    begin
      Tree1 = btree:from_list(L1),
      Tree2 = btree:from_list(L2),
      btree:equalBT(Tree1,Tree2)
    end
  ).

add_commutative_test() ->
  Property = prop_add_commutativity(),
  ?assert(get_property_test_result(Property)).