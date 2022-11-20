-module(test).
-author("олег").

%% API
-export([]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("proper/include/proper.hrl").

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


%%Prop tests
prop_add_commutativity() ->
  ?FORALL(
    {L1, L2},
    {list(integer()), list(integer())},
    begin
      Tree1 = btree:from_list(L1),
      Tree2 = btree:from_list(L2),
      btree:equalBT(btree:insertBT(Tree1, Tree2), btree:insertBT(Tree2, Tree1))
    end
  ).
