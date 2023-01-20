-module(btree).

-export([init_bt/0, is_bt/1, insert_bt/2, isempty_bt/1, increase_bt/1, add_tree/2, list_to_tree/1, sum_bt/1, foldl/3, foldr/3, btree_map/2, map/2, filter/2, btree_filter/2, remove_bt/2, join_tree/2]).


init_bt() -> {}.

is_bt(B) -> {_LMin, _RMax, _H, T} = leaf(B), (T or isempty_bt(B)).

% Лист
leaf({Key, {}, {}, Height}) -> {Key, Key, Height, is_number(Key)
  and is_number(Height) and (Height == 1)};

leaf({Key, Left, {}, H}) -> {ULMin, _URMax, Right, Height} = leaf(Left),
  {ULMin, Key, H, (Height and is_number(Key)
    and is_number(H) and (Key > ULMin) and (Right + 1 == H))};

leaf({Key, {}, Right, Height}) -> {_ULMin, URMax, Left, Height} = leaf(Right),
  {Key, URMax, Height, (Height and is_number(Key)
    and is_number(Height) and (Key =< URMax) and (Left + 1 == Height))};


leaf({Key, Left, Right, Height}) -> {LMin, _RMax, Left, URigh} = leaf(Left),
  {_LMin, UrRMax, URight, Height} = leaf(Right),
  {LMin, UrRMax, Height,
    % проверка достоверности
    (URigh and Height and is_number(Key)
      and is_number(Height) and (Key > LMin) and (Key =< UrRMax)
      and (my_max(Left, URight) + 1 == Height))
  };

% Все остальное не является BTree
leaf(_) -> {a, a, a, false}.

% Самый простой случай: пустое дерево
insert_bt({}, E) -> {E, {}, {}, 1};

% Вставить в дерево
insert_bt({Key, LTree, RTree, Height}, Element) ->
  case (Element < Key) of
    true -> case (isempty_bt(LTree)) of
              true -> {Key, {Element, {}, {}, 1}, RTree, Height + 1};
              false -> Node = insert_bt(LTree, Element),
                case (Height > Node) of
                  true -> {Key, Node, RTree, Height};
                  false -> {Key, Node, RTree, Height + 1}
                end
            end;
    false -> case (isempty_bt(RTree)) of
               true -> {Key, LTree, {Element, {}, {}, 1}, Height + 1};
               false -> Node = insert_bt(RTree, Element),
                 case (Height > Node) of
                   true -> {Key, LTree, Node, Height};
                   false -> {Key, LTree, Node, Height + 1}
                 end
             end
  end.
% Remove element from tree
remove_bt({Key, LTree, RTree, Height}, Element)
  -> case (Element < Key) of
       true -> case (isempty_bt(LTree)) of
                 true -> {Key, LTree, RTree, Height};
                 false ->
                   {NextKey, LeftNext, RightNext, HeightNext} = remove_bt(LTree, Element),
                   case (Height > HeightNext) of
                     true ->
                       {Key, {NextKey, LeftNext, RightNext, HeightNext}, RTree, Height};
                     false ->
                       {Key, {NextKey, LeftNext, RightNext, HeightNext}, RTree, Height - 1}
                   end
               end;

       false -> case (isempty_bt(RTree)) of
                  true -> {Key, LTree, RTree, Height};
                  false ->
                    {RightNextKey, RightRightNext, LeftRightNext, HeightRight} = remove_bt(RTree, Element),
                    case (Height > HeightRight) of
                      true -> {Key, LTree,
                        {RightNextKey, RightRightNext, LeftRightNext, HeightRight}, Height};
                      false -> {Key, LTree,
                        {RightNextKey, RightRightNext, LeftRightNext, HeightRight}, Height - 1}
                    end
                end
     end.


foldl(_, Acc, {}) -> Acc;
foldl(Fun, Acc, {Key, L, R, _H}) ->
  Acc0 = foldl(Fun, Acc, L),
  Acc1 = Fun(Key, Acc0),
  foldl(Fun, Acc1, R).

foldr(_, Acc, {}) -> Acc;
foldr(Fun, Acc, {Key, L, R, _H}) ->
  Acc0 = foldr(Fun, Acc, R),
  Acc1 = Fun(Key, Acc0),
  foldr(Fun, Acc1, L).

btree_map(Fun, {Key, L, R, H}) ->
  map(Fun, {Key, L, R, H}).
map(Fun, {Key, L, R, _H}) ->
  Node3 = insert_bt({}, Fun(Key)),
  map(Fun, L, Node3),
  map(Fun, R, Node3).
map(_, {}, {W, L, R, H}) -> {W, L, R, H};
map(Fun, {Key, L, R, _H}, {Key2, L2, R2, H2}) ->
  Node2 = map(Fun, L, {Key2, L2, R2, H2}),
  Node3 = insert_bt(Node2, Fun(Key)),
  map(Fun, R, Node3).



btree_filter(Fun, {W, L, R, H}) -> filter(Fun, {W, L, R, H}).
filter(Fun, {W, L, R, _H}) ->
  Node3 = case Fun(W) of
            true -> insert_bt({}, W);
            false -> {}
          end,
  filter(Fun, L, Node3),
  filter(Fun, R, Node3).
filter(_, {}, {Key, L, R, H}) -> {Key, L, R, H};
filter(Fun, {Key, L, R, _H}, {Key2, L2, R2, H2}) ->
  Node2 = filter(Fun, L, {Key2, L2, R2, H2}),
  Node3 = case Fun(Key) of
            true -> insert_bt(Node2, Key);
            false -> Node2
          end,
  filter(Fun, R, Node3).

join_tree({}, {}) -> {};
join_tree({}, Tree) -> Tree;
join_tree(Tree, {}) -> Tree;
join_tree(MasterTree, MasterTree2) ->
  ListFirst = tree_to_list(MasterTree),
  ListSecond = tree_to_list(MasterTree2),
  List = lists:sort(lists:append(ListFirst, ListSecond)),
  list_to_tree(List).

add_tree({}, {}) -> {};
add_tree({}, Tree) -> Tree;
add_tree(Tree, {}) -> Tree;
add_tree(MasterTree, {W, L, R, _H}) ->
  MasterTree1 = add_tree(MasterTree, L),
  MasterTree2 = insert_bt(MasterTree1, W),
  add_tree(MasterTree2, R).

% Сохранение всех значений дерева в список
tree_to_list({}) -> [];
tree_to_list({W, L, R, _H}) -> ([W] ++ tree_to_list(L) ++ tree_to_list(R)).

% сумма всех элементов дерева используя foldl
sum_bt(BT) -> lists:foldl(fun(X, Y) -> X + Y end, 0, tree_to_list(BT)).

% increase all elements in tree use map return new tree
increase_bt(BT) ->
  lists:foldl(fun(X, Y) -> insert_bt(Y, X) end, {},
    lists:map(fun(X) -> X + 1 end, tree_to_list(BT))).

% Проверяет наличие пустого дерева
isempty_bt({}) -> true;
isempty_bt(_) -> false.

my_max(A, B) when A > B -> A;
my_max(_A, B) -> B.

list_to_tree(List) -> lists:foldl(fun(X, Y) -> insert_bt(Y, X) end, {}, List).







