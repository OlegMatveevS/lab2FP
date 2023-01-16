# lab2FP
# project-euler
Министерство науки и высшего образования Российской Федерации федеральное государственное автономное образовательное учреждение высшего образования

«Национальный исследовательский университет ИТМО»

---
__ФПИиКТ, Системное и Прикладное Программное Обеспечение__

__Лабораторная работа №1__

по Функциональному программированию

Выполнил: Матвеев О.И.

Группа: P34112

Преподаватель: Пенской Александр Владимирович

###### Санкт-Петербург
###### 2022 г.
---
Реализовать бинарное дерево.

    Функции:

    добавление и удаление элементов;
    фильтрация;
    отображение (map);
    свертки (левая и правая);
    структура должна быть моноидом.

    Структуры данных должны быть неизменяемыми.
    Библиотека должна быть протестирована в рамках unit testing.
    Библиотека должна быть протестирована в рамках property-based тестирования (как минимум 3 свойства, включая свойства монойда).
    Структура должна быть полиморфной.
    Требуется использовать идиоматичный для технологии стиль программирования.

+ __Функция добавления узла дерева__
``` erlang
insert_bt({Key, LTree, RTree, Height}, Element) -> case (Element < Key) of
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
  ```
  
  + __Функция удаления узла дерева__
  ``` erlang
remove_bt({Key, LTree, RTree, Height}, Element) -> case (Element < Key) of
    true -> case (isempty_bt(LTree)) of
              true -> {Key, LTree, RTree, Height};
              false -> Node = remove_bt(LTree, Element),
                case (Height > Node) of
                  true -> {Key, Node, RTree, Height};
                  false -> {Key, Node, RTree, Height + 1}
                end
            end;

    false -> case (isempty_bt(RTree)) of
               true -> {Key, LTree, RTree, Height};
               false -> Node = remove_bt(RTree, Element),
                 case (Height > Node) of
                   true -> {Key, LTree, Node, Height};
                   false -> {Key, LTree, Node, Height + 1}
                 end
             end
  end.
  ```
   + __Фильтрация__
  ```erlang
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

  ```
  
  
  + __Свертки(левая и правая)__
  ```erlang
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
  ```


  + __отображение (map)__
  ```erlang
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

  ```
  
   + __Добавление деревьев__
  ```erlang
  add_tree({}, {}) -> {};
add_tree({}, Tree) -> Tree;
add_tree(Tree, {}) -> Tree;
add_tree(MasterTree, {W, L, R, _H}) ->
  MasterTree1 = add_tree(MasterTree, L),
  MasterTree2 = insert_bt(MasterTree1, W),
  add_tree(MasterTree2, R).

  ```
  
   + __Слияние деревьев(складывать значения по уровню дерева)__
  ```erlang
  merge({}, {}) -> {};
merge({}, Tree) -> Tree;
merge(Tree, {}) -> Tree;
merge({Key1, L1, R1, H1}, {Key2, L2, R2, H2}) -> lists:foldl(fun(X, Y) -> insert_bt(Y, X) end, {},
  merge_list({Key1, L1, R1, H1}, {Key2, L2, R2, H2})).

merge_list({}, {}) -> [];
merge_list(_, {}) -> [];
merge_list({}, _) -> [];
merge_list({Key1, L1, R1, _H1}, {Key2, L2, R2, _H2}) ->
  [Key1 + Key2] ++ merge_list(L1, L2) ++ merge_list(R1, R2).

  ```
  
  + __Property-based тестирование__
  ```erlang
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

merge_commutative_test() ->
  Property = prop_merge_commutativity(),
  ?assert(get_property_test_result(Property)).

associative_commutative_test() ->
  Property = prop_associative_commutativity(),
  ?assert(get_property_test_result(Property)).


  ```
  
  
  
 

