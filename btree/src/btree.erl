-module(btree).

-export([init_bt/0, isBT/1, insert_bt/2, isempty_bt/1, equal_bt/2, filtration_tree/2, increase_bt/1, add_tree/2, list_to_tree/1, sum_bt/1, mul_list/2, multiply_bt/2]).


init_bt() -> {}.

isBT(B) -> {_LMin, _RMax, _H, T} = reku(B), (T or isempty_bt(B)).

% Лист
reku({W, {}, {}, H}) -> {W, W, H, is_number(W) and is_number(H) and (H == 1)};

reku({W, L, {}, H}) -> {ULMin, _URMax, UH, UG} = reku(L),
  {ULMin, W, H, (UG and is_number(W) and is_number(H) and (W > ULMin) and (UH + 1 == H))};

reku({W, {}, R, H}) -> {_ULMin, URMax, UH, UG} = reku(R),
  {W, URMax, H, (UG and is_number(W) and is_number(H) and (W =< URMax) and (UH + 1 == H))};


reku({W, L, R, H}) -> {LMin, _RMax, Ulh, Ulg} = reku(L),
  {_LMin, UrRMax, Urh, Urg} = reku(R),
  {LMin, UrRMax, H,
    % проверка достоверности
    (Ulg and Urg and is_number(W) and is_number(H) and (W > LMin) and (W =< UrRMax) and (my_max(Ulh, Urh) + 1 == H))
  };

% Все остальное не является BTree
reku(_) -> {a, a, a, false}.

% Самый простой случай: пустое дерево
insert_bt({}, E) -> {E, {}, {}, 1};

% Вставить в дерево
insert_bt({W, LTree, RTree, H}, Ele) ->
  case (Ele < W) of
    % Ссылка установлена:
    true -> case (isempty_bt(LTree)) of
              % Левый слот свободен, поместите сюда новый лист
              % Если что-то висит с другой стороны, высота остается прежней
              true when (H > 1) -> {W, {Ele, {}, {}, 1}, RTree, H};
              %H равно 1 (т.е. лист), поэтому высота увеличивается на 1
              true -> {W, {Ele, {}, {}, 1}, RTree, H + 1};

              % левый слот не свободен, переместите элемент ниже и дождитесь высоты следующего дерева
              false -> {Lw, Llt, Lrt, Lh} = insert_bt(LTree, Ele),
                case (H > Lh) of
                  % Если на другой стороне есть более длинная ветвь, H остается нетронутой
                  true -> {W, {Lw, Llt, Lrt, Lh}, RTree, H};
                  % левое поддерево стало глубже, высоту нужно увеличить на 1
                  false -> {W, {Lw, Llt, Lrt, Lh}, RTree, H + 1}
                end
            end;

    % Установлен справа:
    false -> case (isempty_bt(RTree)) of
               true when (H > 1) -> {W, LTree, {Ele, {}, {}, 1}, H};
               true -> {W, LTree, {Ele, {}, {}, 1}, H + 1};

               false -> {Rw, Rlt, Rrt, Rh} = insert_bt(RTree, Ele),
                 case (H > Rh) of
                   true -> {W, LTree, {Rw, Rlt, Rrt, Rh}, H};
                   false -> {W, LTree, {Rw, Rlt, Rrt, Rh}, H + 1}
                 end
             end
  end.


% перемножить значения деревьев друг на друга
multiply_bt({}, {}) -> {};
multiply_bt({}, Tree) -> Tree;
multiply_bt(Tree, {}) -> Tree;
multiply_bt({W1, _L1, _R1, _H1}, {W2, _L2, _R2, _H2}) -> lists:foldl(fun(X, Y) -> insert_bt(Y, X) end, {},
  mul_list({W1, _L1, _R1, _H1}, {W2, _L2, _R2, _H2})).

mul_list({}, {}) -> [];
mul_list(_,{}) -> [];
mul_list({},_) -> [];
mul_list({W1, L1, R1, _H1}, {W2, L2, R2, _H2})-> [W1*W2] ++ mul_list(L1,L2) ++ mul_list(R1,R2).



add_tree({}, {}) -> {};
add_tree({}, Tree) -> Tree;
add_tree(Tree, {}) -> Tree;
add_tree(MasterTree, {W, L, R, _H}) ->
  MasterTree1 = add_tree(MasterTree, L),
  MasterTree2 = insert_bt(MasterTree1, W),
  add_tree(MasterTree2, R).


% обход дерева, вернуть новое дерево соответсвующее указанным значениям
filtration_tree(Fun, {W, L, R, _H}) -> lists:foldl(fun(X, Y) -> insert_bt(Y, X) end, {},
  filtration_tree_check(Fun, {W, L, R, _H})
).

%вернуть список значений, которые удовлетворяют функции
filtration_tree_check(_, ({})) -> [];
filtration_tree_check(Fun, {W, L, R, _H}) ->
  case (Fun(W)) of
    true -> [W] ++ filtration_tree_check(Fun, L) ++ filtration_tree_check(Fun, R);
    false -> filtration_tree_check(Fun, L), filtration_tree_check(Fun, R)
  end.


% Сохранение всех значений дерева в список
tree_to_list({}) -> [];
tree_to_list({W, L, R, _H}) -> ([W] ++ tree_to_list(L) ++ tree_to_list(R)).

% сумма всех элементов дерева используя foldl
sum_bt(BT) -> lists:foldl(fun(X, Y) -> X + Y end, 0, tree_to_list(BT)).

% increase all elements in tree use map return new tree
increase_bt(BT) ->
  lists:foldl(fun(X, Y) -> insert_bt(Y, X) end, {}, lists:map(fun(X) -> X + 1 end, tree_to_list(BT))).


% Проверяет наличие пустого дерева
isempty_bt({}) -> true;
isempty_bt(_) -> false.


% значение и высота одинаковы, продолжить рекурсивно проверять левое и правое поддерево
equal_bt({W, LT1, RT1, H}, {W, LT2, RT2, H}) -> equal_bt(LT1, LT2) and equal_bt(RT1, RT2);
equal_bt({}, {}) -> true;


% Все остальные случаи не равны
equal_bt(_BT1, _BT2) -> false.

my_max(A, B) when A > B -> A;
my_max(_A, B) -> B.

list_to_tree(List) -> lists:foldl(fun(X, Y) -> insert_bt(Y, X) end, {}, List).



