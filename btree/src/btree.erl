-module(btree).
-author("олег").

-export([initBT/0, isBT/1, insertBT/2, isEmptyBT/1, equalBT/2, testALL/0]).


initBT() -> {}.

isBT(B) -> {_LMin, _RMax, _H, T} = reku(B), (T or isEmptyBT(B)).

% Лист
reku({W, {}, {}, H}) -> {W, W, H, is_number(W) and is_number(H) and (H == 1)};

reku({W, L, {}, H}) -> {ULMin, _URMax, UH, UG} = reku(L),
  {ULMin, W, H, (UG and is_number(W) and is_number(H) and (W > ULMin) and (UH + 1 == H))};

reku({W, {}, R, H}) -> {_ULMin, URMax, UH, UG} = reku(R),
  {W, URMax, H, (UG and is_number(W) and is_number(H) and (W =< URMax) and (UH + 1 == H))};


reku({W, L, R, H}) -> {UL_LMin, _UL_RMax, UL_H, UL_G} = reku(L),
  {_UR_LMin, UR_RMax, UR_H, UR_G} = reku(R),
  {UL_LMin, UR_RMax, H,
    % проверка достоверности
    (UL_G and UR_G and is_number(W) and is_number(H) and (W > UL_LMin) and (W =< UR_RMax) and (my_max(UL_H, UR_H) + 1 == H))
  };

% Все остальное не является BTree
reku(_) -> {a, a, a, false}.

% Самый простой случай: пустое дерево
insertBT({}, E) -> {E, {}, {}, 1};

% Вставить в дерево
insertBT({W, LTree, RTree, H}, Ele) ->
  case (Ele < W) of
    % Ссылка установлена:
    true -> case (isEmptyBT(LTree)) of
              % Левый слот свободен, поместите сюда новый лист
              % Если что-то висит с другой стороны, высота остается прежней
              true when (H > 1) -> {W, {Ele, {}, {}, 1}, RTree, H};
              %H равно 1 (т.е. лист), поэтому высота увеличивается на 1
              true -> {W, {Ele, {}, {}, 1}, RTree, H + 1};

              % левый слот не свободен, переместите элемент ниже и дождитесь высоты следующего дерева
              false -> {L_W, L_LT, L_RT, L_H} = insertBT(LTree, Ele),
                case (H > L_H) of
                  % Если на другой стороне есть более длинная ветвь, H остается нетронутой
                  true -> {W, {L_W, L_LT, L_RT, L_H}, RTree, H};
                  % левое поддерево стало глубже, высоту нужно увеличить на 1
                  false -> {W, {L_W, L_LT, L_RT, L_H}, RTree, H + 1}
                end
            end;

    % Установлен справа:
    false -> case (isEmptyBT(RTree)) of
               true when (H > 1) -> {W, LTree, {Ele, {}, {}, 1}, H};
               true -> {W, LTree, {Ele, {}, {}, 1}, H + 1};

               false -> {R_W, R_LT, R_RT, R_H} = insertBT(RTree, Ele),
                 case (H > R_H) of
                   true -> {W, LTree, {R_W, R_LT, R_RT, R_H}, H};
                   false -> {W, LTree, {R_W, R_LT, R_RT, R_H}, H + 1}
                 end
             end
  end.

% обход дерева, вернуть новое дерево соответсвующее указанным значениям
filtrationTree(Fun, {W, L, R, _H}) -> lists:foldl(fun(X, Y) -> insertBT(Y, X) end, {},
  filtrationTreeCheck(Fun, {W, L, R, _H})
).

%вернуть список значений, которые удовлетворяют функции
filtrationTreeCheck(_, ({})) -> [];
filtrationTreeCheck(Fun, {W, L, R, _H}) ->
  case (Fun(W)) of
    true -> [W] ++ filtrationTreeCheck(Fun, L) ++ filtrationTreeCheck(Fun, R);
    false -> filtrationTreeCheck(Fun, L), filtrationTreeCheck(Fun, R)
  end.


% Сохранение всех значений дерева в список
treeToList({}) -> [];
treeToList({W, L, R, _H}) -> ([W] ++ treeToList(L) ++ treeToList(R)).

% сумма всех элементов дерева используя foldl
sumBT(BT) -> lists:foldl(fun(X, Y) -> X + Y end, 0, treeToList(BT)).

% increase all elements in tree use map return new tree
increaseBT(BT) ->
  lists:foldl(fun(X, Y) -> insertBT(Y, X) end, {}, lists:map(fun(X) -> X + 1 end, treeToList(BT))).


% Проверяет наличие пустого дерева
isEmptyBT({}) -> true;
isEmptyBT(_) -> false.


% значение и высота одинаковы, продолжить рекурсивно проверять левое и правое поддерево
equalBT({W, LT1, RT1, H}, {W, LT2, RT2, H}) -> equalBT(LT1, LT2) and equalBT(RT1, RT2);
equalBT({}, {}) -> true;

% Все остальные случаи не равны
equalBT(_BT1, _BT2) -> false.

my_max(A, B) when A > B -> A;
my_max(_A, B) -> B.

testALL() ->

  X = btree:initBT(),
  X1 = btree:insertBT(X, 10),
  X2 = btree:insertBT(X1, 20),
  X3 = btree:insertBT(X2, 30),
  X4 = btree:insertBT(X3, 40),
  X5 = btree:insertBT(X4, 50),
  X6 = btree:insertBT(X5, 5),
  io:format("X6 = ~p~n", [sumBT(X6)]),
  io:format("X6 = ~p~n", [increaseBT(X6)]),
  io:format("X6 = ~p~n", [filtrationTree(fun(T) -> case T of 20 -> false; _ -> true end end, X6)]).

