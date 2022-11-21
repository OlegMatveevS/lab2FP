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
```
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
  ```
  
  + __Функция удаления узла дерева__
  ```
deleteBT({W, LTree, RTree, H}, Ele) ->
  case (Ele < W) of
    true -> case (isEmptyBT(LTree)) of
              true -> {W, LTree, RTree, H};
              false -> {L_W, L_LT, L_RT, L_H} = deleteBT(LTree, Ele),
                case (H > L_H) of
                  true -> {W, {L_W, L_LT, L_RT, L_H}, RTree, H};
                  false -> {W, {L_W, L_LT, L_RT, L_H}, RTree, H - 1}
                end
            end;

    false -> case (isEmptyBT(RTree)) of
               true -> {W, LTree, RTree, H};
               false -> {R_W, R_LT, R_RT, R_H} = deleteBT(RTree, Ele),
                 case (H > R_H) of
                   true -> {W, LTree, {R_W, R_LT, R_RT, R_H}, H};
                   false -> {W, LTree, {R_W, R_LT, R_RT, R_H}, H - 1}
                 end
             end
  end.
  
  + __Функция поиска уровня узла по значению__
  ```
  % вернуть высоту узла если он равен значению элемента пройти лево и право
findBT({W, LTree, RTree, H}, Ele) ->
  case (Ele == W) of
    true -> H;
    false -> case (Ele < W) of
               true -> case (isEmptyBT(LTree)) of
                         true -> 0;
                         false -> findBT(LTree, Ele)
                       end;
               false -> case (isEmptyBT(RTree)) of
                          true -> 0;
                          false -> findBT(RTree, Ele)
                        end
             end
  end.
  ```

  + __Функция сложения деревьев__
  ```
  addTree({}, {}) -> {};
  addTree({}, Tree) -> Tree;
  addTree(Tree, {}) -> Tree;
  addTree(MasterTree, {W, L, R, _H}) ->
  MasterTree1 = addTree(MasterTree, L),
  MasterTree2 = insertBT(MasterTree1, W),
  addTree(MasterTree2, R).
  ```
  + __Функция фильтрации элементов по функции__
  ```
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
  ```
  
  + __Функции для работы со списком, вывода суммы и операция увеличения элементов__
  ```
  % Сохранение всех значений списка в дерева
  listToTree(List) -> lists:foldl(fun(X, Y) -> insertBT(Y, X) end, {}, List).
  
  % Сохранение всех значений дерева в список
  treeToList({}) -> [];
  treeToList({W, L, R, _H}) -> ([W] ++ treeToList(L) ++ treeToList(R)).

  % сумма всех элементов дерева используя foldl
  sumBT(BT) -> lists:foldl(fun(X, Y) -> X + Y end, 0, treeToList(BT)).

  % увеличение всех элементов дерева
  increaseBT(BT) ->
  lists:foldl(fun(X, Y) -> insertBT(Y, X) end, {}, lists:map(fun(X) -> X + 1 end, treeToList(BT))).
  ```


