% ============================================================================
% DIJKSTRA ALGORITHM - SHORTEST PATH IN WEIGHTED GRAPH
% Authors: Avdieienko Dmytro (main), Aleksieienko Anastasiia, Tytarenko Vladyslava
% Implementation of Dijkstra's algorithm in Prolog
% ============================================================================

% ============================================================================
% GRAPH REPRESENTATION
% ============================================================================

% Граф представлено як список ребер: edge(From, To, Weight)
% Граф є неорієнтованим - ребро (A,B) означає можливість руху в обидва боки
% Приклад тестового графу:

edge(a, b, 7).
edge(a, c, 9).
edge(a, f, 14).
edge(b, c, 10).
edge(b, d, 15).
edge(c, d, 11).
edge(c, f, 2).
edge(d, e, 6).
edge(e, f, 9).

% ============================================================================
% MAIN PREDICATE: DIJKSTRA
% ============================================================================

% dijkstra(++Start, ++End, --Path, --Distance)
% Знаходить найкоротший шлях від Start до End у зваженому графі
% Start - початкова вершина графу
% End - кінцева вершина графу
% Path - список вершин найкоротшого шляху [Start, ..., End]
% Distance - загальна вага (довжина) шляху
%
% МУЛЬТИПРИЗНАЧЕННІСТЬ:
% 1. dijkstra(++, ++, --, --) - знайти шлях і відстань (основне призначення)
%    Приклад: dijkstra(a, e, Path, Dist) → Path=[a,c,f,e], Dist=20
% 2. dijkstra(++, ++, ++, --) - перевірити коректність шляху, знайти відстань
%    Приклад: dijkstra(a, e, [a,c,f,e], Dist) → Dist=20
% 3. dijkstra(++, ++, ++, ++) - перевірити шлях і відстань
%    Приклад: dijkstra(a, e, [a,c,f,e], 20) → true/false
% НЕ ЗМІСТОВНІ:
% - dijkstra(--, ++, ...) - початкова вершина має бути відома
% - dijkstra(++, --, ...) - кінцева вершина має бути відома
% - dijkstra(--, --, ...) - обидві вершини мають бути відомі

dijkstra(Start, End, Path, Distance) :-
    findall(V, (edge(V, _, _); edge(_, V, _)), Vertices),
    sort(Vertices, UniqueVertices),
    dijkstra_init(UniqueVertices, Start, InitDistances, InitPrevious),
    dijkstra_main(InitDistances, InitPrevious, [], FinalPrevious),
    reconstruct_path(Start, End, FinalPrevious, [], Path),
    get_distance(End, FinalPrevious, Distance).

/** <examples>
?- dijkstra(a, e, Path, Dist).
Path = [a, c, f, e],
Dist = 20.

?- dijkstra(a, d, Path, Dist).
Path = [a, c, d],
Dist = 20.

?- dijkstra(b, f, Path, Dist).
Path = [b, c, f],
Dist = 12.

?- dijkstra(a, e, [a,c,f,e], Dist).
Dist = 20.

?- dijkstra(a, e, [a,c,f,e], 20).
true.
*/

% ============================================================================
% INITIALIZATION
% ============================================================================

% dijkstra_init(++Vertices, ++Start, --Distances, --Previous)
% Ініціалізація структур даних для алгоритму Дейкстри
% Vertices - список всіх вершин графу
% Start - початкова вершина
% Distances - список пар [vertex-distance] з початковими відстанями
% Previous - список пар [vertex-prev] для відновлення шляху
%
% МУЛЬТИПРИЗНАЧЕННІСТЬ:
% 1. dijkstra_init(++, ++, --, --) - ініціалізувати структури (єдине призначення)
% НЕ ЗМІСТОВНІ:
% - Всі інші комбінації - Vertices та Start мають бути конкретизовані
%
% Початкова вершина Start отримує відстань 0, всі інші - infinity

dijkstra_init(Vertices, Start, Distances, Previous) :-
    dijkstra_init_helper(Vertices, Start, Distances, Previous).

% dijkstra_init_helper(++Vertices, ++Start, --Distances, --Previous)
% Допоміжний предикат для ініціалізації
%
% МУЛЬТИПРИЗНАЧЕННІСТЬ:
% 1. dijkstra_init_helper(++, ++, --, --) - рекурсивна ініціалізація (єдине)
% НЕ ЗМІСТОВНІ:
% - Вхідні параметри мають бути конкретизовані

dijkstra_init_helper([], _, [], []).
dijkstra_init_helper([V|Vs], Start, [V-Dist|Dists], [V-none|Prevs]) :-
    (V = Start -> Dist = 0 ; Dist = inf),
    dijkstra_init_helper(Vs, Start, Dists, Prevs).

/** <examples>
?- dijkstra_init([a,b,c], a, Distances, Previous).
Distances = [a-0, b-inf, c-inf],
Previous = [a-none, b-none, c-none].
*/

% ============================================================================
% MAIN ALGORITHM LOOP
% ============================================================================

% dijkstra_main(++Distances, ++Previous, ++Visited, --FinalPrevious)
% Основний цикл алгоритму Дейкстри
% Distances - поточні мінімальні відстані до вершин
% Previous - попередні вершини на шляху (для відновлення)
% Visited - список вже оброблених вершин
% FinalPrevious - фінальний список попередніх вершин після завершення
%
% МУЛЬТИПРИЗНАЧЕННІСТЬ:
% 1. dijkstra_main(++, ++, ++, --) - виконати алгоритм (єдине призначення)
% НЕ ЗМІСТОВНІ:
% - Всі вхідні параметри мають бути конкретизовані
%
% Алгоритм завершується коли всі досяжні вершини відвідано

dijkstra_main(Distances, Previous, Visited, Previous) :-
    \+ select_min_unvisited(Distances, Visited, _, _), !.

dijkstra_main(Distances, Previous, Visited, FinalPrevious) :-
    select_min_unvisited(Distances, Visited, MinVertex, MinDist),
    MinDist \= inf,
    findall(Neighbor-Weight,
            (edge(MinVertex, Neighbor, Weight);
             edge(Neighbor, MinVertex, Weight)),
            Neighbors),
    relax_edges(MinVertex, MinDist, Neighbors, Distances, Previous,
                NewDistances, NewPrevious),
    dijkstra_main(NewDistances, NewPrevious, [MinVertex|Visited], FinalPrevious).

% ============================================================================
% HELPER PREDICATES
% ============================================================================

% select_min_unvisited(++Distances, ++Visited, --MinVertex, --MinDist)
% Вибирає невідвідану вершину з мінімальною відстанню
% Distances - список поточних відстаней до вершин
% Visited - список вже оброблених вершин
% MinVertex - вершина з мінімальною відстанню серед необроблених
% MinDist - мінімальна відстань
%
% МУЛЬТИПРИЗНАЧЕННІСТЬ:
% 1. select_min_unvisited(++, ++, --, --) - знайти мінімум (основне)
% 2. select_min_unvisited(++, ++, ++, --) - перевірити вершину, знайти відстань
% 3. select_min_unvisited(++, ++, ++, ++) - перевірити вершину і відстань
% НЕ ЗМІСТОВНІ:
% - select_min_unvisited(--, ...) - Distances мають бути відомі
% - select_min_unvisited(++, --, ...) - Visited мають бути відомі
%
% Використовує findall та sort для ефективного пошуку мінімуму

select_min_unvisited(Distances, Visited, MinVertex, MinDist) :-
    findall(Dist-Vertex,
            (member(Vertex-Dist, Distances),
             \+ member(Vertex, Visited),
             Dist \= inf),
            Available),
    Available \= [],
    sort(Available, [MinDist-MinVertex|_]).

/** <examples>
?- select_min_unvisited([a-0, b-7, c-9], [], MinV, MinD).
MinV = a,
MinD = 0.

?- select_min_unvisited([a-0, b-7, c-9], [a], MinV, MinD).
MinV = b,
MinD = 7.
*/

% relax_edges(++Vertex, ++Dist, ++Neighbors, ++Distances, ++Previous,
%             --NewDistances, --NewPrevious)
% Релаксація ребер - оновлення відстаней через поточну вершину
% Vertex - поточна оброблювана вершина
% Dist - відстань до поточної вершини
% Neighbors - список сусідів у форматі [Neighbor-Weight, ...]
% Distances, Previous - поточні структури даних
% NewDistances, NewPrevious - оновлені структури після релаксації
%
% МУЛЬТИПРИЗНАЧЕННІСТЬ:
% 1. relax_edges(++, ++, ++, ++, ++, --, --) - оновити відстані (єдине)
% НЕ ЗМІСТОВНІ:
% - Всі вхідні параметри мають бути конкретизовані для коректної роботи
%
% Якщо шлях через Vertex коротший - оновлює відстань та попередника

relax_edges(_, _, [], Distances, Previous, Distances, Previous).
relax_edges(Vertex, Dist, [Neighbor-Weight|Rest], Distances, Previous,
            FinalDistances, FinalPrevious) :-
    member(Neighbor-OldDist, Distances),
    NewDist is Dist + Weight,
    (compare_distances(NewDist, OldDist) ->
        update_list(Neighbor-OldDist, Neighbor-NewDist, Distances, UpdatedDistances),
        update_list(Neighbor-_, Neighbor-Vertex, Previous, UpdatedPrevious)
    ;
        UpdatedDistances = Distances,
        UpdatedPrevious = Previous
    ),
    relax_edges(Vertex, Dist, Rest, UpdatedDistances, UpdatedPrevious,
                FinalDistances, FinalPrevious).

% compare_distances(++Dist1, ++Dist2)
% Порівнює дві відстані з урахуванням infinity
% Dist1 - перша відстань для порівняння
% Dist2 - друга відстань для порівняння
%
% МУЛЬТИПРИЗНАЧЕННІСТЬ:
% 1. compare_distances(++, ++) - перевірити чи Dist1 < Dist2 (єдине)
% НЕ ЗМІСТОВНІ:
% - Обидві відстані мають бути конкретизовані
%
% Успішно якщо Dist1 менше Dist2 (infinity найбільше значення)

compare_distances(D1, inf) :- D1 \= inf, !.
compare_distances(D1, D2) :- D1 \= inf, D2 \= inf, D1 < D2.

/** <examples>
?- compare_distances(5, 10).
true.

?- compare_distances(5, inf).
true.

?- compare_distances(10, 5).
false.
*/

% update_list(++OldElement, ++NewElement, ++List, --NewList)
% Замінює перше входження OldElement на NewElement у списку
% OldElement - елемент для заміни
% NewElement - новий елемент
% List - вхідний список
% NewList - список з заміненим елементом
%
% МУЛЬТИПРИЗНАЧЕННІСТЬ:
% 1. update_list(++, ++, ++, --) - замінити елемент (основне)
% 2. update_list(++, ++, ++, ++) - перевірити результат заміни
% НЕ ЗМІСТОВНІ:
% - update_list(--, ...) - OldElement має бути відомий
% - update_list(++, --, ...) - NewElement має бути відомий
% - update_list(++, ++, --, ...) - List має бути відомий

update_list(_, _, [], []).
update_list(Old, New, [Old|T], [New|T]) :- !.
update_list(Old, New, [H|T], [H|NewT]) :-
    update_list(Old, New, T, NewT).

/** <examples>
?- update_list(b-10, b-5, [a-0, b-10, c-15], Result).
Result = [a-0, b-5, c-15].
*/

% ============================================================================
% PATH RECONSTRUCTION
% ============================================================================

% reconstruct_path(++Start, ++End, ++Previous, ++Acc, --Path)
% Відновлює шлях від Start до End використовуючи список попередників
% Start - початкова вершина шляху
% End - кінцева вершина шляху
% Previous - список пар [vertex-previous_vertex]
% Acc - акумулятор для побудови шляху (зворотний порядок)
% Path - результуючий шлях від Start до End
%
% МУЛЬТИПРИЗНАЧЕННІСТЬ:
% 1. reconstruct_path(++, ++, ++, ++, --) - побудувати шлях (основне)
% 2. reconstruct_path(++, ++, ++, ++, ++) - перевірити шлях
% НЕ ЗМІСТОВНІ:
% - Всі вхідні параметри мають бути конкретизовані
%
% Рекурсивно йде від End до Start використовуючи Previous

reconstruct_path(Start, Start, _, Acc, [Start|Acc]) :- !.
reconstruct_path(Start, Current, Previous, Acc, Path) :-
    member(Current-Prev, Previous),
    Prev \= none,
    reconstruct_path(Start, Prev, Previous, [Current|Acc], Path).

/** <examples>
?- reconstruct_path(a, c, [a-none, b-a, c-b], [], Path).
Path = [a, b, c].
*/

% get_distance(++Vertex, ++Previous, --Distance)
% Отримує фінальну відстань до вершини
% Vertex - цільова вершина
% Previous - список попередників з інформацією про шлях
% Distance - загальна відстань до вершини
%
% МУЛЬТИПРИЗНАЧЕННІСТЬ:
% 1. get_distance(++, ++, --) - обчислити відстань (основне)
% 2. get_distance(++, ++, ++) - перевірити відстань
% НЕ ЗМІСТОВНІ:
% - get_distance(--, ...) - Vertex має бути відома
% - get_distance(++, --, ...) - Previous має бути відомий

get_distance(Vertex, Previous, Distance) :-
    member(Vertex-Prev, Previous),
    count_distance(Vertex, Prev, Previous, 0, Distance).

% count_distance(++Current, ++Prev, ++Previous, ++Acc, --Distance)
% Рахує відстань шляхом зворотного проходу по шляху
% Current - поточна вершина
% Prev - попередня вершина на шляху
% Previous - список всіх попередників
% Acc - акумулятор відстані
% Distance - фінальна відстань
%
% МУЛЬТИПРИЗНАЧЕННІСТЬ:
% 1. count_distance(++, ++, ++, ++, --) - підрахувати відстань (єдине)
% НЕ ЗМІСТОВНІ:
% - Всі вхідні параметри мають бути конкретизовані

count_distance(_, none, _, Acc, Acc) :- !.
count_distance(Current, Prev, Previous, Acc, Distance) :-
    (edge(Prev, Current, W); edge(Current, Prev, W)),
    NewAcc is Acc + W,
    member(Prev-PrevPrev, Previous),
    count_distance(Prev, PrevPrev, Previous, NewAcc, Distance).

/** <examples>
?- count_distance(c, b, [a-none, b-a, c-b], 0, Dist).
Dist = 17 (якщо edge(a,b,7) та edge(b,c,10)).
*/

% ============================================================================
% END OF FILE
% ============================================================================
