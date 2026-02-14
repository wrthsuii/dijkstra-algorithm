# Реалізація на Haskell

## Опис

Функційна реалізація алгоритму Дейкстри з використанням чистих функцій та незмінних структур даних. Граф представлено як `Map Vertex [(Vertex, Weight)]`. Композиція функцій, рекурсія замість циклів, сильна статична типізація.

**Головна функція:** `shortestPath :: Graph -> Vertex -> Vertex -> Maybe ([Vertex], Weight)`

## Особливості

- Незмінні структури (Map, List)
- Чисті функції без побічних ефектів
- Функції вищого порядку (foldr, minimumBy)
- Монада Maybe для безпеки
- Рекурсія замість циклів

## Приклад

```haskell
main = case shortestPath exampleGraph "A" "E" of
    Just (path, dist) -> print (path, dist)
    Nothing -> putStrLn "Path not found"
-- Output: (["A","C","B","D","E"], 7)
```

## Запуск

```bash
ghc djikstra.hs -o dijkstra
./dijkstra
```
