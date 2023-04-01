# Установка пакета
install.packages("igraph")
library(igraph)


start_v <- 1
# Создание графа
g <- graph(c(1,2,
             8,3,
             3,7,
             4,5,
             1,4,
             #5,1,
             4,6,
             1,8), n=8)
# Рисование графа
#V(g)$label <- V(g)$name
V(g)$size <- 13
E(g)$color <- "gray"
#set.seed(13)
plot(g, layout=layout.reingold.tilford)
# Разные layout:
#circle
#sphere
#reingold.tilford
#random
#fruchterman.reingold
#kamada.kawai
#lgl


# BFS
bfs <- function(graph, start) {
  # Список непосещённых вершин(для current шага)
  queue <- start
  # Список непосещённых вершин(для next шага)
  visited <- c()
  # Пока есть непосещенные вершины
  while (length(queue) > 0) {
    # Достаем первую вершину из очереди
    node <- queue[1]
    # Удаляем её из очереди
    queue <- queue[-1]
    # Если вершина не была посещена
    if (!(node %in% visited)) {
      # Добавляем её в список посещённых
      visited <- c(visited, node)
      # Добавляем непосещённых соседей в список посещённых вершин 
      #(с помощью функции из igraph находим соседей)
      neighbors <- neighbors(graph, node)
      # Добавляем в список queue непосещенных соседей, которые еще не содержатся
      #в списке посещенных вершин
      queue <- c(queue, setdiff(neighbors, visited))
    }
  }
  # Возвращение посещённых вершин, за исключением начальной
  return(visited[-1])
}


# DFS
dfs <- function(graph, start) {
  # Список непосещённых вершин(для current шага)
  stack <- start
  # Список непосещённых вершин(для next шага)
  visited <- c()
  # Пока есть непосещенные вершины
  while (length(stack) > 0) {
    # Достаем последнюю вершину из списка непосещённых
    node <- stack[length(stack)]
    stack <- stack[-length(stack)]
    # Если вершина не была посещена
    if (!(node %in% visited)) {
      # Добавляем её в список посещённых
      visited <- c(visited, node)
      # Добавляем непосещённых соседей в список посещённых вершин
      neighbors <- neighbors(graph, node)
      stack <- c(stack, setdiff(neighbors, visited))
    }
  }
  # Return the visited nodes
  return(rev(visited[-1]))
}


# BFS algo
bfs_result <- bfs(g, start_v)
cat('Последовательность посещения вершин(BFS)', bfs_result, '\n')

# DFS algo
dfs_result <- dfs(g, start_v)
cat('Последовательность посещения вершин(DFS)', dfs_result, '\n')