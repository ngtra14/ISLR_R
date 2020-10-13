## ---- echo=FALSE, warning=FALSE------------------------------------------
suppressPackageStartupMessages(library(microbenchmark, quietly = TRUE))


# Profiling ---------------------------------------------------------------
graph <- function(n, edges) {
  m <- matrix(0, nrow = n, ncol = n)
  no_edges <- length(edges)
  if (no_edges >= 1) {
    for (i in seq(1, no_edges, by = 2)) {
      m[edges[i], edges[i+1]] <- m[edges[i+1], edges[i]] <- 1
    }
  }
  structure(m, class = "graph")
}

smooth_weights <- function(graph, node_weights, alpha) {
  if (length(node_weights) != nrow(graph))
    stop("Incorrect number of nodes")
  no_nodes <- length(node_weights)
  new_weights <- vector("numeric", no_nodes)
  for (i in 1:no_nodes) {
    neighbour_weights <- 0
    n <- 0
    for (j in 1:no_nodes) {
      if (i != j && graph[i, j] == 1) {
        neighbour_weights <- neighbour_weights + node_weights[j]
        n <- n + 1
      }
    }
    if (n > 0) {
      new_weights[i] <-
        alpha * node_weights[i] +
        (1 - alpha) * neighbour_weights / n
    } else {
      new_weights[i] <- node_weights[i]
    }
  }
  new_weights
}

library(profvis)
profvis::profvis({
  n <- 1000
  nodes <- 1:n
  edges <- sample(nodes, 600, replace = TRUE)
  weights <- rnorm(n)
  g <- graph(n, edges)
  smooth_weights(g, weights, 0.8)
})


# represent the edges as a list  ------------------------------------------

graph <- function(n, edges) {
  neighbours <- vector("list", length = n)
  for (i in seq_along(neighbours)) {
    neighbours[[i]] <- vector("integer", length = 0)
  }
  no_edges <- length(edges)
  if (no_edges >= 1) {
    for (i in seq(1, no_edges, by = 2)) {
      n1 <- edges[i]
      n2 <- edges[i+1]
      neighbours[[n1]] <- c(n2, neighbours[[n1]])
      neighbours[[n2]] <- c(n1, neighbours[[n2]])
    }
  }
  for (i in seq_along(neighbours)) {
    neighbours[[i]] <- unique(neighbours[[i]])
  }
  structure(neighbours, class = "graph")
}

smooth_weights <- function(graph, node_weights, alpha) {
  if (length(node_weights) != length(graph))
    stop("Incorrect number of nodes")
  no_nodes <- length(node_weights)
  new_weights <- vector("numeric", no_nodes)
  for (i in 1:no_nodes) {
    neighbour_weights <- 0
    n <- 0
    for (j in graph[[i]]) {
      if (i != j) {
        neighbour_weights <- neighbour_weights + node_weights[j]
        n <- n + 1
      }
    }
    if (n > 0) {
      new_weights[i] <-
        alpha * node_weights[i] +
        (1 - alpha) * neighbour_weights / n
    } else {
      new_weights[i] <- node_weights[i]
    }
  }
  new_weights
}

profvis::profvis({
  n <- 10000
  nodes <- 1:n
  edges <- sample(nodes, 1200, replace = TRUE)
  weights <- rnorm(n)
  g <- graph(n, edges)
  smooth_weights(g, weights, 0.8)
})


#  remove all the duplicated edges  ---------------------------------------
graph <- function(n, edges) {
  neighbours <- vector("list", length = n)
  for (i in seq_along(neighbours)) {
    neighbours[[i]] <- vector("integer", length = 0)
  }
  no_edges <- length(edges)
  if (no_edges >= 1) {
    sources <- seq(1, no_edges, by = 2)
    destinations <- seq(2, no_edges, by = 2)
    edge_matrix <- matrix(NA, nrow = length(sources), ncol = 2)
    edge_matrix[,1] <- edges[sources]
    edge_matrix[,2] <- edges[destinations]
    for (i in 1:nrow(edge_matrix)) {
      if (edge_matrix[i,1] > edge_matrix[i,2]) {
        edge_matrix[i,] <- c(edge_matrix[i,2], edge_matrix[i,1])
      }
    }
    edge_matrix <- unique(edge_matrix)
    for (i in seq(1, nrow(edge_matrix))) {
      n1 <- edge_matrix[i, 1]
      n2 <- edge_matrix[i, 2]
      neighbours[[n1]] <- c(n2, neighbours[[n1]])
      neighbours[[n2]] <- c(n1, neighbours[[n2]])
    }
  }
  structure(neighbours, class = "graph")
}

flow_weights_iteration <- function(graph, node_weights, alpha) {
  if (length(node_weights) != length(graph))
    stop("Incorrect number of nodes")
  no_nodes <- length(node_weights)
  new_weights <- vector("numeric", n)
  for (i in 1:no_nodes) {
    neighbour_weights <- 0
    n <- 0
    for (j in graph[[i]]) {
      if (i != j) {
        neighbour_weights <- neighbour_weights + node_weights[j]
        n <- n + 1
      }
    }
    if (n > 0) {
      new_weights[i] <- (alpha * node_weights[i] + (1 - alpha)
                         * neighbour_weights / n)
    } else {
      new_weights[i] <- node_weights[i]
    }
  }
  new_weights
}

smooth_weights <- function(graph, node_weights, alpha, no_iterations) {
  new_weights <- node_weights
  replicate(no_iterations, {
    new_weights <- flow_weights_iteration(graph, new_weights, alpha)
  })
  new_weights
}

profvis::profvis({
  n <- 20000
  nodes <- 1:n
  edges <- sample(nodes, 100000, replace = TRUE)
  weights <- rnorm(n)
  g <- graph(n, edges)
  smooth_weights(g, weights, 0.8, 10)
})

## ------------------------------------------------------------------------

# Speeding Up Your Code ---------------------------------------------------


mysum <- function(sequence) {
  s <- 0
  for (x in sequence) s <- s + x
  s
}

microbenchmark(
  sum(1:10),
  mysum(1:10)
)

## ------------------------------------------------------------------------
microbenchmark(
  sum(1:10),
  mysum(1:10),
  Reduce(`+`, 1:10, 0)
)

## ------------------------------------------------------------------------
x <- sample(LETTERS, 1000, replace = TRUE)
microbenchmark(
  factor(x, levels = LETTERS),
  factor(x)
)

## ------------------------------------------------------------------------
x <- rnorm(1000)
names(x) <- paste("n", 1:1000)
microbenchmark(
  unlist(Map(function(x) x**2, x), use.names = FALSE),
  unlist(Map(function(x) x**2, x))
)

