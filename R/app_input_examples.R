phase2_example_data <- function() {
  matrix_a <- data.frame(
    S1 = c(9, 8, 1, 2, 7, 6),
    S2 = c(8, 9, 2, 1, 6, 7),
    S3 = c(1, 2, 9, 8, 3, 4),
    S4 = c(2, 1, 8, 9, 4, 3),
    S5 = c(7, 6, 3, 4, 9, 8),
    row.names = paste0("OTU", 1:6),
    check.names = FALSE
  )

  matrix_b <- data.frame(
    S1 = c(3, 4, 9, 8, 2, 1),
    S2 = c(4, 3, 8, 9, 1, 2),
    S3 = c(9, 8, 3, 4, 7, 6),
    S4 = c(8, 9, 4, 3, 6, 7),
    S5 = c(2, 1, 7, 6, 8, 9),
    row.names = paste0("Gene", 1:6),
    check.names = FALSE
  )

  edges <- data.frame(
    source = c("OTU1", "OTU1", "OTU2", "OTU3", "OTU4", "OTU5"),
    target = c("OTU2", "OTU5", "OTU6", "OTU4", "OTU6", "OTU6"),
    weight = c(0.82, 0.58, -0.41, 0.77, 0.49, 0.66),
    check.names = FALSE
  )

  modules <- data.frame(
    node = paste0("OTU", 1:6),
    module = c("A", "A", "B", "B", "C", "C"),
    check.names = FALSE
  )

  sample_metadata <- data.frame(
    Sample = paste0("S", 1:5),
    Group = c("Early", "Early", "Late", "Late", "Late"),
    Batch = c("B1", "B1", "B2", "B2", "B2"),
    check.names = FALSE
  )

  adjacency <- matrix(0, nrow = 6, ncol = 6, dimnames = list(paste0("OTU", 1:6), paste0("OTU", 1:6)))
  adjacency[cbind(c(1, 1, 2, 3, 4, 5), c(2, 5, 6, 4, 6, 6))] <- c(0.82, 0.58, -0.41, 0.77, 0.49, 0.66)
  adjacency <- adjacency + t(adjacency)
  diag(adjacency) <- 0

  tom <- adjacency
  tom[tom < 0] <- abs(tom[tom < 0])
  diag(tom) <- 1

  set.seed(1115)
  rmt_matrix <- matrix(
    stats::rpois(120 * 30, lambda = 20),
    nrow = 120,
    ncol = 30,
    dimnames = list(paste0("RMT", seq_len(120)), paste0("S", seq_len(30)))
  )

  list(
    matrix_a = matrix_a,
    matrix_b = matrix_b,
    rmt_matrix = as.data.frame(rmt_matrix, check.names = FALSE),
    edges = edges,
    modules = modules,
    sample_metadata = sample_metadata,
    adjacency = as.data.frame(adjacency, check.names = FALSE),
    tom = as.data.frame(tom, check.names = FALSE)
  )
}

write_phase2_example_data <- function(dir = file.path("inst", "extdata")) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  data <- phase2_example_data()
  utils::write.csv(data$matrix_a, file.path(dir, "phase2_example_matrix.csv"), quote = FALSE)
  utils::write.csv(data$matrix_b, file.path(dir, "phase2_example_matrix_b.csv"), quote = FALSE)
  utils::write.csv(data$rmt_matrix, file.path(dir, "phase2_example_rmt_matrix.csv"), quote = FALSE)
  utils::write.csv(data$edges, file.path(dir, "phase2_example_edges.csv"), row.names = FALSE, quote = FALSE)
  utils::write.csv(data$modules, file.path(dir, "phase2_example_modules.csv"), row.names = FALSE, quote = FALSE)
  utils::write.csv(data$sample_metadata, file.path(dir, "phase2_example_sample_metadata.csv"), row.names = FALSE, quote = FALSE)
  utils::write.csv(data$adjacency, file.path(dir, "phase2_example_adjacency.csv"), quote = FALSE)
  utils::write.csv(data$tom, file.path(dir, "phase2_example_tom.csv"), quote = FALSE)
  invisible(normalizePath(dir, mustWork = FALSE))
}
