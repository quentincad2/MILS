#' @title MILS (Multivariate Individual Load State)
#' @description Fits a hidden Markov model to analyze individual workload states.
#' @param data A data frame with columns: id (athlete ID), date, and load.
#' @param n_obs Integer. Minimum number of observations required per athlete. Default is 2.
#' @param n_states Integer. Number of hidden states in the model. Default is 3.
#' @param family Character. Family of the distribution used in the model (e.g., "gaussian"). Default is "gaussian".
#' @param init Logical. Whether to use initial parameters. Default is FALSE.
#' @param state_name Character vector. Names of the states. Default is NULL.
#' @return A list containing the fitted model, processed data, and plots.
#' @import dplyr ggplot2 gridExtra reshape2 depmixS4
#'
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @import gridExtra
#' @import reshape2
#' @import stats
#' @import data.table
#' @import igraph
#'
#' @return une liste avec pleins de choses super dedans
#' @export
#' @examples NULL
mils <- function(data, n_obs = 2, n_states = 3, family = "gaussian", init = FALSE, state_name = NULL) {

  colnames(data) <- c("id", "date", "load")

  # Default state names
  if (is.null(state_name)) {
    state_name <- paste0("S", 1:n_states)
  }

  # ---- Data Preprocessing ---- #
  data <- add_missing_days(data)

  data <- data %>%
    mutate(
      id = as.character(id),
      date = as.Date(date),
      load = as.numeric(load)
    ) %>%
    group_by(id, date) %>%
    summarise(load = sum(load, na.rm = TRUE), .groups = "keep") %>%
    ungroup() %>%
    mutate(load = ifelse(load == 0, NA, load)) %>%
    arrange(id, date) %>%
    mutate(index = row_number()) %>%
    relocate(index)

  # ---- Feature Initialization ---- #
  date <- Day$new()
  date$get_data(data)
  load <- Load$new()
  load$get_data(data)
  load$process()

  list_features <- list(date, load)
  df <- pipeline(list_features)

  # Remove athletes with insufficient data
  n <- df %>% group_by(id) %>% count()
  ath <- n %>% dplyr::filter(n <= n_obs)
  if (nrow(ath) > 0) {
    df <- df %>% dplyr::filter(!id %in% ath$id)
  }

  # ---- Model Setup ---- #
  n <- df %>% group_by(id) %>% count()
  n_times <- n$n
  states <- paste0("S", 1:n_states)

  set.seed(1)
  mod <- depmix(
    response = list(load_scaled ~ 1),
    data = df,
    nstates = n_states,
    family = list(get(family)()),
    ntimes = n_times
  )
  if (init) {
    mod <- setpars(mod, values = getpars(graal))
  }

  model <- fit(mod, emc = em.control(maxit = 2000))
  print(model)

  # ---- Results Extraction ---- #
  result <- list()
  para <- getpars(model)

  result$init <- para[1:n_states]
  result$trans <- matrix(
    para[(n_states + 1):(n_states^2 + n_states)],
    byrow = TRUE, nrow = n_states, ncol = n_states
  )
  result$emiss <- matrix(
    para[((n_states^2 + n_states) + 1):((n_states^2 + n_states + 1) + ((n_states * 2) - 1))],
    byrow = TRUE, nrow = n_states, ncol = 2
  )

  ordre <- order(result$emiss[, 1])
  result$emiss <- result$emiss[ordre, ]
  result$trans <- result$trans[ordre, ordre]
  result$init <- result$init[ordre]

  states <- paste0("S", 1:n_states)
  fb <- forwardbackward(model)
  post_data <- data.frame(fb$gamma)
  names(post_data) <- states[ordre]
  result$data <- cbind(df, post_data) %>%
    mutate(state = apply(post_data, 1, function(x) names(which.max(x))))

  # ---- Plotting ---- #
  emiss <- data.frame(result$emiss)
  names(emiss) <- c("mean", "sd")
  emiss$state <- 1:nrow(emiss)
  p1 <- plot_emiss(emiss)
  print(p1)

  links <- melt(result$trans)
  names(links) <- c("from", "to", "weight")
  nodes <- data.frame(
    id = c(1:length(state_name)),
    etat = state_name
  )
  p2 <- plot_trans(links, nodes, emiss)
  print(p2)

  id <- unique(result$data$id)
  p <- list()
  nb_indiv <- ifelse(length(id) >= 4, 4, length(id))
  for (i in 1:nb_indiv) {
    ath <- id[i]
    p[[i]] <- grid.arrange(
      plot_workload(ath, result$data),
      plot_state(ath, result$data, n_states)
    )
  }

  res=cbind(df,model@posterior)

  return(
    list(
      model = model,
      data_process = df,
      plot = p,
      result=res
    )
  )
}
