#' @title Functions Library
#' @description Collection of utility functions used throughout the package.
#' @name utils.R
#' @noRd

# ---- Don't display messages and warnings ---- #
# Suppress package startup messages
tg <- suppressPackageStartupMessages

# ---- Print time ---- #
#' @title Print Current Time with Message
#' @param string A message string to print.
#' @return None
my_print <- function(string) {
  print(paste0(Sys.time(), ": ", string))
}

# ---- Data pipeline ---- #
#' @title Data Processing Pipeline
#' @description Processes a list of features and combines them into a dataframe.
#' @param features A list of Feature objects with processed data.
#' @return A combined dataframe with processed features.
pipeline <- function(features) {
  for (feature in features) {
    feature$process()
  }
  df_features <- merge(
    features[[1]]$data_processed,
    features[[2]]$data_processed
  ) %>% arrange(index)
  return(df_features)
}

# ---- Plot emission matrix ---- #
#' @title Plot Emission Matrix
#' @description Plots an emission matrix with means and standard deviations.
#' @param emiss A dataframe representing the emission matrix.
#' @return A ggplot object.
plot_emiss <- function(emiss) {
  p <- ggplot(emiss, aes(x = state)) +
    geom_bar(aes(y = mean, fill = mean), stat = "identity", col = "black") +
    geom_errorbar(aes(y = mean, ymin = mean-sd, ymax = mean+sd), stat = "identity", width = 0.4) +
    scale_fill_gradient(low = "green", high = "red") +
    theme_bw() + theme(legend.position = "none")
  return(p)
}

# ---- Plot transition ---- #
#' @title Plot Transition Graph
#' @description Creates a network graph from transition data.
#' @param links A dataframe of links (edges).
#' @param nodes A dataframe of nodes (vertices).
#' @param emiss A dataframe representing the emission matrix.
#' @return A network graph plot.
plot_trans <- function(links, nodes, emiss) {
  net <- graph_from_data_frame(
    d = links,
    vertices = nodes,
    directed = TRUE
  )
  net <- simplify(net, remove.multiple = FALSE, remove.loops = TRUE)
  E(net)$width <- E(net)$weight * 50
  V(net)$size <- 50
  values <- lapply(apply(emiss, 1, function(x) {
    data.frame(x)
  }), function(y) {
    t(y)
  })

  if (nrow(nodes) == 3) {
    col <- c("green", "orange", "red")
  } else if (nrow(nodes) == 2) {
    col <- c("green", "red")
  } else {
    col <- sample(grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)], nrow(nodes))
  }
  set.seed(133)
  plot(
    net,
    edge.curved = .5,
    vertex.shape = "pie",
    vertex.pie.color = col,
    vertex.label = V(net)$etat,
    vertex.label.color = "black",
    vertex.frame.color = "#ffffff",
    edge.arrow.size = 1,
    vertex.label.font = 2,
    layout = layout_in_circle(net),
    edge.label = paste(round(E(net)$weight * 100, 2), "%")
  )
}


# ---- Add Missing Days ---- #
#' @title Add Missing Days to Data
#' @description Ensures the data has complete daily entries for each ID.
#' @param data A dataframe containing `id`, `date`, and `load` columns.
#' @return A dataframe with missing days added.
add_missing_days <- function(data) {
  data <- data.frame(data.table::rbindlist(by(data, data$id, function(x) {
    complet <- data.frame(
      id = unique(x$id),
      date = as.Date(seq(as.Date(min(x$date)), as.Date(max(x$date)), by = "1 day")),
      load = NA
    )
    x$date <- as.Date(x$date)
    df_complet <- left_join(complet, x, by = c("id", "date")) %>%
      rename(load = load.y) %>%
      select(-load.x)
    return(df_complet)
  })))
  return(data)
}
