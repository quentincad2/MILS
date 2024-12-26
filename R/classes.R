#' @title Classes for Data Handling
#' @description R6 classes for handling and processing data in the package.
#' @name classes.R
#' @noRd
#' @import R6

# ---- Classe Day ---- #
#' @title Day Class
#' @description Handles daily data processing and keeps specific columns.
Day <- R6Class(
  classname = "Day",
  inherit = Features,
  public = list(
    kept_columns = NULL,

    # ---- Initialization ---- #
    #' @description Initializes a Day object.
    #' @param data_raw A dataframe with raw data.
    #' @param data_processed A dataframe with processed data.
    initialize = function(data_raw = data.frame(), data_processed = data.frame()) {
      super$initialize(data_raw, data_processed)
      self$kept_columns <- c(self$index, "date")
    },

    # ---- Process the data ---- #
    #' @description Processes the raw data and keeps relevant columns.
    process = function() {
      data <- self$data_raw
      data$date <- as.Date(data$date)
      self$data_processed <- data[, self$kept_columns]
    }
  )
)

# ---- Classe Load ---- #
#' @title Load Class
#' @description Handles load data processing, including scaling and NA handling.
Load <- R6Class(
  classname = "Load",
  inherit = Features,
  public = list(
    kept_columns = NULL,

    # ---- Initialization ---- #
    #' @description Initializes a Load object.
    #' @param data_raw A dataframe with raw data.
    #' @param data_processed A dataframe with processed data.
    initialize = function(data_raw = data.frame(), data_processed = data.frame()) {
      super$initialize(data_raw, data_processed)
      self$kept_columns <- c(self$index, "load", "load_scaled")
    },

    # ---- Process the data ---- #
    #' @description Processes the raw load data, handles missing values, and scales the load.
    process = function() {
      data <- self$data_raw
      data$load <- as.numeric(data$load)

      # ---- Remove NA at the start and end for each ID ---- #
      data <- data.frame(data.table::rbindlist(by(data, data$id, function(x) {
        x <- x %>%
          dplyr::filter(
            index >= x$index[min(which(!is.na(x$load)))] &
              index <= x$index[max(which(!is.na(x$load)))]
          )
        return(x)
      })))

      # ---- Scale the load for each ID ---- #
      dt <- data.frame(data.table::rbindlist(by(data, data$id, function(x) {
        x$load_scaled <- scale(x$load)
        return(x)
      })))

      self$data_processed <- dt[, self$kept_columns]
    }
  )
)
