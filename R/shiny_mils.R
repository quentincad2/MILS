#' Plot the workload
#'
#' Function to plot the workload of an athlete.
#'
#' @param ath athlete id.
#' @param data a data.frame containing workload data.
#'
#' @return plot_workload() returns a ggplot2 object.
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom scales rescale
#' @export
plot_workload <- function(ath, data){

  ## ---- Data management ---- ##
  data <- data %>% filter(id == ath)
  data <- data %>% filter(
    date > data$date[min(which(!is.na(data$load)))] &
      date < data$date[max(which(!is.na(data$load)))]
  ) %>% mutate(date = as.Date(date))

  ## -- Plot -- ##
  p <- ggplot(data = data) +
    geom_rect(aes(xmin = date, xmax = date + 1, ymin = 0, ymax = rescale(load, to = c(0,1)),
                  fill = load, text = paste("Date:",date,
                                            "<br>Load:",load,
                                            "<br>Load rescaled:",round(rescale(load, to = c(0,1)),2))),
              stat = "identity") +
    scale_x_date(date_breaks = "1 month", date_minor_breaks = "2 week") +
    theme_bw() + labs(title = as.character(ath)) +
    scale_fill_gradient(low = "cyan", high = "blue") +
    theme(axis.text.x = element_text(size = 10),
          axis.title.y = element_text(size = 14),
          axis.text.y = element_text(size = 10),
          plot.title = element_text(hjust = 0.5, size = 16),
          legend.position = "none") +
    ylab("Normalized \n workload")

  return(p)
}


#' Plot the workload
#'
#' Function to plot the workload of an athlete.
#'
#' @param ath athlete id.
#' @param data a data.frame containing workload and state data.
#' @param n_state the number of state.
#'
#' @return plot_state() returns a ggplot2 object.
#'
#' @import ggplot2
#' @import dplyr
#' @import grDevices
#' @export
plot_state <- function(ath, data, n_state){

  ## ---- Data management ---- ##
  data <- data %>% filter(id == ath)
  data <- data %>% filter(
    date > data$date[min(which(!is.na(data$load)))] &
      date < data$date[max(which(!is.na(data$load)))]
  ) %>% mutate(date = as.Date(date))

  ## ---- Plot ---- ##
  if (n_state == 3){
    col <- c("green","orange","red")
  } else if (n_state == 2){
    col <- c("green","red")
  } else {
    col <- sample(grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)], n_state)
  }
  p <- ggplot(data = data) +
    scale_x_date(date_breaks = "1 month", date_minor_breaks = "2 week") +
    theme_bw() +
    theme(axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title.y = element_text(size = 12))+
    geom_rect(aes(xmin = date, xmax = date + 1, ymin = 0, ymax = S1, text = paste("Date:",date,
                                                                                  "<br>State 1:",round(S1,2))),
              fill = col[1], alpha = 0.5) +
    ylab("MILS state \n probability")

  if (n_state > 1){
    for (i in 2:n_state){
      done_states <- paste0("S",unique(c(1:(i-1))))
      state <- paste0("S",i)
      p <- p +
        geom_rect(aes_string(xmin = "date", xmax = "date + 1" , ymin = paste0(done_states, collapse = "+"),
                             ymax = paste0(paste0(done_states, collapse = "+"), "+", state),
                             text = shQuote(paste0("State ",i))),
                  alpha = 0.5, fill = col[i])
    }
  }

  return(p)
}


#' bs4Dash box
#'
#' Function creating a bs4Dash box.
#'
#' @param ... bs4Dash box arguments.
#' @param title a title.
#' @param width the width of the box (12 by default).
#' @param collapsible boolean. If TRUE, display a button in the upper right that allows the user to collapse the box (TRUE by default).
#' @param maximizable boolean. If TRUE, the card can be displayed in full screen mode (TRUE by default).
#' @param closable boolean.	If TRUE, display a button in the upper right that allows the user to close the box (FALSE by default).
#'
#' @return my_box() returns a HTML object.
#'
#' @importFrom shiny tagList
#' @importFrom bs4Dash box
#' @export
my_box <- function(..., title, width = 12, collapsible = TRUE, maximizable = TRUE, closable = FALSE){

  tagList(
    bs4Dash::box(..., title = title, width = width, collapsible = collapsible, maximizable = maximizable, closable = closable),
    bs4Dash_deps
  )
}

#' Import bs4Dash dependencies
#'
#' HMTL object containing all bs4Dash dependencies.
#'
#' @return bs4Dash_ui is a HTML object.
#'
#' @importFrom bs4Dash dashboardPage dashboardHeader dashboardSidebar dashboardBody
#' @export
bs4Dash_ui <- bs4Dash::dashboardPage(

  bs4Dash::dashboardHeader(),
  bs4Dash::dashboardSidebar(),
  bs4Dash::dashboardBody()
)
#' Get bs4Dash dependencies
#'
#' List containing all bs4Dash dependencies.
#'
#' @return bs4Dash_deps is a list of html_dependency.
#'
#' @importFrom htmltools findDependencies
#' @export
bs4Dash_deps <- htmltools::findDependencies(bs4Dash_ui)

## ------------------------------------------------- ##
## ------------------- Shiny app ------------------- ##
## ------------------------------------------------- ##

#' Explore a Markov index load state.
#'
#' Function displaying a shiny app to explore a Markov index load state.
#'
#' @param result a dataframe containing workload and state data.
#'
#' @return emils() returns a shiny app.
#'
#' @import shiny
#' @import plotly
#' @importFrom shinyjs hidden show useShinyjs
#' @importFrom shinyWidgets sendSweetAlert
#' @export
emils <- function(result){

  #### ---- UI part ---- ####
  ui <- tagList(
    fluidPage(
      useShinyjs(),

      my_box(
        title = "", width = 12, collapsible = TRUE, closable = FALSE,  maximizable = FALSE,
        fluidRow(
          column(8,selectizeInput("athlete", "Athlete :" , choices = NULL, multiple = FALSE, options = list(placeholder = "Selectionnez au moins un individu"), width = "100%")),
          column(2,div(actionButton("import", "Update"), style = "padding-top: 30px;"))
        )
      ),

      hidden(
        div(
          id = "body",
          my_box(
            title = "", width = 12, collapsible = TRUE, closable = FALSE,
            fluidRow(
              plotlyOutput("workload", height = "600px")
            )
          ),
          my_box(
            title = "", width = 12, collapsible = TRUE, closable = FALSE,
            fluidRow(
              plotlyOutput("state", height = "600px")
            )
          )
        )
      )
    )
  )


  #### ---- SERVER part ---- ####
  server <- function(input, output){

    ## ---- Reactive ---- ##
    r <- reactiveValues()

    ## ---- Update input ---- ##
    observe({
      indiv <- unique(result$result$id)
      updateSelectizeInput(
        inputId = "athlete",
        choices = indiv,
        selected = character(0),
        server = TRUE
      )
    })

    ## ---- Import data ---- ##
    observeEvent(input$import,{
      if (length(input$athlete) == 0){
        sendSweetAlert(
          title = "Aucun(e) athlete selectionne(e)",
          text = "Veuillez selectionner au moins un(e) athlete.",
          type = "error"
        )
      } else {
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = "Importation des donnees", detail = "Patientez un instant...", value = 0.5)

        r$data <- result$result %>% filter(id == input$athlete)

        shinyjs::show("body")
        progress$inc(1, detail = "Termine")
      }
    })

    ## ---- Plot ---- ##
    output$workload <- renderPlotly({
      p <- plot_workload(input$athlete, result$result)

      ggplotly(p, tooltip = "text")
    })

    output$state <- renderPlotly({
      p <- plot_state(input$athlete, result$result, length(unique(result$result$state)))

      ggplotly(p, tooltip = "text")
    })

  }


  #### ---- Launch the app ---- ####
  shinyApp(ui, server)
}

