#' @title RangeModelMetadata Check in Shiny
#'
#' @description Run shiny app to visualize rmm check functions
#'
#' @details
#' See Examples.
#'
#' @param rmm a range model metadata list
#' @examples
#' rmmCheckShiny(rmm)
#'
#' @return None
#' @author Jamie Kass <jamie.m.kass@@gmail.com>
#' @note This function launches a shiny app in the default web browser
#' @export

rmmCheckShiny <- function() {
  require(shiny)
  shinyApp(
    ui = fluidPage(

      # app title
      titlePanel("RMM Check"),

      # sidebar with controls
      sidebarLayout(
        sidebarPanel(
          fileInput("rmm_in", label = "Load RMMs", accept = c("csv", "rds"), multiple = TRUE),
          helpText("Note: RMMs can be loaded as either a list of RMMs saved as
                   .rds or one or more .csv files."),
          strong("Compare RMMs"),
          br(), br(),
          actionButton("rmm_compare", "Go"),
          helpText("Note: TBD.")
        ),

        # Show a summary of the dataset and an HTML table with the
        # requested number of observations. Note the use of the h4
        # function to provide an additional header above each output
        # section.
        mainPanel(
          h4("Check Summary"),
          verbatimTextOutput("rmmCheckOut")

          # h4("Observations"),
          # tableOutput("view")
        )
      )
    ),

    server = function(input, output) {


      readRMM <- reactive({
        getExt <- function(x) strsplit(basename(x), "\\.")[[1]][2]
        exts <- unname(sapply(input$rmm_in$datapath, getExt))
        sameExtTest <- rep(exts[1], length(exts))
        identicalTest <- identical(exts, sameExtTest)

        if (identicalTest == TRUE) {
          if (exts[1] == "csv") {
            return(lapply(input$rmm_in$datapath, read.csv))
          }
          if (exts[1] == "rds") {
            return(readRDS(input$rmm_in$datapath))
          }
        } else {
          message("ERROR: Please make sure all the input files are of the same type.")
          return()
        }
      })

      # checkRMM <- reactive({
      #   if (!is.null(readRMM())) {
      #
      #   } else {
      #     return()
      #   }
      #
      # })

      # observe(print(readRMM()))

      output$rmmCheckOut <- renderPrint({
        if (!is.null(readRMM())) {
          cat("-------------------------------")
          cat("\n")
          for (i in 1:length(readRMM())) {
            cat(paste("Performing all checks on RMM", i, "..."))
            cat("\n")
            rmmCheckFinalize(readRMM()[[i]])
            cat("All checks complete.")
            cat("\n")
            cat("-------------------------------")
            cat("\n")
          }
        }

      })

  #     # Return the requested dataset
  #     datasetInput <- reactive({
  #       switch(input$dataset,
  #              "rock" = rock,
  #              "pressure" = pressure,
  #              "cars" = cars)
  #     })
  #
  #     # Generate a summary of the dataset
  #     output$summary <- renderPrint({
  #       dataset <- datasetInput()
  #       summary(dataset)
  #     })
  #
  #     # Show the first "n" observations
  #     output$view <- renderTable({
  #       head(datasetInput(), n = input$obs)
  #     })
    }
  )
}
