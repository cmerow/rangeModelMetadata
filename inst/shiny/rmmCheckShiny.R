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

      # Application title.
      titlePanel("More Widgets"),

      # Sidebar with controls to select a dataset and specify the
      # number of observations to view. The helpText function is
      # also used to include clarifying text. Most notably, the
      # inclusion of a submitButton defers the rendering of output
      # until the user explicitly clicks the button (rather than
      # doing it immediately when inputs change). This is useful if
      # the computations required to render output are inordinately
      # time-consuming.
      sidebarLayout(
        sidebarPanel(
          selectInput("dataset", "Choose a dataset:",
                      choices = c("rock", "pressure", "cars")),

          numericInput("obs", "Number of observations to view:", 10),

          helpText("Note: while the data view will show only the specified",
                   "number of observations, the summary will still be based",
                   "on the full dataset."),

          submitButton("Update View")
        ),

        # Show a summary of the dataset and an HTML table with the
        # requested number of observations. Note the use of the h4
        # function to provide an additional header above each output
        # section.
        mainPanel(
          h4("Summary"),
          verbatimTextOutput("summary"),

          h4("Observations"),
          tableOutput("view")
        )
      )
    ),

    server <- function(input, output) {

      # Return the requested dataset
      datasetInput <- reactive({
        switch(input$dataset,
               "rock" = rock,
               "pressure" = pressure,
               "cars" = cars)
      })

      # Generate a summary of the dataset
      output$summary <- renderPrint({
        dataset <- datasetInput()
        summary(dataset)
      })

      # Show the first "n" observations
      output$view <- renderTable({
        head(datasetInput(), n = input$obs)
      })
    }
  )
}
