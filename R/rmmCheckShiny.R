#' @title RangeModelMetadata Check in Shiny
#'
#' @description Run shiny app to visualize rmm check functions
#'
#' @details
#' See Examples.
#'
#' @examples
#' rmm1=rangeModelMetadataTemplate(useCase='apAll')
#' rmm1=rmmAutofillPackageCitation(rmm1,c('raster','sp'))
# rmm1AutoFillData(rmm1,species=) # not used yet
#' r.f=system.file("extdata/Env_Demo",package='rangeModelMetadata')
#' raster.files=list.files(r.f,full.names = TRUE)
#' env=raster::stack(raster.files)
#' # for fitting environment
#' rmm1=rmmAutofillEnvironment(rmm1,env,transfer=0)
#' # for transfer environment 1 (assuming different than for fitting)
#' rmm1=rmmAutofillEnvironment(rmm1,env,transfer=1)
#' # for transfer environment 2 (assuming different than 1)
#' rmm1=rmmAutofillEnvironment(rmm1,env,transfer=2)
#' \dontrun{ rmmCheckShiny(rmm1) }
#'
#' @return None
#' @author Jamie Kass <jamie.m.kass@@gmail.com>
#' @note This function launches a shiny app in the default web browser
#' @export

rmmCheckShiny <- function() {
  # require(shiny) # CM: cran wants you to namespace the functions rather than use require
  shiny::shinyApp(
    ui = shiny::fluidPage(

      # app title
      shiny::titlePanel("Range Model Metadata (RMM) Check"),

      # sidebar with controls
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::fileInput("rmm_in", label = "Load RMMs", accept = c("csv", "rds"), multiple = TRUE),
          # CM: for flexibility, can we also accept .rdata or .rda? This should be no issue now that we have an rmm class, right?
          shiny::helpText("Note: RMMs can be loaded as either a list of RMMs saved as
                   .rds, a single RMM per .rds file, or one or more .csv files. Up to 4 RMMs can be compared at once."),
          # CM: how many can be compared at once now?
          shiny::strong("Compare RMMs"),
          shiny::br(), shiny::br(),
          shiny::actionButton("rmm_compare", "Go"),
          shiny::helpText("Note: TBD.")
        ),

        # Show a summary of the dataset and an HTML table with the
        # requested number of observations. Note the use of the h4
        # function to provide an additional header above each output
        # section.
        shiny::mainPanel(
          shiny::h4("Check Summary"),
          shiny::verbatimTextOutput("rmmCheckOut")

          # h4("Observations"),
          # tableOutput("view")
        )
      )
    ),

    server = function(input, output) {

      # reactive function to read the input file(s), error if they are
      # not the same type, and
      readRMM <- shiny::reactive({
        getExt <- function(x) strsplit(basename(x), "\\.")[[1]][2]
        exts <- unname(sapply(input$rmm_in$datapath, getExt))
        sameExtTest <- rep(exts[1], length(exts))
        identicalTest <- identical(exts, sameExtTest)

        # CM: can we change this so that mutliple types are allowed, to enable different studies that might've used different outputs? this just amounts to using rmmToCSV or vice versa, right?
        shiny::validate(
          shiny::need(identicalTest == TRUE, "Please make sure all the input files are of the same type.")
        )

        if (exts[1] == "csv") {
          return(lapply(input$rmm_in$datapath, csvToRMM))
        }
        if (exts[1] == "rds") {
          shiny::validate(
            shiny::need(length(exts) == 1, "Cannot input more than one .rds file.")
          )
          rds <- readRDS(input$rmm_in$datapath)
          # check if we have a list of rmms or just a single one
          if ("RMM" %in% class(rds)) {
            return(list(rds))
          } else {
            return(rds)
          }
        }
      })

      output$rmmCheckOut <- shiny::renderPrint({
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
    }
  )
}
