#' @title RangeModelMetadata Check in Shiny
#'
#' @description Run shiny app to visualize rmm check functions
#'
#' @details
#' See Examples.
#'
#' @examples
#' \dontrun{
#' rmm1=rmmTemplate()
#' rmm1=rmmAutofillPackageCitation(rmm1,c('raster','sp'))
# rmm1AutoFillData(rmm1,species=) # not used yet
#' rasterFiles=list.files(path=paste(system.file(package='dismo'), '/ex', sep=''),
#'                        pattern='grd', full.names=TRUE)
#' make a stack of the rasters
#' env=raster::stack(rasterFiles)
#' # for fitting environment
#' rmm1=rmmAutofillEnvironment(rmm1,env,transfer=0)
#' # for transfer environment 1 (assuming different than for fitting)
#' rmm1=rmmAutofillEnvironment(rmm1,env,transfer=1)
#' # for transfer environment 2 (assuming different than 1)
#' rmm1=rmmAutofillEnvironment(rmm1,env,transfer=2)
#' }
#' \dontrun{ rmmCheckShiny(rmm1) }
#'
#' @return None
#' @author Jamie Kass <jamie.m.kass@@gmail.com>
#' @note This function launches a shiny app in the default web browser
#' @export

rmmCheckShiny <- function() {
  # require(shiny) # CM: cran wants you to namespace the functions rather than use require
  shiny::shinyApp(options = list(launch.browser = TRUE),
    ui = shiny::fluidPage(

      # app title
      shiny::titlePanel("Range Model Metadata (RMM) Check"),

      # sidebar with controls
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::fileInput("rmm1", label = "Load RMM 1", accept = c("csv", "rds")),
          shiny::fileInput("rmm2", label = "Load RMM 2", accept = c("csv", "rds")),
          # CM: for flexibility, can we also accept .rdata or .rda? This should be no issue now that we have an rmm class, right?
          shiny::helpText("Note: Single RMMs can be loaded as .rds or .csv files.")#,
          #shiny::helpText("Note: TBD.")
        ),

        # Show a summary of the dataset and an HTML table with the
        # requested number of observations. Note the use of the h4
        # function to provide an additional header above each output
        # section.
        shiny::mainPanel(
          shiny::h4("Check Summary"),
          # tags$style(type='text/css', '#rmmCheck {background-color: rgba(255,255,0,0.40); color: green;}'),
          shiny::verbatimTextOutput("rmmCheck")

          # h4("Observations"),
          # tableOutput("view")
        )
      )
    ),

    server = function(input, output) {

      # reactive function to read the input file(s), error if they are
      # not the same type, and
      readRMM <- function(rmm_in) {
        ext <- strsplit(basename(rmm_in$datapath), "\\.")[[1]][2]

        if (ext == "csv") {
          return(csvToRMM(rmm_in$datapath))
        }
        if (ext == "rds") {
          return(readRDS(rmm_in$datapath))
        }
      }

      checkRMM <- function(rmm_read, i) {
        cat(paste("Performing all checks on RMM", i, "..."))
        cat("\n")
        rmmCheckFinalize(rmm_read)
        cat("All checks complete.")
        cat("\n")
        cat("-------------------------------")
        cat("\n")
      }

      output$rmmCheck <- shiny::renderPrint({
        if (!is.null(input$rmm1) & !is.null(input$rmm2)) {
          cat("-------------------------------")
          cat("\n")
          checkRMM(readRMM(input$rmm1), 1)
          checkRMM(readRMM(input$rmm2), 2)
        }
      })
    }
  )
}
