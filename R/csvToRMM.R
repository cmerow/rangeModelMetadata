#' @title Recreate rangeModelMetaData Object From .csv File
#'
#' @description Takes user-input .csv file and converts it back to a rangeModelMetaData object.
#'
#' @details
#' See Examples.
#'
#' @param csv A character file path to the csv file.
#'
#' @examples
#'
#' @author Jamie Kass <jamie.m.kass@@gmail.com>
# @note
# @seealso
# @references
# @aliases - a list of additional topic names that will be mapped to
# this documentation when the user looks them up from the command
# line.
# @family - a family name. All functions that have the same family tag will be linked in the documentation.
#' @export
#'

## NOT FUNCTIONAL


csvToRMM <- function(csv) {
  csv <- read.csv(csv)
  rmm <- list()
  field1 <- unique(csv[,1])
  for (i in field1) {
    rowSel1 <- csv[which(csv[,1] == i),]
    rmm[[i]] <- list()
    field2 <- unique(rowSel1[,2])
    for (j in field2) {
      rowSel2 <- rowSel1[which(rowSel1[,2] == j),]
      rmm[[i]][[j]] <- list()
      field3 <- unique(rowSel2[,3])
      for (k in field3) {
        rowSel3 <- rowSel2[which(rowSel2[,3] == k),]
        rmm[[i]][[j]][[k]] <- list()
        field4 <- unique(rowSel3[,4])
        for (m in field4) {
          rowSel4 <- rowSel3[which(rowSel3[,4] == m)]
          rmm[[i]][[j]][[k]][[m]] <- list()
          field5 <- unique(rowSel4[,5])
          for (n in field5) {
            rowSel5 <- rowSel4[which(rowSel4[,5] == n)]
            rmm[[i]][[j]][[k]][[m]][[n]] <- rowSel5[,5]
          }
        }
      }
    }
  }
  return(rmm)
}
