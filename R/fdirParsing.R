#' Parses landings (sales notes)
#' @description
#'  Parses sales notes data from the Norwegian Directorate of Fisheries (FDIR) on the LSS format
#' @details
#'  The LSS format is a pipe-separated format encoding landings (sales-notes).
#'  It is provided to IMR on a regular basis from FDIR.
#'  Column headers are in Norwegian.
#'
#'  Historically, columns in the landings provided from FDIR has been adapted for each data delivery
#'  Lately data deliveries has become standardized, but in order to support variants
#'  adherence to the standardization is not enfored by this function, and data types are inferred from data.
#'  The parameter 'guessMax' limits how many lines are inspected for data type inference (passed to \code{\link[readr]{read_delim}})
#' @param file file with LSS landings
#' @param encoding encoding for 'file'
#' @param guessMax passed to readr::read_delim
#' @return data.table with LSS landings
#' @import data.table
#' @import readr
#' @export
readLssFile <- function(file, encoding="latin1", guessMax = 100000){
  loc <- readr::default_locale()
  loc$decimal_mark <- ","
  loc$encoding <- encoding
  db <- readr::read_delim(file, delim="|", col_names=T, trim_ws=TRUE, na=c("", "na", "NA"), locale=loc, guess_max = guessMax)
  return(db)
}
