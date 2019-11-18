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
#' @param file path to file with LSS landings
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
  return(data.table::as.data.table(db))
}


#' Read pipe separated file with specified columns
#' @noRd
read_psv <- function(file, encoding, col_types){
  loc <- readr::default_locale()
  loc$decimal_mark <- ","
  loc$encoding <- encoding
  db <- readr::read_delim(file, delim="|", col_names=T, trim_ws=TRUE, na=c("", "na", "NA"),locale=loc, col_types = col_types)
  return(db)
}

#' Parses logbooks (ERS) 
#' @description 
#'  Parses electronic logbooks (ERS) from tabular format delivered by Directorate of Fisheries (FDIR)
#' @details 
#'  The format is a pipe-separated format encoding aggregated ERS records (logbooks).
#'  It is provided to IMR on a regular basis from FDIR.
#'  Column headers are in Norwegian.
#' @param file path to file
#' @param encoding encoding for 'file'
#' @return data.table() with logbooks
#' @export
readErsFile <- function(file, encoding="latin1"){
  
  spec_log <- cols(
    RC = col_character(),
    REGM = col_character(),
    STORSTE_LENGDE = col_double(),
    BRUTTOTONNASJE = col_integer(),
    MOTORKRAFT = col_integer(),
    TM1 = col_character(),
    AKTIVITET_KODE = col_character(),
    AKTIVITET = col_character(),
    PUMPET_FRA = col_character(),
    FANGSTAR = col_integer(),
    STARTTIDSPUNKT = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
    START_LT = col_double(),
    START_LG = col_double(),
    SONE = col_character(),
    KVOTETYPE_KODE = col_character(),
    KVOTETYPE = col_character(),
    REDSKAP_FAO = col_character(),
    REDSKAP_NS = col_character(),
    REDSKAP = col_character(),
    REDSKAPSSPESIFIKASJON_KODE = col_character(),
    REDSKAPSSPESIFIKASJON = col_character(),
    MASKEVIDDE = col_integer(),
    REDSKAP_PROBLEMER_KODE = col_character(),
    REDSKAP_PROBLEMER = col_character(),
    STOPPTIDSPUNKT = col_datetime(format = "%Y-%m-%d %H:%M:%S"),
    STOPP_LT = col_double(),
    STOPP_LG = col_double(),
    VARIGHET = col_integer(),
    INNSATS = col_number(),
    SILD_BESTAND_KODE = col_character(),
    SILD_BESTAND_NS = col_character(),
    SILD_BESTAND = col_character(),
    HOVEDART_FAO = col_character(),
    HOVEDART_NS = col_character(),
    HOVEDART = col_character(),
    INT_OMR_GML_START = col_character(),
    INT_OMR_NY_START = col_character(),
    INT_OMR_GML_STOPP = col_character(),
    INT_OMR_NY_STOPP = col_character(),
    HAV_DYBDE_START = col_number(),
    HAV_DYBDE_STOPP = col_number(),
    LOKASJON_START = col_character(),
    LOKASJON_STOPP = col_character(),
    TREKK_AVSTAND_METER = col_integer(),
    FANGSTART_FAO = col_character(),
    FANGSTART_NS = col_character(),
    FANGSTART = col_character(),
    RUNDVEKT = col_double()
  )
  names(spec_log$cols) <- c(names(spec_log$cols)[1:2], "ST\u00D8RSTE_LENGDE", names(spec_log$cols)[4:9], "FANGST\u00C5R", names(spec_log$cols)[11:length(spec_log$cols)])
  
  logb <- read_psv(file, encoding, col_types=spec_log)
  
  return(data.table::as.data.table(logb))
}
