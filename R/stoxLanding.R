#' StoxLandingData
#'
#' Table (\code{\link[data.table]{data.table}}) with aggregated landings data from sales notes.
#'
#'
#' @section Column definitions:
#'  \describe{
#'   \item{speciesFAOCommercial}{character() FAO code for species (ASFIS)}
#'   \item{speciesCategoryCommercial}{character() code for species category (several codes may code the same species or stock, and some species may be recorded only at higher taxonomic classifications)}
#'   \item{commonNameCommercial}{character() common name used for species category in trade documents}
#'   \item{year}{integer() Year of catch}
#'   \item{catchDate}{POSIXct() Date of catch (last catch on trip) in UTC}
#'   \item{gear}{character() Code for gear used for catch (dominant gear for trip)}
#'   \item{gearDescription}{character() Descriptive text for column 'gear'}
#'   \item{area}{character() Area code for the position of catch (dominant area for trip)}
#'   \item{location}{character() Location code (subdivision of 'Area') for the position of catch (dominant area for trip)}
#'   \item{icesAreaGroup}{character() Area code for the position of catch (dominant area for trip), based on different levels of the ICES spatial coding system}
#'   \item{coastal}{character() code indidcating whether catch was taken within coastal delimitation line (dominant side for trip)}
#'   \item{coastalDescription}{character() Descriptive text for column 'coastal'}
#'   \item{n62Code}{character() Code indidcating whether catch was taken north or south of 62 deg. Lat. (dominant side for trip)}
#'   \item{n62Description}{character() Descriptive text indidcating whether catch was taken north or south of 62 deg. Lat. (dominant side for trip)}
#'   \item{vesselLength}{numeric() Maximal length of vessel in meters}
#'   \item{countryLanding}{character() Country where catch was landed}
#'   \item{weight}{nuermic() Weight of round catch in kg. Round weight may be estimated from post-processing weights.}
#'  }
#'  
#'  DEVELOPMENT NOTE:
#'  Will add, pending fix in XML parser:
#'  \describe{
#'   \item{countryVessel}{Country of the vessel that caugth the catch}
#'   \item{landingSite}{Code identifying landing site (buyer of catch)}
#'   \item{usage}{Market usage of catch.}
#'   \item{usageDescription}{Descriptive text for column 'usage'}
#'  }
#'  
#' @section Correspondance to other formats:
#'  Correspondances indicate which field a value is derived from, not necessarily verbatim copied.
#' 
#'  Correspondance to NMDlandings (http://www.imr.no/formats/landinger/v2):
#'  \describe{
#'   \item{speciesFAOCommercial}{ArtFAO_kode}
#'   \item{speciesCategoryCommercial}{Art_kode}
#'   \item{commonNameCommercial}{Art_bokmål}
#'   \item{year}{Fangstår}
#'   \item{catchDate}{SisteFangstdato}
#'   \item{gear}{Redskap_kode}
#'   \item{gearDescription}{Redskap_bokmål}
#'   \item{area}{Hovedområde_kode}
#'   \item{location}{Lokasjon_kode}
#'   \item{icesAreaGroup}{Områdegruppering_bokmål}
#'   \item{coastal}{KystHav_kode}
#'   \item{coastalDescription}{KystHav_kode}
#'   \item{n62Code}{NordSørFor62GraderNord}
#'   \item{n62Description}{NordSørFor62GraderNord}
#'   \item{vesselLength}{StørsteLengde}
#'   \item{countryVessel}{Fartøynasjonalitet_kode}
#'   \item{landingSite}{Mottaksstasjon}
#'   \item{countryLanding}{Landingsnasjon_kode}
#'   \item{usage}{HovedgruppeAnvendelse_kode}
#'   \item{usageDescription}{HovedgruppeAnvendelse_bokmål}
#'   \item{weight}{Rundvekt}
#'  }
#'
#' @name StoxLandingData
#'
NULL

loadResource <- function(name){
  
  if (name == "gear"){
    filename = "gear.csv"
    col_types = "ccc"
  }
  else if (name == "coastal"){
    filename = "coastal.csv"
    col_types = "cc"
  }
  else if (name == "n62"){
    filename = "n62.csv"
    col_types = "cc"
  }
  else if (name == "usage"){
    filename = "usage.csv"
    col_types = "ccc"
  }
  else{
    stop(paste("Resource", name, "not recognized"))
  }

  
  
  loc <- readr::locale()
  loc$encoding <- "UTF-8"
  return(readr::read_delim(system.file("extdata","codeDescriptions", filename, package="RstoxData"), delim = "\t", locale = loc, col_types = col_types))
  
}

#' Extracts relevant columns for StoxLandingData and aggregates
#' NA is treated as a category.
#' @noRd
extractAggregateLandings <- function(nmdLandings){
  
  flatLandings <- merge(nmdLandings$Seddellinje, nmdLandings$Fangstdata)
  flatLandings <- merge(flatLandings, nmdLandings$Art)
  flatLandings <- merge(flatLandings, nmdLandings$Produkt)
  flatLandings <- merge(flatLandings, nmdLandings$Mottaker)
  #flatLandings <- merge(flatLandings, nmdLandings$LandingOgProduksjonType)
  
  #
  # Note: if non-character columns are added to aggColumns. Handle accoridngly in NA-aggregation below
  #
  aggColumns <- c("ArtFAO_kode", 
                  "Art_kode", 
                  "Art_bokm\u00E5l", 
                  "Fangst\u00E5r", 
                  "SisteFangstdato", 
                  "Redskap_kode", 
                  "Hovedomr\u00E5de_kode", 
                  "Lokasjon_kode",
                  "Omr\u00E5degruppering_bokm\u00E5l", 
                  "KystHav_kode", 
                  "NordS\u00F8rFor62GraderNord", 
                  "St\u00F8rsteLengde", 
                  "Mottaksstasjon",
                  "Mottakernasjonalitet_kode")
  
  flatLandings <- flatLandings[,c(aggColumns, "Rundvekt"), with=F]

  aggList <- list()
  for (aggC in aggColumns){
    flatLandings[[aggC]][is.na(flatLandings[[aggC]])] <- "<NA>" #set NAs to text-string for aggregation
    aggList[[aggC]] <- flatLandings[[aggC]]
  }
  names(aggList) <- aggColumns
  
  aggLandings <- stats::aggregate(list(Rundvekt=flatLandings$Rundvekt), by=aggList, FUN=function(x){sum(x, na.rm=T)})
  aggLandings <- aggLandings[,c(aggColumns, "Rundvekt")]
  
  
  #reset NAs
  for (aggC in aggColumns){
    aggLandings[[aggC]][aggLandings[[aggC]] == "<NA>"] <- NA
  }
  
  # rename headers
  names(aggLandings) <- c("speciesFAOCommercial",
                           "speciesCategoryCommercial",
                           "commonNameCommercial",
                           "year",
                           "catchDate",
                           "gear",
                           "area",
                           "location",
                           "icesAreaGroup",
                           "coastal",
                           "n62Code",
                           "vesselLength",
                           "landingSite",
                           "countryLanding",
                           "weight"
                           )
  
  
  gear <- loadResource("gear")[,c("gear", "gearDescription")]
  aggLandings <- merge(aggLandings, gear, all.x=T, by="gear")
  #usage <- loadResource("usage")[,c("usage", "usageDescription")]
  #aggLandings <- merge(aggLandings, usage, all.x=T)
  coastal <- loadResource("coastal")[,c("coastal", "coastalDescription")]
  aggLandings <- merge(aggLandings, coastal, all.x=T, by="coastal")
  n62 <- loadResource("n62")[,c("n62Code", "n62Description")]
  aggLandings <- merge(aggLandings, n62, all.x=T, by="n62Code")
  
  # format conversions
  cd <- as.POSIXct(aggLandings$catchDate, format="%d.%m.%Y")
  attributes(cd)$tzone <- "UTC"
  aggLandings$catchDate <- as.POSIXct(substr(as.character(cd),1,10), format="%Y-%m-%d", tzone="UTC")
  
  aggLandings$vesselLength[aggLandings$vesselLength == "<NA>"] <- NA
  aggLandings$vesselLength <- as.numeric(aggLandings$vesselLength)
  
  aggLandings$year[aggLandings$year == "<NA>"] <- NA
  aggLandings$year <- as.integer(aggLandings$year)
  
  returnOrder <- c("speciesFAOCommercial",
                   "speciesCategoryCommercial",
                   "commonNameCommercial",
                   "year",
                   "catchDate",
                   "gear",
                   "gearDescription",
                   "area",
                   "location",
                   "icesAreaGroup",
                   "coastal",
                   "coastalDescription",
                   "n62Code",
                   "n62Description",
                   "vesselLength",
                   "landingSite",
                   "countryLanding",
                   "weight")
  
  return(data.table::as.data.table(aggLandings[,returnOrder]))
}