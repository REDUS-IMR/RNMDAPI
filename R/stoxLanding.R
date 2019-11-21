#' StoxLandingData
#'
#' Table (\code{\link[data.table]{data.table}}) with aggregated landings data from sales notes.
#'
#'
#' @section Column definitions:
#'  \describe{
#'   \item{speciesFAOCommercial}{FAO code for species (ASFIS)}
#'   \item{speciesCategoryCommercial}{code for species category (several codes may code the same species or stock, and some species may be recorded only at higher taxonomic classifications)}
#'   \item{commonNameCommercial}{common name used for species category in trade documents}
#'   \item{weight}{Weight of round catch in kg. Round weight may be estimated from post-processing weights.}
#'   \item{year}{Year of catch}
#'   \item{catchDate}{Date of catch (last catch on trip)}
#'   \item{gear}{Code for gear used for catch (dominant gear for trip)}
#'   \item{gearDescription}{Descriptive text for column 'gear'}
#'   \item{area}{Area code for the position of catch (dominant area for trip)}
#'   \item{location}{Location code (subdivision of 'Area') for the position of catch (dominant area for trip)}
#'   \item{icesAreaGroup}{Area code for the position of catch (dominant area for trip), based on different levels of the ICES spatial coding system}
#'   \item{coastal}{code indidcating whether catch was taken within coastal delimitation line (dominant side for trip)}
#'   \item{coastalDescription}{Descriptive text for column 'coastal'}
#'   \item{n62Description}{Descriptive text indidcating whether catch was taken north or south of 62 deg. Lat. (dominant side for trip)}
#'   \item{vesselLength}{Maximal length of vessel in meters}
#'   \item{countryVessel}{Country of the vessel that caugth the catch}
#'   \item{landingSite}{Code identifying landing site (buyer of catch)}
#'   \item{countryLanding}{Country where catch was landed}
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
#'   \item{weight}{Rundvekt}
#'   \item{year}{Fangstår}
#'   \item{catchDate}{SisteFangstdato}
#'   \item{gear}{Redskap_kode}
#'   \item{gearDescription}{Redskap_bokmål}
#'   \item{area}{Hovedområde_kode}
#'   \item{location}{Lokasjon_kode}
#'   \item{icesAreaGroup}{Områdegruppering_bokmål}
#'   \item{coastal}{KystHav_kode}
#'   \item{coastalDescription}{KystHav_kode}
#'   \item{n62}{NordSørFor62GraderNord}
#'   \item{vesselLength}{StørsteLengde}
#'   \item{countryVessel}{Fartøynasjonalitet_kode}
#'   \item{landingSite}{Mottaksstasjon}
#'   \item{countryLanding}{Landingsnasjon_kode}
#'   \item{usage}{HovedgruppeAnvendelse_kode}
#'   \item{usageDescription}{HovedgruppeAnvendelse_bokmål}
#'  }
#'
#' @name StoxLandingData
#'
NULL

#' Extracts relevant columns for StoxLandingData and aggregates
#' NA is treated as a category.
#' @noRd
extractAggregateLandings <- function(nmdLandings){
  flatLandings <- merge(nmdLandings$Seddellinje, nmdLandings$Fangstdata)
  flatLandings <- merge(flatLandings, nmdLandings$Art)
  flatLandings <- merge(flatLandings, nmdLandings$Produkt)
  flatLandings <- merge(flatLandings, nmdLandings$Mottaker)
  flatLandings <- merge(flatLandings, nmdLandings$LandingOgProduksjonType)
  
  flatLandings <- flatLandings[,c("ArtFAO_kode", "Art_kode", "Art_bokm\u00E5l", "Rundvekt", "Fangst\u00E5r", "SisteFangstdato", "Redskap_kode", "Hovedomr\u00E5de_kode", "Omr\u00E5ddegruppering_bokm\u00E5dl", "KystHav_kode", "NordS\u00F8rFor62GraderNord", "St\u00F8rsteLengde", "Fart\u00F8ynasjonalitet_kode", "Mottaksstasjon", "Landingsnasjon_kode", "HovedgruppeAnvendelse_kode")]
  
  #aggregate, treat NA in aggregation columns as values.
  
  # rename headers
  # merge in descriptive fields
  
  # return as data.table
}