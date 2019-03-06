#' Read NMD Biotic XML format file downloaded from IMR NMD API
#'
#' Read NMD Biotic XML format file. Supports only version 3 format at the moment.
#'
#' @param filePath full path to the Biotic XML file to be read
#' @param version Biotic XML format version
#'
#' @return List containing three matrices containing fish stations, catch samples, and individuals data.
#'
#' @examples
#' \dontrun{
#' readBioticXML("./test.xml")
#' }
#'
#' @useDynLib RNMDAPI
#' @importFrom Rcpp sourceCpp
#' @importFrom XML xmlParse xmlSApply getNodeSet xmlNamespaceDefinitions
#'
#' @export
readBioticXML <- function(filePath, version = "3") {

	# Expand path
	filePath <- path.expand(filePath)

	# Check file exists
	if(!file.exists(filePath))
		return(NULL)

	# Get XSD
	if ( version == "3" )
		doc <- xmlParse("http://www.imr.no/formats/nmdbiotic/v3/nmdbioticv3.xsd")
	else
		stop("Not implemented.")

	# Get reference keys
	nodeKeys <- list()
	nodeKeys[["FS"]] <- xmlSApply(getNodeSet(doc, "//xs:complexType[@name=\"FishstationType\"]/*/xs:element[not(@type=\"CatchsampleType\")]/@name"), function(x) x[["name"]])
	nodeKeys[["FS"]] <- c("serialnumber", nodeKeys[["FS"]])

	nodeKeys[["CA"]] <- xmlSApply(getNodeSet(doc, "//xs:complexType[@name=\"CatchsampleType\"]/*/xs:element[not(@type=\"IndividualType\")]/@name"), function(x) x[["name"]])
	nodeKeys[["CA"]] <- c("serialnumber", "catchsampleid", nodeKeys[["CA"]])

	nodeKeys[["ID"]] <- xmlSApply(getNodeSet(doc, "//xs:complexType[@name=\"IndividualType\"]/*/xs:element[not(@type=\"AgedeterminationType\" or @type=\"TagType\" or @type=\"PreyType\")]/@name"), function(x) x[["name"]])
	nodeKeys[["ID"]] <- c("serialnumber", "catchsampleid", "specimenid", nodeKeys[["ID"]])

	# Get element numbers for pre-allocation
	biotic <- xmlParse(filePath)
	nst <- c(d1 = as.character(xmlNamespaceDefinitions(biotic, simplify = TRUE)))
	levelDims <- c()
	levelDims["FS"] <- getNodeSet(biotic, "count(//d1:mission/d1:fishstation)", nst)
	levelDims["CA"] <- getNodeSet(biotic, "count(//d1:mission/d1:fishstation/d1:catchsample)", nst)
	levelDims["ID"] <- getNodeSet(biotic, "count(//d1:mission/d1:fishstation/d1:catchsample/d1:individual)", nst)

	print("Found these structures:")
	print(levelDims)

	# Read it
	ret <- readBioticXMLCpp(filePath, nodeKeys, levelDims)

	return(ret)

}
