#' Read NMD XML format file downloaded from IMR NMD API
#'
#' Read NMD XML format file. Supports only Biotic version 1.4 and 3, Echosounder version 1, and Landing version 2 formats at the moment.
#' Streaming XML pull parser can be used to avoid loading the whole XML into memory and it supports ZIP file reading. Please note that
#' the XML file inside the zip file should be using the same name as the zip file itself (e.g. test.xml inside test.zip). 
#'
#' @param xmlFilePath full path to the XML file to be read.
#' @param stream a streaming XML pull parser is used if this is set to TRUE. An XML DOM parser is used if this is set to FALSE. Default to FALSE.
#'
#' @return List of data.table objects containing the "flattened" XML data.
#'
#' @examples
#' \dontrun{
#' # Reading test.xml using XML DOM parser
#' one <- readNMDxmlFile("./test.xml")
#' # Reading test.xml using XML pull parser
#' two <- readNMDxmlFile("./test.xml", stream = TRUE)
#' # Reading test.xml inside test.zip file
#' three <- readNMDxmlFile("./test.zip", stream = TRUE)
#' }
#'
#' @useDynLib RNMDAPI
#' @importFrom Rcpp sourceCpp
#' @importFrom data.table as.data.table transpose
#'
#' @export
readNMDxmlFile <- function(xmlFilePath, stream = FALSE) {

	# Process column names and types
	applyNameType <- function(x, result, tableHeaders, tableTypes) {

		# Get result matrix
		y <- result[[x]]

		# Handle empty data
		if(ncol(y) == 0)
			y <- matrix(data = "", nrow = 0, ncol = length(tableHeaders[[x]]))

		# Convert to data.table
		z <- as.data.table(y)

		# Set column names
		tableHeader <- tableHeaders[[x]]
		colnames(z) <- tableHeader

		# Set column types (only double and integer for now)
		tableType <- tableTypes[[x]]
		if(length(tableType) > 0) {
			for(i in 1:ncol(z)) {
				j <- tail(unlist(strsplit(tableType[i], ":")), 1)
				if(j %in% c("double", "integer", "decimal")) {
					# Map the types
					typeMap <- c("double" = "double", "integer" = "integer", "decimal" = "double")
					doConv <- eval(parse(text = paste0("as.", typeMap[[j]])))
					z[, tableHeader[i] := doConv(z[[tableHeader[i]]])]
				}
			}
		}
		return(z)
	}

	# Expand path
	xmlFilePath <- path.expand(xmlFilePath)

	# Check file exists
	if(!file.exists(xmlFilePath)) {
		print(paste("File", xmlFilePath, "does not exist."))
		return(NULL)
	}

	# Invoke C++ xml reading
	if(stream) {
		res <- readNMDxmlCppStream(xmlFilePath, xsdObjects)
	} else {
		res <- readNMDxmlCpp(xmlFilePath, xsdObjects)
	}

	result <- res[["result"]]
	xsd <- res[["xsd"]]

	tableHeaders <- xsdObjects[[xsd]][["tableHeaders"]]
	tableTypes <- xsdObjects[[xsd]][["tableTypes"]]

	# Finishing touch
	final <- lapply(names(result), applyNameType, result, tableHeaders, tableTypes)
	names(final) <- names(result)
	return(final)
}
