#' Read NMD XML format file downloaded from IMR NMD API
#'
#' Read NMD XML format file. Supports only Biotic version 3 and Echosounder version 1 format at the moment.
#'
#' @param xmlFilePath full path to the XML file to be read.
#' @param stream a streaming XML pull parser is used if this set to TRUE. A XML DOM parser is used if this set to FALSE. Default to FALSE.
#'
#' @return List of data.table(s) containing the "flattened" XML data.
#'
#' @examples
#' \dontrun{
#' readNMDxmlFile("./test.xml")
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

		# Convert to data.table
		z <- as.data.table(y)

		# Set column names
		tableHeader <- tableHeaders[[x]]
		colnames(z) <- tableHeader

		# Set column types (only double and integer that is going to be use)
		tableType <- tableTypes[[x]]
		if(length(tableType) > 0) {
			for(i in 1:ncol(z)) {
				j <- tail(unlist(strsplit(tableType[i], ":")), 1)
				if(j %in% c("double", "integer")) {
					doConv <- eval(parse(text = paste0("as.", j)))
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

		result <- lapply(res[["result"]], function(x) transpose(x))
		names(result) <- names(res[["result"]])
		xsd <- res[["xsd"]]

	} else {
		res <- readNMDxmlCpp(xmlFilePath, xsdObjects)

		result <- res[["result"]]
		xsd <- res[["xsd"]]
	}

	tableHeaders <- xsdObjects[[xsd]][["tableHeaders"]]
	tableTypes <- xsdObjects[[xsd]][["tableTypes"]]

	# Finishing touch
	final <- lapply(names(result), applyNameType, result, tableHeaders, tableTypes)
	names(final) <- names(result)
	return(final)
}
