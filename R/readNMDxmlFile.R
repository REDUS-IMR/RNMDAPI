#' Read NMD XML format file downloaded from IMR NMD API
#'
#' Read NMD XML format file. Supports only Biotic version 3 and Echosounder verion 1 format at the moment.
#'
#' @param xmlFilePath full path to the XML file to be read.
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
#' @importFrom data.table as.data.table
#'
#' @export
readNMDxmlFile <- function(xmlFilePath) {

	# Process column names and types
	applyNameType <- function(x, result, tableHeaders, tableTypes) {

		# Get result matrix
		y <- result[[x]]

		# Set column names
		tableHeader <- tableHeaders[[x]]
		colnames(y) <- tableHeader

		# Convert to data.table
		z <- as.data.table(y)

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
	res <- readNMDxmlCpp(xmlFilePath, xsdObjects)

	result <- res[["result"]]
	xsd <- res[["xsd"]]

	tableHeaders <- xsdObjects[[xsd]][["tableHeaders"]]
	tableTypes <- xsdObjects[[xsd]][["tableTypes"]]

	# Finishing touch
	final <- lapply(names(result), applyNameType, result, tableHeaders, tableTypes)
	names(final) <- names(result)

	return(final)
}
