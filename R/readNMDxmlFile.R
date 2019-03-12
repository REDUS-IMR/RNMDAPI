processXSD <- function(doc) {

	getNameType <- function(x) {
		y <- xmlAttrs(x)
		return(c(y[["name"]], y[["type"]]))
	}

	getNameTypeExt <- function(x, base) {
		y <- xmlAttrs(x)
		return(c(base, y[["base"]]))
	}

	getRecNameType <- function(x, rootName = "") {
		# Extract name
		sName <- x[2]

		# Get rootname
		if(rootName == "")
			rootName <- x[1]

		# Clean sName
		sName <- gsub(paste0(rootName, ":"), "", sName)
		
		# Extract elements
		y <- getNodeSet(doc, paste0("//xs:complexType[@name=\"", sName, "\"]//xs:element"))

		# This is needed for echosounder v1 SA records
		extension <- getNodeSet(doc, paste0("//xs:complexType[@name=\"", sName, "\"]//xs:extension"))


		# If no children and "KeyType" (This might be specific to Biotic)
		if(length(y) > 0 && !grepl("KeyType", sName)) {
			z <- lapply(lapply(y, getNameType), getRecNameType, rootName)

			# Prepare flat
			flat[[x[1]]] <<- z
			flatAttr[[x[1]]] <<- sapply(getNodeSet(doc, paste0("//xs:complexType[@name=\"", sName, "\"]//xs:attribute/@name")), function(xx) xx[["name"]])

			# Remove nested elements
			flat[[x[1]]] <<- lapply(flat[[x[1]]], function(xx){ if(is.list(xx)) return(xx[[1]][1]) else return(xx) })
			return(list(x, members=z))
		# Below is specific for Echosounder v1's SA records (with XSD extension)
		} else if (length(extension) > 0)  {
			z <- lapply(extension, getNameTypeExt, x[1])

			# Prepare flat
			flat[[x[1]]] <<- z
			flatAttr[[x[1]]] <<- sapply(getNodeSet(doc, paste0("//xs:complexType[@name=\"", sName, "\"]//xs:attribute/@name")), function(xx) xx[["name"]])

			# Remove nested elements
			flat[[x[1]]] <<- lapply(flat[[x[1]]], function(xx){ if(is.list(xx)) return(xx[[1]][1]) else return(xx) })
			return(list(x, members=z))
		} else {
			return(x)
		}
	}


	rootInfo <- getNameType(getNodeSet(doc, "/xs:schema/xs:element")[[1]])

	flat <- list()
	flatAttr <- list()

	# start the recursive search
	invisible(getRecNameType(rootInfo))

	return(list(flat = flat, flatAttr = flatAttr, rootInfo = rootInfo))

}


processXML <- function(flat, flatAttr, rootInfo, xmlFilePath, xmlFileObj, xmlFileObjNS) {

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

	# Process headers and dimensions
	getAttrTable <- function(root, headAttr = c(), xpath="") {

		rootStart <- root[1]

		state <- flat[[rootStart]]
		attrs <- flatAttr[[rootStart]]

		# Combine with attributs
		tableElements <- c(headAttr, attrs)
		tableTypes <- rep("attr", length(tableElements))

		# List prefix for next children	
		prefix <- c(headAttr, attrs)

		# Get length of header
		headLen <- length(headAttr)

		# Get number of elements
		tempXpath <- paste(xpath, rootStart, sep='/d1:')
		elemNum <- getNodeSet(xmlFileObj, paste0("count(", tempXpath, ")"), nst)

		# If there is at least one element then it can be the parent
		if(elemNum > 0)
			xpath <- tempXpath

		for(s in 1:length(state)) {
			if(length(state[[s]]) > 1) {
				tableElements <- c(tableElements, state[[s]][1])
				tableTypes <- c(tableTypes, state[[s]][2])
			} else {
				getAttrTable(state[[s]], prefix, xpath)
			}
		}

		tableHeaders[[rootStart]] <<- unlist(tableElements)
		tablePrefix[[rootStart]] <<- unlist(prefix)
		tableTypes[[rootStart]] <<- unlist(tableTypes)
		levelDims[[rootStart]] <<- elemNum
	}

	# Meta data before go to C++
	tableHeaders <- list()
	tablePrefix <- list()
	levelDims <- list()

	# For element types
	tableTypes <- list()

	# Defining root
	root <- rootInfo[1]

	nst <- c(d1 = xmlFileObjNS)

	invisible(getAttrTable(rootInfo))

	# Fill in missing values
	missingSets <- setdiff(names(flatAttr), names(tableHeaders))
	invisible(lapply(missingSets, function (x) {
			tablePrefix[[x]] <<- character(0)
			tableHeaders[[x]] <<- character(0)
	}))
	tablePrefix[[root]] <- character(0)

	# Function to get information about children nodes
	getChildren <- function(root, flat) {

		x <- flat[[root]]

		children <- c()
		for(it in 1:length(x))
			if(length(x[[it]]) == 1)
				children <- c(children, x[[it]])
		return (children)
	} 

	# Get tree information
	treeStruct <- lapply(names(flat), getChildren, flat)
	names(treeStruct) <- names(flat)

	# Get prefix length information
	prefixLens <- lapply(tablePrefix, function(x) length(x))
	names(prefixLens) <- names(tablePrefix)

	# Unlist couple of metadata before going into c++
	levelDims <- unlist(levelDims)
	prefixLens <- unlist(prefixLens)

	# Invoke c++ xml reading
	result <- readNMDxmlCpp(xmlFilePath, root, treeStruct, tableHeaders, prefixLens, levelDims)

	# Finishing touch
	final <- lapply(names(result), applyNameType, result, tableHeaders, tableTypes)
	names(final) <- names(result)

	return(final)
}

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
#' @importFrom XML xmlAttrs xmlParse xmlSApply getNodeSet xmlNamespaceDefinitions
#' @importFrom utils tail
#' @importFrom data.table as.data.table
#'
#' @export
readNMDxmlFile <- function(xmlFilePath) {

	# Expand path
	xmlFilePath <- path.expand(xmlFilePath)

	# Check file exists
	if(!file.exists(xmlFilePath)) {
		print(paste("File", xmlFilePath, "does not exist."))
		return(NULL)
	}

	# Read XML file
	xmlFileObj <- xmlParse(xmlFilePath)

	# Read NameSpace
	xmlFileObjNS <- as.character(xmlNamespaceDefinitions(xmlFileObj, simplify = TRUE))

	# Determine correct XSD file
	nsInfo <- tail(unlist(strsplit(xmlFileObjNS, "/")), 2)
	xsdFile <- paste0(nsInfo[1], nsInfo[2], ".xsd")

	# Check if XSD exists
	print(paste("Using :", xsdFile, "."))

	# Get path from local environment
	fpath <- get("fpath", envir = localEnv)
	xsdFilePath <- paste0(fpath, "/", xsdFile)
	if(!file.exists(xsdFilePath)) {
		print(paste("It seems that", xsdFile, "does not exist or the format is not supported."))
		return(NULL)
	}

	# Parse XSD
	xsdObj <- xmlParse(xsdFilePath)

	# Get metadata based on XSD
	metaData <- processXSD(xsdObj)

	# Process XML file
	ret <- processXML(metaData$flat, metaData$flatAttr, metaData$rootInfo, xmlFilePath, xmlFileObj, xmlFileObjNS)

	return(ret)
}

