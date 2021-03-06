processXSD <- function(doc) {

	getNameType <- function(x) {
		y <- xml_attrs(x)
		return(c(y[["name"]], y[["type"]]))
	}

	getNameTypeExt <- function(x, base) {
		y <- xml_attrs(x)
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
		y <- xml_find_all(doc, paste0("//xs:complexType[@name=\"", sName, "\"]//xs:element"))

		# This is needed for echosounder v1 SA records
		extension <- xml_find_all(doc, paste0("//xs:complexType[@name=\"", sName, "\"]//xs:extension"))


		# If no children and "KeyType" (This might be specific to Biotic)
		if(length(y) > 0 && !grepl("KeyType", sName)) {
			z <- lapply(lapply(y, getNameType), getRecNameType, rootName)

			# Prepare flat
			flat[[x[1]]] <<- z
			flatAttr[[x[1]]] <<- sapply(xml_find_all(doc, paste0("//xs:complexType[@name=\"", sName, "\"]//xs:attribute/@name")), function(xx) xml_text(xx))

			# Remove nested elements
			flat[[x[1]]] <<- lapply(flat[[x[1]]], function(xx){ if(is.list(xx)) return(xx[[1]][1]) else return(xx) })
			return(list(x, members=z))
		# Below is specific for Echosounder v1's SA records (with XSD extension)
		} else if (length(extension) > 0)  {
			z <- lapply(extension, getNameTypeExt, x[1])

			# Prepare flat
			flat[[x[1]]] <<- z
			flatAttr[[x[1]]] <<- sapply(xml_find_all(doc, paste0("//xs:complexType[@name=\"", sName, "\"]//xs:attribute/@name")), function(xx) xml_text(xx))

			# Remove nested elements
			flat[[x[1]]] <<- lapply(flat[[x[1]]], function(xx){ if(is.list(xx)) return(xx[[1]][1]) else return(xx) })
			return(list(x, members=z))
		} else {
			return(x)
		}
	}


	rootInfo <- getNameType(xml_find_all(doc, "/xs:schema/xs:element")[[1]])

	flat <- list()
	flatAttr <- list()

	# start the recursive search
	invisible(getRecNameType(rootInfo))

	return(list(flat = flat, flatAttr = flatAttr, rootInfo = rootInfo))

}

processMetadata <- function(flat, flatAttr, rootInfo, xsdFile) {

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
		tempXpath <- paste(xpath, rootStart, sep='/')
		elemNumXpath <- paste0("count(", tempXpath, ")")
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
		levelDims[[rootStart]] <<- elemNumXpath
	}

	# Meta data before go to C++
	tableHeaders <- list()
	tablePrefix <- list()
	levelDims <- list()

	# For element types
	tableTypes <- list()

	# Defining root
	root <- rootInfo[1]

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

	# Unlist couple of metadata
	levelDims <- unlist(levelDims)
	prefixLens <- unlist(prefixLens)

	xsdObject <- list() 
	xsdObject[["root"]] <- root
	xsdObject[["treeStruct"]] <- treeStruct;
	xsdObject[["tableTypes"]] <- tableTypes;
	xsdObject[["tableHeaders"]] <- tableHeaders;
	xsdObject[["prefixLens"]] <- prefixLens;
	xsdObject[["levelDims"]] <- levelDims;

	return(xsdObject)
}

#' @importFrom xml2 xml_attrs read_xml xml_find_all xml_find_num xml_ns xml_text
#' @importFrom utils tail
createXsdObject <- function(xsdFile) {

	# Check if XSD exists
	print(paste("Using:", xsdFile))

	# If not exists
	if(!file.exists(xsdFile)) {
		# Get path from local environment
		fpath <- get("fpath", envir = localEnv)
		xsdFilePath <- paste0(fpath, "/", basename(xsdFile))
		if(!file.exists(xsdFilePath)) {
			print(paste("It seems that", xsdFile, "does not exist or the format is not supported."))
			return(NULL)
		}
	} else {
		xsdFilePath <- xsdFile
	}

	# Parse XSD
	xsdObj <- read_xml(xsdFilePath)

	# Get metadata based on XSD
	metaData <- processXSD(xsdObj)

	# Process XML file
	ret <- processMetadata(metaData$flat, metaData$flatAttr, metaData$rootInfo, xsdFile)

	return(ret)
}
