
dm_biotic <- list("KeyType"="as.character", "CompositeTaxaKeyType"="as.character","CompositeTaxaSexKeyType"="as.character", "xs:integer"="as.integer", "xs:string"="as.character", "xs:decimal"="as.double", "xs:date"="as.Date", "xs:time"="as.character")
ct_biotic <- list("missions"="MissionsType", "mission"="MissionType", "fishstation"="FishstationType", "catchsample"="CatchsampleType", "individual"="IndividualType", "prey"="PreyType", "copepodedevstagefrequencytable"="CopepodedevstageType", "preylengthfrequencytable"="PreylengthType", "tag"="TagType", "agedetermination"="AgedeterminationType")

#'Set data types based on xsd.
#'@description readNMDxmlFile returns a relational representation of the data as list of data frames, one for each complexType in xsd, and with keys (attributes) of added to lower levels in the hierarchical data model.
#'@details Assumes fixed namespace prefix xs for http://www.w3.org/2001/XMLSchema. Assumes that key names are not repeated between levels / complexTypes.
#'@param parsed_data list of data.frames as returned by readNMDxmlFile
#'@param schema XMLInternalDocument representing the xsd
#'@param datatype_mapping list mapping data types in schema to names of functions used to convert to R data types
#'@param complextype_mapping list mapping data frame names in parsed_data to complexType definitions in xsd.
#'
set_data_types <- function(parsed_data, schema, datatype_mapping=dm_biotic, complextype_mapping=ct_biotic){
  get_data_types <- function(typename){
    elements <- getNodeSet(schema, paste("/xs:schema/xs:complexType[@name='",complextype_mapping[[typename]],"']//xs:element", sep=""), c(xs="http://www.w3.org/2001/XMLSchema"))
    attribs <- getNodeSet(schema, paste("/xs:schema/xs:complexType[@name='",complextype_mapping[[typename]],"']//xs:attribute", sep=""), c(xs="http://www.w3.org/2001/XMLSchema"))
    fieldnames <- lapply(elements, xmlGetAttr, "name")
    fieldnames <- append(fieldnames, lapply(attribs, xmlGetAttr, "name"))
    fieldtypes <- lapply(elements, xmlGetAttr, "type")
    fieldtypes <- append(fieldtypes, lapply(attribs, xmlGetAttr, "type"))
    names(fieldtypes) <- fieldnames
    return(fieldtypes)
  }
  
  get_keytype_mapping <- function(){
    
    #identiify foreign keys (columns not found on complexType in xsd)
    keys <- c()
    for (fr in names(parsed_data)){
      frame <- parsed_data[[fr]]
      dtypes <- get_data_types(fr)  
      for (coln in names(frame)){
        if (is.null(dtypes[[coln]])){
          keys <- c(coln, keys)
        }
      }
    }
    
    #identify type from correpsonding primary keys
    mapping <- list()
    for (fr in names(parsed_data)){
      dtypes <- get_data_types(fr)  
      for (key in unique(keys)){
        if (key %in% names(dtypes)){
          if (key %in% names(mapping)){
            stop(paste(key, "occurs as key on several data frames"))
          }
          else{
            mapping[[key]] <- dtypes[[key]]
          }
        }
      }
    }
    return(mapping)
  }
  
  foreign_key_types <- get_keytype_mapping()
  
  #convert data types
  for (fr in names(parsed_data)){
    dtypes <- get_data_types(fr)
    frame <- parsed_data[[fr]]
    for (coln in names(frame)){
      t<-dtypes[[coln]]
      
      #deal with type of foreign keys
      if (coln %in% names(foreign_key_types)){
        t <- foreign_key_types[[coln]]
      }
      
      if (is.null(t)){
        stop(paste("Type could not be determined for", coln, "on", fr)) 
      }
      
      ff <- datatype_mapping[[t]]        
      
      if (is.null(ff)){
        stop(paste("Data type", t, "not mapped"))
      }
      frame[[coln]] <- eval(call(ff, frame[[coln]]))
    }
    parsed_data[[fr]] <- frame
  }
  
  return(parsed_data)
}