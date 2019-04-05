#include <iostream>
#include <fstream>
#include <vector>
#include <cstddef>
#include <cassert>
#include <map>
#include <string>

#define PUGIXML_HEADER_ONLY
#define PUGIXML_NO_EXCEPTIONS

#include "pugixml/pugixml.hpp"
#include <Rcpp.h>

void processNode(pugi::xml_node& node, const std::vector<const char*>& parentPrefix, std::map<std::string, std::vector<std::string> >& tableHeaders, std::map<std::string, int >& prefixLens, std::map<std::string, int>& levelCtrs, Rcpp::List& ret) {
	const char* root = node.name();

	// Getting header keys
	const std::vector<std::string> NodeKeys = tableHeaders[root];
	Rcpp::CharacterMatrix tempRes = ret[root];

	// Prefix
	std::vector<const char*> prefix;
	prefix.resize(prefixLens[root]);

	// Apply parent attributes to row and to children prefix
	for(unsigned long i = 0; i < parentPrefix.size(); i++) {
		if(parentPrefix[i] != NULL) {
			tempRes(levelCtrs[root], i) = parentPrefix[i];
			prefix[i] = parentPrefix[i];
		}
	}

	// Getting attributes
	for(pugi::xml_attribute a = node.first_attribute()
		; a
		; a = a.next_attribute()
	) {
		// Determine position
		std::string NodeKey(a.name());
		std::vector<std::string> NodeKeys = tableHeaders[root];
		unsigned col = find(NodeKeys.begin(), NodeKeys.end(), NodeKey) - NodeKeys.begin();
		if( col < NodeKeys.size() ) {
			tempRes(levelCtrs[root], col) = a.value();
			prefix[col] = a.value();
		}
	}
	
	// Getting elements
	for(pugi::xml_node n = node.first_child()
		; n
		; n = n.next_sibling()
	) {
		// For echousounder's sa records
		std::string NodeKey;
		if(n.name()[0] == '\0')
			NodeKey.append(root);
		else
			NodeKey.append(n.name());

		// Determine position
		std::vector<std::string> NodeKeys = tableHeaders[root];
		unsigned col = find(NodeKeys.begin(), NodeKeys.end(), NodeKey) - NodeKeys.begin();

		if( col < NodeKeys.size() ) {
#ifdef DEBUG
			cout << idx << endl;
			cout << levelCtrs[root] << endl;
			cout << tempRes.ncol() << endl;
			cout << tempRes.nrow() << endl;
#endif
			tempRes(levelCtrs[root], col) = n.text().as_string();
		} else {
			processNode(n, prefix, tableHeaders, prefixLens, levelCtrs, ret);
		}
	}

	// Increment counter
	levelCtrs[root] = levelCtrs[root] + 1;
}

// [[Rcpp::export]]
Rcpp::List readNMDxmlCpp(Rcpp::CharacterVector inputFile, Rcpp::List xsdObjects)
{

	pugi::xml_document doc;
	pugi::xml_node root_node;

	// Read xml using ifstream and buffer vector
	//std::ifstream iFile (inputFile[0]);
	//std::vector<char> buffer((std::istreambuf_iterator<char>(iFile)), std::istreambuf_iterator<char>());
	//buffer.push_back('\0');

	//pugi::xml_parse_result result = doc.load_buffer_inplace_own(&buffer[0], buffer.size());

	// Read file
	if (!doc.load_file(inputFile[0])) {
		Rcpp::Rcout << "Unable to read " << inputFile << std::endl;
		return -1;
	}

	// Get namespace
	char* xmlns = NULL;
	char* ns = NULL;

	std::string xmlStr("xmlns");
	for(pugi::xml_attribute a = doc.first_child().first_attribute()
		; a
		; a = a.next_attribute()
	) {
		std::string NodeKey(a.name());
		std::size_t found = NodeKey.find(xmlStr);
		if (found!=std::string::npos) {
			xmlns = strdup(a.value());
			// Try to get the namespace
			char *dup = strdup(NodeKey.c_str());
			strtok(dup, ":");
			char *tmp = strtok(NULL, ":");
			if(tmp != NULL)
				ns = strdup(tmp);
			free(dup);
			break;
		}
	}

	if (xmlns == NULL) {
		Rcpp::stop("Can not find the XML namespace, exiting...\n");
	}


	Rcpp::Rcout << "Root: " <<  doc.first_child().name() << "\n";
	Rcpp::Rcout << "XML namespace: " << xmlns << "\n";
	if(ns != NULL && strlen(ns) > 0) {
		Rcpp::Rcout << "XML namespace prefix: " << ns << "\n";
		Rcpp::stop("Unfortunately, namespace support is still broken!!!\n");
	} else {
		ns = NULL;
	}

	// Process namespace to get the correct XSD data
	char *token = std::strtok(xmlns, "/");

	char *one = NULL;
	char *two = token;

	while (token) {
		one = two;
		two = token;
		token = strtok(NULL, "/");
	}

	char xsd[50];
	sprintf (xsd, "%s%s.xsd", one, two);

	// Print out XML information
	Rcpp::Rcout << "Using XSD: " << xsd << std::endl;

	// Get XSD object
	Rcpp::CharacterVector root = Rcpp::as<Rcpp::List>(xsdObjects[xsd])["root"];
	Rcpp::List treeStruct = Rcpp::as<Rcpp::List>(xsdObjects[xsd])["treeStruct"];
	Rcpp::List tableHeaders = Rcpp::as<Rcpp::List>(xsdObjects[xsd])["tableHeaders"];
	Rcpp::NumericVector prefixLens = Rcpp::as<Rcpp::List>(xsdObjects[xsd])["prefixLens"];
	Rcpp::CharacterVector levelDims = Rcpp::as<Rcpp::List>(xsdObjects[xsd])["levelDims"];


	// convert R headers to std c++
	std::vector<std::string>  tableNamesCpp;
	std::map<std::string, std::vector<std::string> > tableHeadersCpp;
	std::map<std::string, int > prefixLensCpp;
	Rcpp::CharacterVector tables(tableHeaders.names());

	std::string appendNS(":");
	if(ns != NULL)
		appendNS.insert(0, ns);

	for(Rcpp::CharacterVector::iterator it = tables.begin(); it != tables.end(); ++it) {
		std::string its(*it);
		std::string itsrc(*it);

		// Use Namespace for table names
		if(ns != NULL)
			its.insert(0, appendNS);

		tableNamesCpp.push_back(its);
		tableHeadersCpp[its] = Rcpp::as<std::vector<std::string> >(tableHeaders[itsrc]);

		// Appending namespace (if any) into table header names
		if(ns != NULL && tableHeadersCpp[its].size() != 0) {
			for( unsigned subit = 0; subit < tableHeadersCpp[its].size(); ++subit) {
				tableHeadersCpp[its][subit].insert(0, appendNS);
			}
		}

		prefixLensCpp[its] = prefixLens[itsrc];
	}

	// Find our root node
	char * rootStr = root[0];
	root_node = doc.child(rootStr);

	// Create prefix storage
	std::vector<const char*> prefix;

	// Allowing one level down
	std::string toErase = "";
	if(!root_node) {
		toErase = toErase + "/" + rootStr;
		Rcpp::CharacterVector downLevel = treeStruct[rootStr];
		rootStr = downLevel[0];
		root_node = doc.child(rootStr);
	}

	// Prepare counters
	std::map<std::string, int> levelCtrs;
	
	// Prepare the result list
	Rcpp::List ret = Rcpp::List::create();
	
	// Pre-allocations
	for(unsigned i = 0; i < tableNamesCpp.size(); i++) {
		std::string tStr(tableNamesCpp[i]);

		// Counter		
		levelCtrs[tStr.c_str()] = 0;

		// Run XPATH
		std::string te =  Rcpp::as< std::string >(levelDims[tStr.c_str()]);

		// Cut first root if needed
		if(toErase.length() > 1) {
			size_t pos = te.find(toErase);
			te.erase(pos, toErase.length());
		}

		int count = 0;

		if(te != "count()") {
			pugi::xpath_query query_countnode(te.c_str());
			count = query_countnode.evaluate_number(doc);
		}

		// Matrix
		Rcpp::CharacterVector tH = tableHeaders[tStr.c_str()];

#ifdef DEBUG
		Rcpp::Rcout << te << ", "<<  count << ", " << tH.size() << std::endl;
#endif

		Rcpp::CharacterMatrix xy((int)count, tH.size());
		std::fill(xy.begin(), xy.end(), Rcpp::CharacterVector::get_na()) ;
		ret.push_back( xy, tStr );

#ifdef DEBUG
		int sz = tH.size();
		int cols = levelDims[tStr.c_str()];

		Rcpp::Rcout << "Created matrix: " << tStr.c_str() << "(" << cols << "," << sz << ", "<<  xy.size() << ")" << std::endl;
#endif
 	}

	// Naming the result list
	ret.names() = tables;

	// Process Nodes
	processNode(root_node, prefix, tableHeadersCpp, prefixLensCpp, levelCtrs, ret);

#ifdef DEBUG
	Rcpp::Rcout << "Final tally" << std::endl;
	for(int i = 0; i < tables.size(); i++) {
		std::string tStr(tables[i]);
		Rcpp::Rcout << levelCtrs[tStr] << std::endl;
	}
#endif

	// Return results and xsd name
	return Rcpp::List::create(
		Rcpp::_["xsd"]  = xsd,
		Rcpp::_["result"]  = ret
	);
}

