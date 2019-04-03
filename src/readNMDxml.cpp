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

void processNode(pugi::xml_node& node, const std::vector<const char*>& parentPrefix, const Rcpp::List& treeStruct, const Rcpp::List& tableHeaders, const Rcpp::NumericVector& prefixLens, std::map<std::string, int>& levelCtrs, Rcpp::List& ret) {
	const char* root = node.name();

	// Getting header keys
	Rcpp::CharacterVector NodeKeys = tableHeaders[root];
	Rcpp::CharacterMatrix tempRes = ret[root];

	// Prefix
	std::vector<const char*> prefix;
	prefix.resize(prefixLens[root]);

	// Apply parent attributes to row and to children prefix
	for(unsigned long i = 0; i < parentPrefix.size(); i++) {
		tempRes(levelCtrs[root], i) = parentPrefix[i];
		prefix[i] = parentPrefix[i];
	}

	// Getting attributes
	for(pugi::xml_attribute a = node.first_attribute()
		; a
		; a = a.next_attribute()
	) {
		Rcpp::CharacterVector NodeKey(a.name());
		Rcpp::IntegerVector col = match(NodeKey, NodeKeys);
		//cout << col[0] <<endl;
		if( col[0] > 0 ) {
			int idx = col[0] - 1;
			tempRes(levelCtrs[root], idx) = a.value();
			prefix[idx] = a.value();
		}
	}
	
	// Getting elements
	for(pugi::xml_node n = node.first_child()
		; n
		; n = n.next_sibling()
	) {
		//cout << n.name() << n.value() << endl;
		// For echousounder's sa records
		Rcpp::CharacterVector NodeKey("");
		if(n.name()[0] == '\0')
			NodeKey[0] = root;
		else
			NodeKey[0] = n.name();
		Rcpp::IntegerVector col = match(NodeKey, NodeKeys);
		if( col[0] > 0 ) {
#ifdef DEBUG
			cout << idx << endl;
			cout << levelCtrs[root] << endl;
			cout << tempRes.ncol() << endl;
			cout << tempRes.nrow() << endl;
#endif
			tempRes(levelCtrs[root], col[0] - 1) = n.text().as_string();
		} else {
			processNode(n, prefix, treeStruct, tableHeaders, prefixLens, levelCtrs, ret);
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
	char* xmlns = (char*) doc.first_child().attribute("xmlns").value();

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
	Rcpp::Rcout << "Parsing XML: " << inputFile << std::endl;
	Rcpp::Rcout << "Detected XSD: " << xsd << std::endl;

	// Get XSD object
	Rcpp::CharacterVector root = Rcpp::as<Rcpp::List>(xsdObjects[xsd])["root"];
	Rcpp::List treeStruct = Rcpp::as<Rcpp::List>(xsdObjects[xsd])["treeStruct"];
	Rcpp::List tableHeaders = Rcpp::as<Rcpp::List>(xsdObjects[xsd])["tableHeaders"];
	Rcpp::NumericVector prefixLens = Rcpp::as<Rcpp::List>(xsdObjects[xsd])["prefixLens"];
	Rcpp::CharacterVector levelDims = Rcpp::as<Rcpp::List>(xsdObjects[xsd])["levelDims"];

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
	Rcpp::CharacterVector tables(treeStruct.names());
	
	// Prepare the result list
	Rcpp::List ret = Rcpp::List::create();
	
	// Pre-allocations
	for(int i = 0; i < tables.size(); i++) {
		std::string tStr(tables[i]);

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
	processNode(root_node, prefix, treeStruct, tableHeaders, prefixLens, levelCtrs, ret);

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

