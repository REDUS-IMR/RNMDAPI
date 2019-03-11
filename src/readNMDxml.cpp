#include <iostream>
#include <fstream>
#include <vector>
#include <cstddef>
#include <cassert>
#include <map>
#include <string>

#define RAPIDXML_NO_STDLIB
#define RAPIDXML_NO_EXCEPTIONS
#include "rapidxml-1.13/rapidxml.hpp"
#include <Rcpp.h>

#if defined(RAPIDXML_NO_EXCEPTIONS)
void rapidxml::parse_error_handler(const char* what, void* where)
{
    Rcpp::stop("Parse error(@%p): %s\n", where, what);
}
#endif

void processNode(const rapidxml::xml_node<>* node, const std::vector<char*>& parentPrefix, const Rcpp::List& treeStruct, const Rcpp::List& tableHeaders, const Rcpp::NumericVector& prefixLens, std::map<std::string, int>& levelCtrs, Rcpp::List& ret) {
	const char* root = node->name();

	// Getting header keys
	Rcpp::CharacterVector NodeKeys = tableHeaders[root];
	Rcpp::CharacterMatrix tempRes = ret[root];

	// Prefix
	std::vector<char*> prefix;
	prefix.resize(prefixLens[root]);

	// Apply parent attributes to row and to children prefix
	for(unsigned long i = 0; i < parentPrefix.size(); i++) {
		tempRes(levelCtrs[root], i) = parentPrefix[i];
		prefix[i] = parentPrefix[i];
	}

	// Getting attributes
	for(const rapidxml::xml_attribute<>* a = node->first_attribute()
		; a
		; a = a->next_attribute()
	) {
		Rcpp::CharacterVector NodeKey(a->name());
		Rcpp::IntegerVector col = match(NodeKey, NodeKeys);
		//cout << col[0] <<endl;
		if( col[0] > 0 ) {
			int idx = col[0] - 1;
			tempRes(levelCtrs[root], idx) = a->value();
			prefix[idx] = a->value();
		}
	}
	
	// Getting elements
	for(const rapidxml::xml_node<>* n = node->first_node()
		; n
		; n = n->next_sibling()
	) {
		//cout << n->name() << n->value() << endl;
		Rcpp::CharacterVector NodeKey(n->name());
		Rcpp::IntegerVector col = match(NodeKey, NodeKeys);
		if( col[0] > 0 ) {
#ifdef DEBUG
			cout << idx << endl;
			cout << levelCtrs[root] << endl;
			cout << tempRes.ncol() << endl;
			cout << tempRes.nrow() << endl;
#endif
			tempRes(levelCtrs[root], col[0] - 1) = n->value();
		} else {
			processNode(n, prefix, treeStruct, tableHeaders, prefixLens, levelCtrs, ret);
		}
	}

	// Increment counter
	levelCtrs[root] = levelCtrs[root] + 1;
}

// [[Rcpp::export]]
Rcpp::List readNMDxmlCpp(Rcpp::CharacterVector inputFile, Rcpp::CharacterVector root, Rcpp::List treeStruct, Rcpp::List tableHeaders, Rcpp::NumericVector prefixLens, Rcpp::NumericVector levelDims)
{

	rapidxml::xml_document<> doc;
	rapidxml::xml_node<> * root_node;

	Rcpp::Rcout << "Parsing biotic XML :" << inputFile << "." << std::endl;

	// Read xml using ifstream and buffer vector
	std::ifstream iFile (inputFile[0]);
	std::vector<char> buffer((std::istreambuf_iterator<char>(iFile)), std::istreambuf_iterator<char>());
	buffer.push_back('\0');

	// Parse the buffer using the xml file parsing library into doc 
	doc.parse<0>(&buffer[0]);

	// Find our root node
	char * rootStr = root[0];
	root_node = doc.first_node(rootStr);

	// Create prefix storage
	std::vector<char*> prefix;

	// Allowing one level down
	if(!root_node) {
		Rcpp::CharacterVector downLevel = treeStruct[rootStr];
		rootStr = downLevel[0];
		root_node = doc.first_node(rootStr);
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

		// Matrix
		Rcpp::CharacterVector tH = tableHeaders[tStr.c_str()];
		Rcpp::CharacterMatrix xy(levelDims[tStr.c_str()], tH.size());
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

	return ret;
}

