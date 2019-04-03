#include "xmlio/xmlinput.h"
#include "xmlio/xmlfile.h"
#include <iostream>
#include <cstring>
#include <algorithm>
#include <cctype>
#include <locale>

#include "Rcpp.h"

// code below are from: https://stackoverflow.com/questions/216823/whats-the-best-way-to-trim-stdstring
// trim from start (in place)
static inline void ltrim(std::string &s) {
	s.erase(s.begin(), std::find_if(s.begin(), s.end(),
	                                std::not1(std::ptr_fun<int, int>(std::isspace))));
}

// trim from end (in place)
static inline void rtrim(std::string &s) {
	s.erase(std::find_if(s.rbegin(), s.rend(),
	                     std::not1(std::ptr_fun<int, int>(std::isspace))).base(), s.end());
}

// trim from both ends (in place)
static inline void trim(std::string &s) {
	ltrim(s);
	rtrim(s);
}


class persistentData {
private:
	const Rcpp::List* tableHeaders;
	const Rcpp::NumericVector* prefixLens;
	std::map<std::string, std::list<std::vector<std::string> > >* ret;
public:
	persistentData(const Rcpp::List& a, const Rcpp::NumericVector& b, std::map<std::string, std::list<std::vector<std::string> > >* c)
		:
		tableHeaders(&a),
		prefixLens(&b),
		ret(c)
	{}

	const Rcpp::List* getTableHeaders()
	{
		return tableHeaders;
	}
	const Rcpp::NumericVector* getPrefixLens()
	{
		return prefixLens;
	}
	std::map<std::string, std::list<std::vector<std::string> > >* getRet()
	{
		return ret;
	}

};

class passingData : public persistentData {
private:
	const char* parent;
	std::vector<std::string>* parentPrefix;
	const char* column;
	std::vector<std::string>* content;

public:
	passingData(const Rcpp::List& a, const Rcpp::NumericVector& b, std::map<std::string, std::list<std::vector<std::string> > >* c, const char* d, std::vector<std::string>* e, const char* f, std::vector<std::string>* g) : persistentData(a, b,c),
		parent(d),
		parentPrefix(e),
		column(f),
		content(g)
	{}

	const char* getParentName()
	{
		return parent;
	}

	std::vector<std::string>* getParentPrefix()
	{
		return parentPrefix;
	}

	const char* getColumn()
	{
		return column;
	}

	std::vector<std::string>* getContent()
	{
		return content;
	}

};


class returnData {
private:
	const char* xsdUsed;
	std::map<std::string, std::list<std::vector<std::string> > >* ret;
	const Rcpp::List *xsdObjects;

public:
	returnData(Rcpp::List& a) :
		xsdObjects(&a)
	{}
	const Rcpp::List *getXsdObjects()
	{
		return xsdObjects;
	}
	void setXsdUsed(char* input)
	{
		xsdUsed = strdup(input);
	}
	void setReturnData(std::map<std::string, std::list<std::vector<std::string> > >& input)
	{
		ret = &input;
	}
	const char* getXsdUsed()
	{
		return xsdUsed;
	}
	std::map<std::string, std::list<std::vector<std::string> > >* getReturnData()
	{
		return ret;
	}
};

static void sDataHandler(const XML_Char *data, size_t len, void *userData)
{
	if(len > 0 && data[0] != '\0') {

		// Put data inside string
		std::string strdata(data, len);

		// Remove (left/right) whitespaces
		trim(strdata);

		if(strdata.size() > 0) {
			// Parse data
			passingData* pD = (passingData*) userData;

			// get table headers
			const Rcpp::List* tableHeaders = pD->getTableHeaders();
			const char* parent = pD->getParentName();
			const char* column = pD->getColumn();

#ifdef DEBUG
			Rcpp::Rcout << parent << "-> ";
			Rcpp::Rcout << column << ": " ;
			Rcpp::Rcout << strdata << std::endl;
#endif

			// Get content
			std::vector<std::string>* contentPtr = pD->getContent();
#ifdef DEBUG
			for (std::vector<std::string>::iterator it = contentPtr->begin() ; it != contentPtr->end(); ++it)
				Rcpp::Rcout << *it << " ";
			Rcpp::Rcout << '\n';
#endif
			// Determine position
			Rcpp::CharacterVector NodeKey(column);
			Rcpp::CharacterVector NodeKeys = (*tableHeaders)[parent];
			Rcpp::IntegerVector col = match(NodeKey, NodeKeys);

			// Put the value inside content
			(*contentPtr)[col[0]-1] = strdata;

#ifdef DEBUG
			Rcpp::Rcout << "( " << col[0]-1 << " )" << NodeKeys << std::endl;
			for (std::vector<std::string>::iterator it = contentPtr->begin() ; it != contentPtr->end(); ++it)
				Rcpp::Rcout << *it << " ";
			Rcpp::Rcout << '\n';
#endif
		}
	}
}

static void sElemHandler(XML::Element &elem, void *userData)
{
	// Get shared data
	passingData* pD = (passingData*) userData;

	std::vector<std::string>* parentPrefix = pD->getParentPrefix();

	const Rcpp::List* tableHeaders = pD->getTableHeaders();
	const Rcpp::NumericVector* prefixLens = pD->getPrefixLens();
	std::map<std::string, std::list<std::vector<std::string> > >* ret = pD->getRet();

	// Get root
	const char* root = elem.GetName();

	// Check if we are almost near the value
	Rcpp::CharacterVector rK(root);
	Rcpp::CharacterVector TableNames = tableHeaders->names();
	Rcpp::IntegerVector check = match(rK, TableNames);

	// Prepare prefix
	std::vector<std::string>* prefixPtr;

	// Prepare parent
	const char* parent;

	// Prepare content pointer
	std::vector<std::string>* contentPtr;

	if( check[0] > 0 ) {

		parent = root;

		//std::string tStr(root);

		// Getting result placeholder
		std::list<std::vector<std::string> >* tempRes = &((*ret)[(char*) root]);

		// Getting header keys
		Rcpp::CharacterVector NodeKeys = (*tableHeaders)[(char*) root];

		// Create content
		std::vector<std::string> *content = new std::vector<std::string>(NodeKeys.size());

		// Prefix resize
		int prefixSize = (*prefixLens)[(char*) root];
		std::vector<std::string> *prefix = new std::vector<std::string>(prefixSize);

		// Apply parent attributes to row and to children prefix
		for(unsigned long i = 0; i < parentPrefix->size(); i++) {
			(*content)[i] = (*prefix)[i] = (*parentPrefix)[i];
		}

		// begin new element (and write attributes)
		if (elem.NumAttributes() > 0)
		{
			XML::Attribute a = elem.GetAttrList();
			while (a)
			{
				Rcpp::CharacterVector NodeKey(a.GetName());
				Rcpp::IntegerVector col = match(NodeKey, NodeKeys);
				if( col[0] > 0 ) {
					int idx = col[0] - 1;
					(*content)[idx] = (*prefix)[idx] = a.GetValue();
				}
				a = a.GetNext();
			}
		}


		// Push back the result
#ifdef __cpp_rvalue_references
		tempRes->push_back(std::move(*content));
#else
		tempRes->push_back(*content);
#endif
		contentPtr = &(tempRes->back());
		prefixPtr = prefix;

#ifdef DEBUG
		for (std::vector<std::string>::iterator it = contentPtr->begin() ; it != contentPtr->end(); ++it)
			Rcpp::Rcout << *it << " ";
		Rcpp::Rcout << '\n';
#endif


	} else {
		parent = pD->getParentName();
		prefixPtr = parentPrefix;
		contentPtr = pD->getContent();
	}

	passingData *newPD = new passingData(*tableHeaders, *prefixLens, ret, parent, prefixPtr, root, contentPtr);

	// handle the subelements (and data)
	const XML::Handler handlers[] = {
		XML::Handler(sElemHandler),
		XML::Handler(sDataHandler),
		XML::Handler::END
	};

	elem.Process(handlers, newPD);

#ifdef DEBUG
	for (std::vector<std::string>::iterator it = contentPtr->begin() ; it != contentPtr->end(); ++it)
		Rcpp::Rcout << *it << " ";
	Rcpp::Rcout << '\n';
#endif

}

static void rootHandler(XML::Element &elem, void *userData)
{
	returnData *data = (returnData *) userData;

	const Rcpp::List *xsdObjects = data->getXsdObjects();

	const char* root = elem.GetName();
	char* xmlns = strdup(elem.GetAttribute("xmlns"));

	Rcpp::Rcout << "Root: " << root << "\n";
	Rcpp::Rcout << "Document: " << xmlns << "\n";

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

	Rcpp::Rcout << "Detected XSD: " << xsd << std::endl;

	// Put xsd info into return data
	data->setXsdUsed(xsd);

	// Get XSD object
	Rcpp::List tableHeaders = Rcpp::as<Rcpp::List>((*xsdObjects)[xsd])["tableHeaders"];
	Rcpp::NumericVector prefixLens = Rcpp::as<Rcpp::List>((*xsdObjects)[xsd])["prefixLens"];

#ifdef DEBUG
	// Print out XML information
	Rcpp::Rcout << "Start from root: " << root << std::endl;
#endif

	// Prepare counters
	Rcpp::CharacterVector tables(tableHeaders.names());

	// Prepare the result map
	std::map<std::string, std::list<std::vector<std::string> > >* ret = new std::map<std::string, std::list<std::vector<std::string> > >;

	// Pre-allocations
	for(int i = 0; i < tables.size(); i++) {
		std::list<std::vector<std::string> >* df = new std::list<std::vector<std::string> >;
#ifdef __cpp_rvalue_references
		// For c++11 enabled compiler
		(*ret)[(char*)tables[i]] = std::move(*df);
#else
		(*ret)[(char*)tables[i]] = *df;
#endif

#ifdef DEBUG
		std::cout << "size aft: " << tStr << "->" <<  (*ret)[tStr].size() << std::endl;
#endif
	}

	// Get the first root content
	std::string tStr(root);
	Rcpp::CharacterVector NodeKeys = tableHeaders[tStr];
	std::vector<std::string> *content = new std::vector<std::string>(NodeKeys.size());

#ifdef __cpp_rvalue_references
	// For c++11 enabled compiler
	(*ret)[tStr].push_back(std::move(*content));
#else
	(*ret)[tStr].push_back(*content);
#endif
	content = &((*ret)[tStr].back());

	// Create prefix storage
	std::vector<std::string> *prefix = new std::vector<std::string>(prefixLens[tStr]);

	// begin new element (and write attributes)
	if (elem.NumAttributes() > 0)
	{
		Rcpp::CharacterVector NodeKeys = tableHeaders[root];
		XML::Attribute a = elem.GetAttrList();
		while (a)
		{
			Rcpp::CharacterVector NodeKey(a.GetName());
			Rcpp::IntegerVector col = match(NodeKey, NodeKeys);
			if( col[0] > 0 ) {
				int idx = col[0] - 1;
				(*content)[idx] = (*prefix)[idx] = a.GetValue();
			}
			a = a.GetNext();
		}
	}

#ifdef DEBUG
	for ( std::map<std::string, std::list<std::vector<std::string> > >::iterator it = ret->begin(); it != ret->end(); it++ )
	{
		Rcpp::Rcout << it->first  // string (key)
		            << ':'
		            << (it->second).size()   // string's value
		            << std::endl ;
	}
	Rcpp::Rcout << std::endl;

	// Check first content
	for (std::vector<std::string>::iterator it = prefix->begin() ; it != prefix->end(); ++it)
		Rcpp::Rcout << *it << " ";
	Rcpp::Rcout << '\n';

	for (std::vector<std::string>::iterator it = content->begin() ; it != content->end(); ++it)
		Rcpp::Rcout << *it << " ";
	Rcpp::Rcout << '\n';
#endif

	// Prepare passing data store
	passingData *pD = new passingData(tableHeaders, prefixLens, ret, root, prefix, root, content);

	// Handle sub-elements and data
	const XML::Handler handlers[] = {
		XML::Handler(sElemHandler),
		XML::Handler(sDataHandler),
		XML::Handler::END
	};

	elem.Process(handlers, pD);

	// Put all data into return data
	data->setReturnData(*ret);
}


// [[Rcpp::export]]
Rcpp::List readNMDxmlCppStream(Rcpp::CharacterVector inputFile, Rcpp::List xsdObjects)
{
	// Print out XML information
	Rcpp::Rcout << "Parsing XML: " << inputFile << std::endl;

	// open input stream
	XML::FileInputStream istream(inputFile[0]);
	XML::Input input(istream);

	// set up initial handler for Document
	XML::Handler handlers[] = {
		XML::Handler(rootHandler),
		XML::Handler::END
	};

	// Prepare return data class
	returnData *data = new returnData(xsdObjects);

	try {
		input.Process(handlers, data);
	}
	catch (const XML::ParseException &e)
	{
		Rcpp::Rcerr << "ERROR: " << e.What() << "(line " << e.GetLine() << ", column " << e.GetColumn() << ")\n";
	}


	Rcpp::List result = Rcpp::List::create();

	std::map<std::string, std::list<std::vector<std::string> > >* res = data->getReturnData();

	for ( std::map<std::string, std::list<std::vector<std::string> > >::iterator it = res->begin(); it != res->end(); it++ )
	{
#ifdef DEBUG
		Rcpp::Rcout << it->first
		            << ':'
		            << (it->second).size()
		            << std::endl ;
#endif
		result[it->first] = Rcpp::wrap(it->second);
	}

	// Return results and xsd name
	return Rcpp::List::create(
	           Rcpp::_["xsd"]  = data->getXsdUsed(),
	           Rcpp::_["result"]  = result
	       );

	return 0;
}

