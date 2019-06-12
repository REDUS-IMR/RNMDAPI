#include "xmlio/xmlinput.h"
#include "xmlio/xmlfile.h"
#include "xmlio/xmlzipfile.h"
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
	std::map<std::string, std::vector<std::string> >* tableHeaders;
	std::map<std::string, int >* prefixLens;
	std::vector<std::string>* tableNames;
	std::map<std::string, std::list<std::vector<std::string>* >* >* ret;
public:
	persistentData(std::map<std::string, std::vector<std::string> >* a, std::map<std::string, int >* b, std::map<std::string, std::list<std::vector<std::string>* >* >* c, std::vector<std::string>* h)
		:
		tableHeaders(a),
		prefixLens(b),
		tableNames(h),
		ret(c)
	{}
	std::map<std::string, std::vector<std::string> >* getTableHeaders()
	{
		return tableHeaders;
	}
	std::map<std::string, int >* getPrefixLens()
	{
		return prefixLens;
	}
	std::vector<std::string>* getTableNames()
	{
		return tableNames;
	}
	std::map<std::string, std::list<std::vector<std::string>* >* >* getRet()
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
	passingData(std::map<std::string, std::vector<std::string> >* a, std::map<std::string, int >* b, std::map<std::string, std::list<std::vector<std::string>* >* >* c, std::vector<std::string>* h, const char* d, std::vector<std::string>* e, const char* f, std::vector<std::string>* g) : persistentData(a, b, c, h),
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
	char* xsdUsed;
	std::map<std::string, std::list<std::vector<std::string>* >* >* ret;
	const Rcpp::List *xsdObjects;

public:
	returnData(Rcpp::List& a) :
		xsdObjects(&a)
	{}

	~returnData()
	{
		free(xsdUsed);
	}
	const Rcpp::List *getXsdObjects()
	{
		return xsdObjects;
	}
	void setXsdUsed(char* input)
	{
		xsdUsed = strdup(input);
	}
	void setReturnData(std::map<std::string, std::list<std::vector<std::string>* >* >& input)
	{
		ret = &input;
	}
	const char* getXsdUsed()
	{
		return xsdUsed;
	}
	std::map<std::string, std::list<std::vector<std::string>* >* >* getReturnData()
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
			std::map<std::string, std::vector<std::string> >* tableHeaders = pD->getTableHeaders();
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
			std::string NodeKey(column);
			std::vector<std::string> NodeKeys = (*tableHeaders)[parent];
			unsigned col = find(NodeKeys.begin(), NodeKeys.end(), NodeKey) - NodeKeys.begin();

			if( col >= NodeKeys.size() ) {
				Rcpp::Rcout << parent << "-> " << column << ": " << strdata << std::endl;
				Rcpp::stop("Found a value of an undefined element! Stopping process...");
			}

#ifdef DEBUG
			Rcpp::Rcout << "( " << col << " )" << std::endl;
			for (std::vector<std::string>::iterator it = contentPtr->begin() ; it != contentPtr->end(); ++it)
				Rcpp::Rcout << *it << " ";
			Rcpp::Rcout << '\n';
#endif
			// Put the value inside content
			(*contentPtr)[col] = strdata;
		}
	}
}

static void sElemHandler(XML::Element &elem, void *userData)
{
	// Get shared data
	passingData* pD = (passingData*) userData;

	std::vector<std::string>* parentPrefix = pD->getParentPrefix();

	std::map<std::string, std::vector<std::string> >* tableHeaders = pD->getTableHeaders();
	std::map<std::string, int >* prefixLens = pD->getPrefixLens();
	std::map<std::string, std::list<std::vector<std::string>* >* >* ret = pD->getRet();
	std::vector<std::string>* tableNames = pD->getTableNames();

	// Get root
	const char* root = elem.GetName();

	// Check if we are almost near the value
	std::string rK(root);
	unsigned check = find(tableNames->begin(), tableNames->end(), rK) - tableNames->begin();

	// Prepare prefix
	std::vector<std::string> prefix;
	std::vector<std::string>* prefixPtr;

	// Prepare parent
	const char* parent;

	// Prepare content pointer
	std::vector<std::string>* contentPtr;

	if( check < tableNames->size() ) {

		parent = root;

		// Getting result placeholder
		std::list<std::vector<std::string>* >* tempRes = (*ret)[(char*) root];

		// Getting header keys
		std::vector<std::string> NodeKeys = (*tableHeaders)[root];

		// Create content
		std::vector<std::string> *content = new std::vector<std::string>(NodeKeys.size());

		// Prefix resize
		int prefixSize = (*prefixLens)[(char*) root];
		prefix.resize(prefixSize);

		// Apply parent attributes to row and to children prefix
		for(unsigned long i = 0; i < parentPrefix->size(); i++) {
			(*content)[i] = prefix[i] = (*parentPrefix)[i];
		}

		// begin new element (and write attributes)
		if (elem.NumAttributes() > 0)
		{
			XML::Attribute a = elem.GetAttrList();
			while (a)
			{
				std::string NodeKey(a.GetName());
				unsigned col = find(NodeKeys.begin(), NodeKeys.end(), NodeKey) - NodeKeys.begin();
				if( col < NodeKeys.size() ) {
					(*content)[col] = prefix[col] = a.GetValue();
				}
				a = a.GetNext();
			}
		}


		// Push back the result
		tempRes->push_back(content);
		contentPtr = tempRes->back();
		prefixPtr = &prefix;

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

	passingData *newPD = new passingData(tableHeaders, prefixLens, ret, tableNames, parent, prefixPtr, root, contentPtr);

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
	delete newPD;
}

static void rootHandler(XML::Element &elem, void *userData)
{
	returnData *data = (returnData *) userData;

	const Rcpp::List *xsdObjects = data->getXsdObjects();

	const char* root = elem.GetName();
	char* xmlns = NULL;
	char* ns = NULL;

	// Getting the namespace
	if (elem.NumAttributes() > 0)
	{
		std::string xmlStr("xmlns");
		XML::Attribute a = elem.GetAttrList();
		while (a)
		{
			std::string NodeKey(a.GetName());
			std::size_t found = NodeKey.find(xmlStr);
			if (found!=std::string::npos) {
				xmlns = strdup(a.GetValue());
				// Try to get the namespace
				char *dup = strdup(NodeKey.c_str());
				strtok(dup, ":");
				char *tmp = strtok(NULL, ":");
				if(tmp != NULL)
					ns = strdup(tmp);
				free(dup);
				break;
			}
			a = a.GetNext();
		}
	} else {
		Rcpp::stop("Can not find the XML namespace, exiting...\n");
	}


	Rcpp::Rcout << "Root: " << root << "\n";
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

	Rcpp::Rcout << "Using XSD: " << xsd << std::endl;

	// Put xsd info into return data
	data->setXsdUsed(xsd);

	// Get XSD object
	Rcpp::List tableHeaders = Rcpp::as<Rcpp::List>((*xsdObjects)[xsd])["tableHeaders"];
	Rcpp::NumericVector prefixLens = Rcpp::as<Rcpp::List>((*xsdObjects)[xsd])["prefixLens"];

	// convert R headers to std c++
	std::vector<std::string>  tableNamesCpp;
	std::map<std::string, std::vector<std::string> > tableHeadersCpp;
	std::map<std::string, int > prefixLensCpp;
	Rcpp::CharacterVector tbNames(tableHeaders.names());

	std::string appendNS(":");
	if(ns != NULL)
		appendNS.insert(0, ns);

	for(Rcpp::CharacterVector::iterator it = tbNames.begin(); it != tbNames.end(); ++it) {
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

#ifdef DEBUG
	// Print out XML information
	Rcpp::Rcout << "Start from root: " << root << std::endl;
#endif

	// Prepare the result map
	std::map<std::string, std::list<std::vector<std::string>* >* >* ret = new std::map<std::string, std::list<std::vector<std::string>* >* >;

	// Pre-allocations
	for(unsigned i = 0; i < tableNamesCpp.size(); i++) {
		std::list<std::vector<std::string>* >* df = new std::list<std::vector<std::string>* >;
		(*ret)[tableNamesCpp[i]] = df;
#ifdef DEBUG
		std::cout << "size aft: " << tableNamesCpp[i] << "->" <<  (*ret)[tableNamesCpp[i]]->size() << std::endl;
#endif
	}

	// Get the first root content
	std::string tStr(root);
	std::vector<std::string> NodeKeys = tableHeadersCpp[tStr];
	std::vector<std::string> *content = new std::vector<std::string>(NodeKeys.size());

	(*ret)[tStr]->push_back(content);
	content = (*ret)[tStr]->back();

	// Create prefix storage
	std::vector<std::string> prefix(prefixLensCpp[tStr]);

	// begin new element (and write attributes)
	if (elem.NumAttributes() > 0)
	{
		std::vector<std::string> NodeKeys = tableHeadersCpp[root];
		XML::Attribute a = elem.GetAttrList();
		while (a)
		{
			std::string NodeKey(a.GetName());
			unsigned col = find(NodeKeys.begin(), NodeKeys.end(), NodeKey) - NodeKeys.begin();
			if( col < NodeKeys.size() ) {
				(*content)[col] = prefix[col] = a.GetValue();
			}
			a = a.GetNext();
		}
	}

#ifdef DEBUG
	for ( std::map<std::string, std::list<std::vector<std::string>* >* >::iterator it = ret->begin(); it != ret->end(); it++ )
	{
		Rcpp::Rcout << it->first  // string (key)
		            << ':'
		            << (it->second)->size()   // string's value
		            << std::endl ;
	}
	Rcpp::Rcout << std::endl;

	// Check first content
	for (std::vector<std::string>::iterator it = prefix.begin() ; it != prefix.end(); ++it)
		Rcpp::Rcout << *it << " ";
	Rcpp::Rcout << '\n';

	for (std::vector<std::string>::iterator it = content->begin() ; it != content->end(); ++it)
		Rcpp::Rcout << *it << " ";
	Rcpp::Rcout << '\n';
#endif

	// Prepare passing data store
	passingData *pD = new passingData(&tableHeadersCpp, &prefixLensCpp, ret, &tableNamesCpp, root, &prefix, root, content);

	// Handle sub-elements and data
	const XML::Handler handlers[] = {
		XML::Handler(sElemHandler),
		XML::Handler(sDataHandler),
		XML::Handler::END
	};

	elem.Process(handlers, pD);

	// Put all data into return data
	data->setReturnData(*ret);

	// After strdup()
	free(xmlns);
	if(ns != NULL)
		free(ns);

	delete pD;
}


std::string GetExt(const std::string& inputFileName)
{
	if(inputFileName.find_last_of(".") != std::string::npos)
		return inputFileName.substr(inputFileName.find_last_of(".")+1);
	return "";
}

// [[Rcpp::export]]
Rcpp::List readXmlCppStream(Rcpp::CharacterVector inputFile, Rcpp::List xsdObjects)
{

	std::string inputFileName(inputFile[0]);

	XML::Input *input;
	XML::FileInputStream *istream = NULL;
	XML::ZipInputStream *zstream = NULL;

	// Handles zip file input
	if(inputFileName.substr(inputFileName.find_last_of(".")+1) == "zip") {

		size_t basePos = inputFileName.find_last_of(".");
		std::string xmlfile(inputFileName.substr(0, basePos) + ".xml");

		Rcpp::Rcout << "Parsing XML: " << inputFileName << " inside " << inputFileName << " compressed file" << std::endl;

		zstream = new XML::ZipInputStream(inputFileName.c_str(), xmlfile.c_str());
		input = new XML::Input(*zstream);

	} else {

		// Print out XML information
		Rcpp::Rcout << "Parsing XML: " << inputFileName << std::endl;

		// open input stream
		istream = new XML::FileInputStream(inputFileName.c_str());
		input = new XML::Input(*istream);
	}

	// set up initial handler for Document
	XML::Handler handlers[] = {
		XML::Handler(rootHandler),
		XML::Handler::END
	};

	// Prepare return data class
	returnData *data = new returnData(xsdObjects);

	try {
		input->Process(handlers, data);
	}
	catch (const XML::ParseException &e)
	{
		Rcpp::Rcerr << "ERROR: " << e.What() << "(line " << e.GetLine() << ", column " << e.GetColumn() << ")\n";
	}


	Rcpp::List result = Rcpp::List::create();

	std::map<std::string, std::list<std::vector<std::string>* >* >* res = data->getReturnData();

	for ( std::map<std::string, std::list<std::vector<std::string>* >* >::iterator it = res->begin(); it != res->end(); it++ )
	{
		std::list<std::vector<std::string>* >* mylist = it->second;

		// Create counts
		unsigned maxRow = mylist->size();
		unsigned maxCol;
		if(maxRow == 0)
			maxCol = 0;
		else
			maxCol = mylist->front()->size();

#ifdef DEBUG
		Rcpp::Rcout << it->first
		            << ": "
		            << maxRow
			    << ", "
			    << maxCol
		            << std::endl;
#endif
		// Create matrix
		Rcpp::CharacterMatrix xy(maxRow, maxCol);
		result[it->first] = xy;

		// Iterate List
		unsigned currentRow = 0;
		for (std::list<std::vector<std::string>* >::iterator subit = mylist->begin(); subit != mylist->end(); ++subit) {
			for (unsigned j = 0; j < maxCol; j++) {
				if(((*(*subit))[j]).empty())
					xy(currentRow, j) = NA_STRING;
				else
					xy(currentRow, j) = (*(*subit))[j];
			}
			currentRow++;

			// Free up memory
			//std::vector<std::string>().swap((*(*subit)));
			delete *subit;
		}
		// Free up memory
		//std::list<std::vector<std::string>* >().swap(*mylist);
		delete mylist;
	}
	// Free up memory
	//std::map<std::string, std::list<std::vector<std::string>* >* >().swap(*res);
	delete res;

	// Return results and xsd name
	Rcpp::List rReturn = Rcpp::List::create(
	           Rcpp::_["xsd"]  = data->getXsdUsed(),
	           Rcpp::_["result"]  = result
	       );

	// Free up memory
	delete data;
	delete input;

	if(zstream)
		delete zstream;

	if(istream)
		delete istream;

	return rReturn;
}

