#include <string.h>
#include <stdio.h>
#include <iostream>
#include <fstream>
#include <vector>
#include <cstddef>
#include <cassert>
#define RAPIDXML_NO_EXCEPTIONS
#include "rapidxml-1.13/rapidxml.hpp"
#include <Rcpp.h>

using namespace Rcpp;
using namespace rapidxml;
using namespace std;

#if defined(RAPIDXML_NO_EXCEPTIONS)
void rapidxml::parse_error_handler(const char* what, void* where)
{
    stop("Parse error(@%p): %s\n", where, what);
}
#endif

// [[Rcpp::export]]
List readBioticXMLCpp(CharacterVector inputFile, List keys, NumericVector dim)
{
    // Getting the keys
    CharacterVector FSkeys= keys[0]; 
    CharacterVector CAkeys= keys[1]; 
    CharacterVector IDkeys= keys[2]; 
    
    // Preparing matrices
    CharacterMatrix FS(dim[0], FSkeys.size());
    std::fill(FS.begin(), FS.end(), CharacterVector::get_na()) ;
    CharacterMatrix CA(dim[1], CAkeys.size());
    std::fill(CA.begin(), CA.end(), CharacterVector::get_na()) ;
    CharacterMatrix ID(dim[2], IDkeys.size());
    std::fill(ID.begin(), ID.end(), CharacterVector::get_na()) ;

    // Add colnames
    colnames(FS) = FSkeys;
    colnames(CA) = CAkeys;
    colnames(ID) = IDkeys;

	Rcout << "Parsing biotic XML :" << inputFile << "." << endl;
	xml_document<> doc;
	xml_node<> * root_node;

	// Read xml using ifstream and buffer vector
	ifstream iFile (inputFile[0]);
	vector<char> buffer((istreambuf_iterator<char>(iFile)), istreambuf_iterator<char>());
	buffer.push_back('\0');

	// Parse the buffer using the xml file parsing library into doc 
	doc.parse<0>(&buffer[0]);

	// Find our root node
	// If start with missions
	if((root_node = doc.first_node("missions")))
		root_node = root_node->first_node("mission");
	else
		root_node = doc.first_node("mission");
    
    // Prepare counters
    int FSctr = 0;
    int CActr = 0;
    int IDctr = 0;

    // Iterate over the fishstations
    // We know the size for each of the levels, should be easier to unroll
    xml_node<> * fs_node = root_node->first_node("fishstation");
	for (; FSctr < dim[0]; )
	{
        char* serialnumber = NULL;
        if( !fs_node ) 
        {
            break;
        }
        else
        {
            // Get IDs
            serialnumber = fs_node->first_attribute("serialnumber")->value();
            FS(FSctr, 0) = serialnumber;
            // Parse values
            for( xml_node<> * it_fs_node = fs_node->first_node(); it_fs_node; it_fs_node = it_fs_node->next_sibling())
            {
                CharacterVector FSkey(it_fs_node->name());
                IntegerVector col = match(FSkey, FSkeys);
                if( col[0] > 0 ) {
                    //cout << serialnumber << " " << FSkey << " " <<  col[0] << endl;
                    FS(FSctr, col[0] - 1) = it_fs_node->value();
                }
            }
        }

        // Iterate over catchsamples, if any
        xml_node<> * ca_node = fs_node->first_node("catchsample");
        for (; CActr < dim[1];)
        {
            char* catchsampleid = NULL;
            if( !ca_node ) 
            {
                break;
            }
            else
            {
                // Get IDs
                CA(CActr, 0) = serialnumber;
                catchsampleid = ca_node->first_attribute("catchsampleid")->value();
                CA(CActr, 1) = catchsampleid;
                // Parse values
                for( xml_node<> * it_ca_node = ca_node->first_node(); it_ca_node; it_ca_node = it_ca_node->next_sibling())
                {
                    CharacterVector CAkey(it_ca_node->name());
                    IntegerVector col = match(CAkey, CAkeys);
                    if( col[0] > 0 ) {
                        //cout << catchsampleid << " " << CAkey << " " <<  col[0] << endl;
                        CA(CActr, col[0] - 1) = it_ca_node->value();
                    }
                }
            }
            
            // Interate over the individuals, if any
            xml_node<> * id_node = ca_node->first_node("individual");
            for (;  IDctr < dim[2]; )
	        {
                char* specimenid = NULL;
                if( !id_node )
                    break;
                else
                {
                    // Get IDs
                    ID(IDctr, 0) = serialnumber;
                    ID(IDctr, 1) = catchsampleid;
                    specimenid = id_node->first_attribute("specimenid")->value();
                    ID(IDctr, 2) = specimenid;
                    // Parse values
                    for( xml_node<> * it_id_node = id_node->first_node(); it_id_node; it_id_node = it_id_node->next_sibling())
                    {
                        CharacterVector IDkey(it_id_node->name());
                        IntegerVector col = match(IDkey, IDkeys);
                        if( col[0] > 0 ) {
                            //cout << specimenid << " " << IDkey << " " <<  col[0] << endl;
                            ID(IDctr, col[0] - 1) = it_id_node->value();
                        }
                    }
                    }
                
                // Iterate to next ID sibling
                IDctr++;
                //cout << "ID " << IDctr << endl;
                if( !(id_node = id_node->next_sibling()) )
                    break;
            }

            // Iterate to next CA sibling
            CActr++;
            //cout << "CA " << CActr << endl;
            if( !(ca_node = ca_node->next_sibling()) )
                break;
        }

        // Iterate to next FS sibling
        FSctr++;
        //cout << "FS " << FSctr << endl;
        if( !(fs_node = fs_node->next_sibling()) ) 
            break;
	}

    List ret = List::create(FS, CA, ID);
    return ret;
}