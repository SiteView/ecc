// Oracle.h : main header file for the ORACLE DLL
//

#if !defined(AFX_ORACLE_H__CEE5BD90_CA51_4272_B541_30B0BA1721E9__INCLUDED_)
#define AFX_ORACLE_H__CEE5BD90_CA51_4272_B541_30B0BA1721E9__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "resource.h"		// main symbols

#include <iostream>
#include <vector>
#include <iterator>
#include <string>

#define OTL_ODBC // Compile OTL 4/ODBC
//#define OTL_ORA9I
#define OTL_STL // Turn on STL features
#ifndef OTL_ANSI_CPP
#define OTL_ANSI_CPP // Turn on ANSI C++ typecasts
#endif
#include "otlv4.h"

using namespace std;

/////////////////////////////////////////////////////////////////////////////
// COracleApp
// See Oracle.cpp for the implementation of this class
//

class COracleApp : public CWinApp
{
public:
	COracleApp();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(COracleApp)
	//}}AFX_VIRTUAL

	//{{AFX_MSG(COracleApp)
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

class row{
public:
	int f1;
	string f2;

	// default constructor
	row(){f1=0;}

	// destructor
	~row(){}

	// copy constructor
	row(const row& row)
	{
		f1=row.f1;
		f2=row.f2;
	}
 
	// assignment operator
	row& operator=(const row& row)
	{
		f1=row.f1;
		f2=row.f2;
		return *this;
	}

};

// redefined operator>> for reading row& from otl_stream
otl_stream& operator>>(otl_stream& s, row& row)
{
	s>>row.f1>>row.f2;
	return s;
}

// redefined operator<< for writing row& into otl_stream
otl_stream& operator<<(otl_stream& s, const row& row)
{
	s<<row.f1<<row.f2;
	return s;
}

// redefined operator<< writing row& into ostream
ostream& operator<<(ostream& s, const row& row)
{
	s<<"f1="<<row.f1<<", f2="<<row.f2;
	return s;
}


/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Visual C++ will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_ORACLE_H__CEE5BD90_CA51_4272_B541_30B0BA1721E9__INCLUDED_)
