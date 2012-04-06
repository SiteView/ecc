// Oracle.cpp : Defines the initialization routines for the DLL.
//

#include "stdafx.h"
#include "Oracle.h"
#include <iostream>
#include <fstream>

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif


#ifdef WIN32
#pragma warning (disable : 4267)
#endif
//
//	Note!
//
//		If this DLL is dynamically linked against the MFC
//		DLLs, any functions exported from this DLL which
//		call into MFC must have the AFX_MANAGE_STATE macro
//		added at the very beginning of the function.
//
//		For example:
//
//		extern "C" BOOL PASCAL EXPORT ExportedFunction()
//		{
//			AFX_MANAGE_STATE(AfxGetStaticModuleState());
//			// normal function body here
//		}
//
//		It is very important that this macro appear in each
//		function, prior to any calls into MFC.  This means that
//		it must appear as the first statement within the 
//		function, even before any object variable declarations
//		as their constructors may generate calls into the MFC
//		DLL.
//
//		Please see MFC Technical Notes 33 and 58 for additional
//		details.
//

/////////////////////////////////////////////////////////////////////////////
// COracleApp

BEGIN_MESSAGE_MAP(COracleApp, CWinApp)
	//{{AFX_MSG_MAP(COracleApp)
		// NOTE - the ClassWizard will add and remove mapping macros here.
		//    DO NOT EDIT what you see in these blocks of generated code!
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// COracleApp construction

COracleApp::COracleApp()
{
	// TODO: add construction code here,
	// Place all significant initialization in InitInstance
}

/////////////////////////////////////////////////////////////////////////////
// The one and only COracleApp object

COracleApp theApp;


#define		__ServiceName__					"_ServiceName="
#define		__USERACCOUNT__					"_UserAccount="
#define		__PASSWORD__					"_PassWord="
#define		__CONNECTTIMEOUT__				"_ConnTimeout="
#define		__QUERYTIMEOUT__				"_QueryTimeout="
#define		__TABLESPACENAME__				"_TableSpaceName="
#define		__PROCESSNAME__					"_ProcessName="
#define     __SQL__                         "_SQL="
#define     __TEMPLATEID__                  "_TemplateID="
#define     __MONITORID__                  "_MonitorID="

char  szoutFile[256]={0};
BOOL GetResult( CString & strBuf );
void WriteResultFile(const char* chMsg, const char* filename, int nFlag);
void insert();
void select();
BOOL DBrlogon( CString strConn, otl_connect* pdb,char * szReturn);
BOOL DB_TABLESPACENAME(char *dbconn, char *uid, char *pwd,   char * strReturn, int& nSize);
					   
BOOL DB_PROCESSNAME(char *dbconn, char *uid, char *pwd,  char* szReturn,	   int nSize);

#include <string>

using namespace std;

void PrintDebugString(string szMsg)
{
#ifdef WIN32
    OutputDebugString("Oracle.dll-- ");
    OutputDebugString(szMsg.c_str());
    OutputDebugString(" --\n");
#endif
}


extern "C" __declspec(dllexport)
BOOL	TNSPing(CStringList& paramList, TCHAR* szReturn)
{
	AFX_MANAGE_STATE(AfxGetStaticModuleState());
	POSITION pos = paramList.FindIndex(0);
	CString strServiceName = _T("");
	while(pos)
	{
		CString strTemp = paramList.GetNext(pos);
		if(strTemp.Find("_servername=", 0) == 0)
		{
			strServiceName = strTemp.Right(strTemp.GetLength() - strlen("_servername="));
		}
	}

	if( strServiceName.IsEmpty() )
	{
		sprintf( szReturn , "error=%s", FuncGetStringFromIDS("SV_ORACLE",
            "ORACLE_SERVERNAME_NOT_FOUND"));
  		return FALSE;
	}
	
	STARTUPINFO si;
	PROCESS_INFORMATION pi;

	ZeroMemory( &si, sizeof(si) );
	si.cb = sizeof(si);
	ZeroMemory( &pi, sizeof(pi) );

	CFileException ex;
	CFile File;
	CString strFile("");

	char chParam[_MAX_PATH] = {0};
	sprintf( chParam, "tnsping %s", strServiceName);
#if _DEBUG
	strFile.Format("%s\\temp\\tnsping_%d.txt", FuncGetInstallPath(), 
        GetCurrentProcessId());
#else
	strFile.Format("%s\\MonitorManager\\temp\\tnsping_%d.txt", FuncGetInstallPath(), 
        GetCurrentProcessId());
#endif
	if( !File.Open(strFile,CFile::modeCreate|CFile::modeReadWrite , &ex) )
	{
		sprintf( szReturn , "error=%s", FuncGetStringFromIDS("SV_BASIC",
            "BASIC_CREATE_FILE_FAILE"));
		return FALSE;
	}

	si.dwFlags		=	 STARTF_USESTDHANDLES			;
	si.hStdOutput   =	 (HANDLE)  File.m_hFile			;


	// Start the child process. 
	if( !CreateProcess( NULL, // No module name (use command line). 
		chParam,			  // Command line. 
		NULL,				  // Process handle not inheritable. 
		NULL,                 // Thread handle not inheritable. 
		TRUE,			      // Set handle inheritance to FALSE. 
		0,				      // No creation flags. 
		NULL,			      // Use parent's environment block. 
		NULL,			      // Use parent's starting directory. 
		&si,			      // Pointer to STARTUPINFO structure.
		&pi )			      // Pointer to PROCESS_INFORMATION structure.
	) 
	{
		sprintf( szReturn, "error=%s%d$", FuncGetStringFromIDS("SV_BASIC",
            "BASIC_PROCESS_FAILED"), GetLastError());
		return FALSE;
	}

	// Wait until child process exits.
	WaitForSingleObject( pi.hProcess, 1000*60 );
	File.Close();

	// Close process and thread handles. 
	CloseHandle( pi.hProcess );
	CloseHandle( pi.hThread  );

	CString strParam("");
	if ( GetResult(strParam) )
	{
		CString strTime = _T("");
		if (strParam.Find("（" , 0) != -1)
		{
			strTime = strParam.Right( strParam.GetLength()-strParam.Find("（" , 0) );
		}
		else
		{
			strTime = strParam;
		}

		char chTime[_MAX_PATH] = {0};
		if (strTime == strParam)
		{
			sprintf( szReturn , "stats=%s$time=0$", strParam);
		}
		else
		{
			strncpy( chTime, strTime.Right( strTime.GetLength() -2 ), strTime.GetLength()-9 );

			sprintf( szReturn , "stats=%s$time=%d$", strParam.Left( strParam.Find("（",0)), atol(chTime) );
		}		
		
		return TRUE;
	}
	else
	{
		sprintf( szReturn , "error=%s", FuncGetStringFromIDS("SV_BASIC",
            "BASIC_PARSER_STRING_FAILED"));
		return FALSE;
	}
}

BOOL GetResult( CString & strBuf )
{
	char  chBuf[_MAX_PATH] = {0};
	FILE * fp = NULL;
	int iShift = 0;

	CString strTmpFile = _T("");
#if _DEBUG
	strTmpFile.Format("%s\\temp\\tnsping_%d.txt", FuncGetInstallPath(),
        GetCurrentProcessId());
#else
	strTmpFile.Format("%s\\MonitorManager\\temp\\tnsping_%d.txt", FuncGetInstallPath(),
        GetCurrentProcessId());
#endif
	if( (fp=fopen(strTmpFile, "r")) != NULL ) 
	{
		while( !feof(fp) )
		{
			fgets( chBuf, _MAX_PATH, fp );
//			if( iShift++ == 6 )
//			{
//				strBuf.Format( "%s", chBuf );
//				break;
//			}
		}
		strBuf.Format( "%s", chBuf );
		fclose(fp);
	}
	DeleteFile(strTmpFile);

	return !strBuf.IsEmpty();
}

extern "C" __declspec(dllexport)
BOOL	TestDBConnect(CStringList& paramList, TCHAR* szReturn)
{
	AFX_MANAGE_STATE(AfxGetStaticModuleState());

	CString strUser = _T("");
	CString strPwd = _T("");
	CString strService = _T("");
	CString strLog = _T("");
	
	POSITION pos = paramList.GetHeadPosition();
	while(pos)
	{
		CString strTmp = _T("");
		strTmp = paramList.GetNext(pos);
		if (0 == strTmp.Find(__USERACCOUNT__))
		{
			strUser = strTmp.Right(strTmp.GetLength() - strlen(__USERACCOUNT__));
		}
		else if (0 == strTmp.Find(__PASSWORD__))
		{
			strPwd = strTmp.Right(strTmp.GetLength() - strlen(__PASSWORD__));
		}
		else if (0 == strTmp.Find(__ServiceName__))
		{
			strService = strTmp.Right(strTmp.GetLength() - strlen(__ServiceName__));
		}
	}

	strLog.Format("_ServiceName=%s _UserAccount=%s _PassWord=%s", 
		strService, strUser, strPwd);

	if (strService.IsEmpty())
	{
		sprintf(szReturn, "error=%s", FuncGetStringFromIDS("SV_ORACLE",
            "ORACLE_SERVERNAME_NOT_FOUND"));
		return FALSE;
	}

	otl_connect db;
	CString strConn = _T("");

	otl_conn::initialize();
//	strConn.Format("%s/%s@%s", strUser, strPwd, strService);
	strConn.Format("UID=%s;PWD=%s;DSN=%s;QTO=F;APA=T;", strUser, strPwd, strService);

	try
	{
		int nBegin = GetTickCount();
		db.rlogon(strConn.GetBuffer(1));		
		int nEnd = GetTickCount();
		sprintf(szReturn, "status=OK$time=%d", nEnd-nBegin);
	}
	catch(otl_exception& p)
	{ // intercept OTL exceptions
//		cerr<<p.msg<<endl; // print out error message
//		cerr<<p.stm_text<<endl; // print out SQL that caused the error
//		cerr<<p.var_info<<endl; // print out the variable that caused the error
		sprintf(szReturn, "error=%s\r\n%s$", p.msg, p.var_info);
		strLog.Format("TestDBConnect ExceptionMsg=%s ExceptionInfo=%s", p.msg, p.var_info);
		db.logoff(); // disconnect from ODBC
		return FALSE;
	}
	
	db.logoff(); // disconnect from ODBC
	
	return TRUE;
}

extern "C" __declspec(dllexport)
BOOL	TestSQLExec(CStringList& paramList, TCHAR* szReturn)
{
	AFX_MANAGE_STATE(AfxGetStaticModuleState());
	CString strUser = _T("");
	CString strPwd = _T("");
	CString strService = _T("");
	CString strLog = _T("");
	
	POSITION pos = paramList.GetHeadPosition();
	while(pos)
	{
		CString strTmp = _T("");
		strTmp = paramList.GetNext(pos);
		if (0 == strTmp.Find(__USERACCOUNT__))
		{
			strUser = strTmp.Right(strTmp.GetLength() - strlen(__USERACCOUNT__));
		}
		else if (0 == strTmp.Find(__PASSWORD__))
		{
			strPwd = strTmp.Right(strTmp.GetLength() - strlen(__PASSWORD__));
		}
		else if (0 == strTmp.Find(__ServiceName__))
		{
			strService = strTmp.Right(strTmp.GetLength() - strlen(__ServiceName__));
		}
	}

	strLog.Format("_ServiceName=%s _UserAccount=%s _PassWord=%s", 
		strService, strUser, strPwd);

	if (strService.IsEmpty())
	{
		sprintf(szReturn, "error=%s", FuncGetStringFromIDS("SV_ORACLE",
            "ORACLE_SERVERNAME_NOT_FOUND"));
		return FALSE;
	}

	otl_connect db;
	CString strConn = _T("");

	otl_conn::initialize();
//	strConn.Format("%s/%s@%s", strUser, strPwd, strService);
	strConn.Format("UID=%s;PWD=%s;DSN=%s;QTO=F;APA=T;", strUser, strPwd, strService);

	try
	{
		int nBegin = GetTickCount();
		db.rlogon(strConn.GetBuffer(1));		
		otl_stream i(50, // buffer size
              "select * from dual",
                 // SELECT statement
              db // connect object
             ); 		
		int nEnd = GetTickCount();
		sprintf(szReturn, "status=OK$time=%d$", nEnd-nBegin);
	}
	catch(otl_exception& p)
	{ // intercept OTL exceptions
//		cerr<<p.msg<<endl; // print out error message
//		cerr<<p.stm_text<<endl; // print out SQL that caused the error
//		cerr<<p.var_info<<endl; // print out the variable that caused the error
		sprintf(szReturn, "error=%s\r\n%s$", p.msg, p.var_info);
		strLog.Format("TestSQLExec ExceptionMsg=%s ExceptionInfo=%s", p.msg, p.var_info);
		db.logoff(); // disconnect from ODBC
		return FALSE;
	}
	db.logoff(); // disconnect from ODBC
	
	return TRUE;
}
extern "C" __declspec(dllexport)
BOOL GetSQLResult(const char * strParas, char * szReturn, int& nSize)
{
	const char FileName[] = "SelectMonitorLOG.txt";
	ofstream write_txt;
	AFX_MANAGE_STATE(AfxGetStaticModuleState());
	CStringList paramList;
	MakeStringListByChar(paramList,strParas);
	CString strUser = _T("");
	CString strPwd = _T("");
	CString strService = _T("");
	CString strLog = _T("");
	CString strSQLSelect=_T("");

	////////////////////////////////
	int count = 0;
	char TradeName[50];
	//otl_datetime BeginDatetime,EndDatetime; 
	char BeginTime[100],EndTime[100];
	int TradeNum;
	char TradeDetail[1000];
	char result[10000];
	////////////////////////////////
	POSITION pos = paramList.GetHeadPosition();
	while(pos)
	{
		CString strTmp = _T("");
		strTmp = paramList.GetNext(pos);
		if (0 == strTmp.Find(__USERACCOUNT__))
		{
			strUser = strTmp.Right(strTmp.GetLength() - strlen(__USERACCOUNT__));
		}
		else if (0 == strTmp.Find(__PASSWORD__))
		{
			strPwd = strTmp.Right(strTmp.GetLength() - strlen(__PASSWORD__));
		}
		else if (0 == strTmp.Find(__ServiceName__))
		{
			strService = strTmp.Right(strTmp.GetLength() - strlen(__ServiceName__));
		}
		
	}

	strLog.Format("_ServiceName=%s _UserAccount=%s _PassWord=%s", 
		strService, strUser, strPwd);

	if (strService.IsEmpty())
	{
		sprintf(szReturn, "error=%s", FuncGetStringFromIDS("SV_ORACLE",
            "ORACLE_SERVERNAME_NOT_FOUND"));
		return FALSE;
	}

	otl_connect db;
	CString strConn = _T("");
	CString strInput ;
	strSQLSelect.Format("SELECT (SELECT yhhb.yhhb_dm || yhhb.mc FROM db_zswb.t_dm_gy_yhhb yhhb WHERE yhhb.yhhb_dm = t.yhhb_dm) 商行, lrsj_q 开始时间, lrsj_z 结束时间, sl 发生笔数, (SELECT db_zswb.F_LDQ_STRSUM(t.fhm || fhm.mc || '(' || sl || ')') FROM TABLE(t.fhmdetail) t , db_zswb.t_dm_xt_etsfhm fhm WHERE nvl(t.fhm,'null')=fhm.etsfhm_dm (+)) 明细 FROM TABLE(db_zswb.f_ldq_kkxx_enh(15/24/60,'Y') ) t ");
	//strSQLSelect="SELECT(SELECT yhhb.yhhb_dm || yhhb.mc  FROM db_zswb.t_dm_gy_yhhb yhhb WHERE yhhb.yhhb_dm = t.yhhb_dm) 商行,lrsj_q 开始时间, lrsj_z 结束时间,sl 发生笔数 FROM TABLE(db_zswb.f_ldq_kkxx_enh(30/24/60,'Y') )t WHERE t.fhm='BZC' AND SL>=10 ORDER BY t.yhhb_dm,t.lrsj_q";
	strConn.Format("Provider=OraOLEDB.Oracle;UID=%s;PWD=%s;DSN=%s;QTO=F;APA=T;", strUser, strPwd, strService);
	try
	{

		////////
		otl_connect::otl_initialize(); 
		db.rlogon(strConn.GetBuffer(1)); 
		otl_stream o(1,strSQLSelect,db);
		o.open(20000, // buffer size
         strSQLSelect.GetBuffer(0),
                 // SELECT statement
              db // connect object
             );
		if(o.eof())
		{
			sprintf(szReturn,"OracleSelect=%s$","ETS扣款正常");
		}	
		
		else
		{
			write_txt.open(FileName,ios::app);
			while(!o.eof())
			{ 
				o>>TradeName>>BeginTime>>EndTime>>TradeNum>>TradeDetail;
				write_txt<<"商户名："<<TradeName<<"开始时间："<<BeginTime<<"结束时间："<<EndTime<<"交易笔数："<<TradeNum<<"交易明细："<<TradeDetail<<endl;
				count++;
				sprintf(result,"%s,%s,%s,%s,%d,%s\n",result,TradeName,BeginTime,EndTime,TradeNum,TradeDetail);
			}
			sprintf(szReturn, "OracleSelect=%s$",result);
		}
		o.close();
		////////////////////////////////////////////////////////
		strInput = szReturn;
		MakeCharByString(szReturn,nSize,strInput);
	}
	catch(otl_exception& p)
	{ // intercept OTL exceptions
		//		cerr<<p.msg<<endl; // print out error message
		//		cerr<<p.stm_text<<endl; // print out SQL that caused the error
		//		cerr<<p.var_info<<endl; // print out the variable that caused the error
		sprintf(szReturn, "error=%s,%s$", p.msg, p.var_info);
		strLog.Format("GetOracleSelect ExceptionMsg=%s ExceptionInfo=%s", p.msg, p.var_info);
		db.logoff(); // disconnect from ODBC
		return FALSE;
	}
	db.logoff(); // disconnect from ODBC
	return TRUE;

}

//////////////////////////////////////////////////////////////////////////////////////

extern "C" __declspec(dllexport)
BOOL GetSQLResult_two(const char * strParas, char * szReturn, int& nSize)
{
	const char FileName[] = "SelectMonitorLOG.txt";
	ofstream write_txt;
	AFX_MANAGE_STATE(AfxGetStaticModuleState());
	CStringList paramList;
	MakeStringListByChar(paramList,strParas);
	CString strUser = _T("");
	CString strPwd = _T("");
	CString strService = _T("");
	CString strLog = _T("");
	CString strSQLSelect=_T("");

	////////////////////////////////
	int count = 0;
	char TradeName[50];
	//otl_datetime BeginDatetime,EndDatetime; 
	char BeginTime[100],EndTime[100];
	int TradeNum;
	char TradeDetail[1000];
	char result[10000];
	////////////////////////////////
	POSITION pos = paramList.GetHeadPosition();
	while(pos)
	{
		CString strTmp = _T("");
		strTmp = paramList.GetNext(pos);
		if (0 == strTmp.Find(__USERACCOUNT__))
		{
			strUser = strTmp.Right(strTmp.GetLength() - strlen(__USERACCOUNT__));
		}
		else if (0 == strTmp.Find(__PASSWORD__))
		{
			strPwd = strTmp.Right(strTmp.GetLength() - strlen(__PASSWORD__));
		}
		else if (0 == strTmp.Find(__ServiceName__))
		{
			strService = strTmp.Right(strTmp.GetLength() - strlen(__ServiceName__));
		}
		
	}

	strLog.Format("_ServiceName=%s _UserAccount=%s _PassWord=%s", 
		strService, strUser, strPwd);

	if (strService.IsEmpty())
	{
		sprintf(szReturn, "error=%s", FuncGetStringFromIDS("SV_ORACLE",
            "ORACLE_SERVERNAME_NOT_FOUND"));
		return FALSE;
	}

	otl_connect db;
	CString strConn = _T("");
	CString strInput ;
	strSQLSelect.Format("SELECT (SELECT yhhb.yhhb_dm || yhhb.mc FROM db_zswb.t_dm_gy_yhhb yhhb WHERE yhhb.yhhb_dm = t.yhhb_dm) 商行, lrsj_q 开始时间, lrsj_z 结束时间, sl 发生笔数, (SELECT db_zswb.F_LDQ_STRSUM(t.fhm || fhm.mc || '(' || sl || ')') FROM TABLE(t.fhmdetail) t , db_zswb.t_dm_xt_etsfhm fhm WHERE nvl(t.fhm,'null')=fhm.etsfhm_dm (+)) 明细 FROM TABLE(db_zswb.f_ldq_kkxx_enh(15/24/60,'Y') ) t WHERE t.fhm='BZC' AND SL>=10 ORDER BY t.yhhb_dm,t.lrsj_q ");
	//strSQLSelect="SELECT(SELECT yhhb.yhhb_dm || yhhb.mc  FROM db_zswb.t_dm_gy_yhhb yhhb WHERE yhhb.yhhb_dm = t.yhhb_dm) 商行,lrsj_q 开始时间, lrsj_z 结束时间,sl 发生笔数 FROM TABLE(db_zswb.f_ldq_kkxx_enh(30/24/60,'Y') )t WHERE t.fhm='BZC' AND SL>=10 ORDER BY t.yhhb_dm,t.lrsj_q";
	strConn.Format("Provider=OraOLEDB.Oracle;UID=%s;PWD=%s;DSN=%s;QTO=F;APA=T;", strUser, strPwd, strService);
	try
	{

		////////
		otl_connect::otl_initialize(); 
		db.rlogon(strConn.GetBuffer(1)); 
		otl_stream o(1,strSQLSelect,db);
		o.open(20000, // buffer size
         strSQLSelect.GetBuffer(0),
                 // SELECT statement
              db // connect object
             );
		if(o.eof())
		{
			sprintf(szReturn,"OracleSelect=%s$","ETS扣款正常");
		}	
		
		else
		{
			write_txt.open(FileName,ios::app);
			while(!o.eof())
			{ 
				o>>TradeName>>BeginTime>>EndTime>>TradeNum>>TradeDetail;
				write_txt<<"商户名："<<TradeName<<"开始时间："<<BeginTime<<"结束时间："<<EndTime<<"交易笔数："<<TradeNum<<"交易明细："<<TradeDetail<<endl;
				count++;
				sprintf(result,"%s,%s,%s,%s,%d,%s\n",result,TradeName,BeginTime,EndTime,TradeNum,TradeDetail);
			}
			sprintf(szReturn, "OracleSelect=%s$",result);
		}
		o.close();
		////////////////////////////////////////////////////////
		strInput = szReturn;
		MakeCharByString(szReturn,nSize,strInput);
	}
	catch(otl_exception& p)
	{ // intercept OTL exceptions
		//		cerr<<p.msg<<endl; // print out error message
		//		cerr<<p.stm_text<<endl; // print out SQL that caused the error
		//		cerr<<p.var_info<<endl; // print out the variable that caused the error
		sprintf(szReturn, "error=%s,%s$", p.msg, p.var_info);
		strLog.Format("GetOracleSelect ExceptionMsg=%s ExceptionInfo=%s", p.msg, p.var_info);
		db.logoff(); // disconnect from ODBC
		return FALSE;
	}
	db.logoff(); // disconnect from ODBC
	return TRUE;

}






//////////////////////////////////////////////////////////////////////////////////////

extern "C" __declspec(dllexport)
//BOOL	GetOracleInfo(CStringList& paramList, TCHAR* szReturn)
BOOL	GetOracleInfo(const char * strParas, char * szReturn, int& nSize)
{
	AFX_MANAGE_STATE(AfxGetStaticModuleState());
	CStringList paramList;
	MakeStringListByChar(paramList,strParas);

	CString strUser = _T("");
	CString strPwd = _T("");
	CString strService = _T("");
	CString strLog = _T("");
	
	POSITION pos = paramList.GetHeadPosition();
	while(pos)
	{
		CString strTmp = _T("");
		strTmp = paramList.GetNext(pos);
		if (0 == strTmp.Find(__USERACCOUNT__))
		{
			strUser = strTmp.Right(strTmp.GetLength() - strlen(__USERACCOUNT__));
		}
		else if (0 == strTmp.Find(__PASSWORD__))
		{
			strPwd = strTmp.Right(strTmp.GetLength() - strlen(__PASSWORD__));
		}
		else if (0 == strTmp.Find(__ServiceName__))
		{
			strService = strTmp.Right(strTmp.GetLength() - strlen(__ServiceName__));
		}
	}

	strLog.Format("_ServiceName=%s _UserAccount=%s _PassWord=%s", 
		strService, strUser, strPwd);

	if (strService.IsEmpty())
	{
		sprintf(szReturn, "error=%s", FuncGetStringFromIDS("SV_ORACLE",
            "ORACLE_SERVERNAME_NOT_FOUND"));
		return FALSE;
	}

	otl_connect db;
	CString strConn = _T("");

	otl_conn::initialize();
//	strConn.Format("%s/%s@%s", strUser, strPwd, strService);
	//strConn.Format("UID=%s;PWD=%s;DSN=%s;QTO=F;APA=T;", strUser, strPwd, strService);
	
	strConn.Format("Provider=OraOLEDB.Oracle;UID=%s;PWD=%s;DSN=%s;QTO=F;APA=T;", strUser, strPwd, strService);
	//WriteResultFile(strConn,szoutFile,1);
	try
	{
		CString strSQL = _T("");
		//CString strSQLFilePath = _T("");
		//strSQLFilePath.Format("%s\\groups\\sql.ini", FuncGetInstallPath());
		//CString strOracleRec;
		//strOracleRec.Format("%s\\groups\\oracle.log", FuncGetInstallPath());
		
		int nBegin = GetTickCount();
		//db.rlogon(strConn.GetBuffer(1));	
		if(FALSE==DBrlogon(strConn,&db,szReturn))  return FALSE;

//printf("1");
		//游标数
		strSQL.Empty();
		//GetPrivateProfileString("cursors", "cursors", "", strSQL.GetBuffer(4096), 4096, strSQLFilePath);
		//strSQL.Format("%s", strSQL);
		strSQL="select count(*) from v$open_cursor";
		otl_stream i(200, // buffer size
              strSQL.GetBuffer(0),
                 // SELECT statement
              db // connect object
             );
		int nTotalCursor = 0;
		i>>nTotalCursor;
		i.close();
//printf("2");		
		//取当前实例的session数
		strSQL.Empty();
		//GetPrivateProfileString("sessions", "sessions", "", strSQL.GetBuffer(4096), 4096, strSQLFilePath);
		//strSQL.Format("%s", strSQL);
		strSQL="select count(SID) from v$session";
		i.open(200, // buffer size
              strSQL.GetBuffer(0),
                 // SELECT statement
              db // connect object
             );
		int nTotalSession = 0;
		i>>nTotalSession;
		i.close();
//printf("3");
		//每秒事务数
		int nStartCommitNum = 0,
			nEndCommitNum = 0;
		int nStartRollBackNum = 0,
			nEndRollBackNum = 0;

		strSQL.Empty();
		//GetPrivateProfileString("usercommits", "usercommits", "", strSQL.GetBuffer(4096), 4096, strSQLFilePath);
		//strSQL.Format("%s", strSQL);
		strSQL="select value,name from v$sysstat  where name='user commits'";

		i.open(200, // buffer size
              strSQL.GetBuffer(0),
                 // SELECT statement
              db // connect object
             );
		i>>nStartCommitNum;
		i.close();
		
		strSQL.Empty();
		//GetPrivateProfileString("userrollbacks", "userrollbacks", "", strSQL.GetBuffer(4096), 4096, strSQLFilePath);
		strSQL="select value,name from v$sysstat  where name='user rollbacks'";

		//strSQL.Format("%s", strSQL);

		i.open(200, // buffer size
              strSQL.GetBuffer(0),
                 // SELECT statement
              db // connect object
             );
		i>>nStartRollBackNum;
		i.close();

		Sleep(2000);

		strSQL.Empty();
		//GetPrivateProfileString("usercommits", "usercommits", "", strSQL.GetBuffer(4096), 4096, strSQLFilePath);
		//strSQL.Format("%s", strSQL);
		strSQL="select value,name from v$sysstat  where name='user commits'";
		i.open(200, // buffer size
              strSQL.GetBuffer(0),
                 // SELECT statement
              db // connect object
             );
		i>>nEndCommitNum;
		i.close();

		strSQL.Empty();
		//GetPrivateProfileString("userrollbacks", "userrollbacks", "", strSQL.GetBuffer(4096), 4096, strSQLFilePath);
		//strSQL.Format("%s", strSQL);
		strSQL="select value,name from v$sysstat  where name='user rollbacks'";
		i.open(200, // buffer size
              strSQL.GetBuffer(0),
                 // SELECT statement	
              db // connect object
             );
		i>>nEndRollBackNum;
		i.close();
		int nTotalTransaction = (nEndCommitNum+nEndRollBackNum-nStartCommitNum-nStartRollBackNum)/2;
		
//printf("4");
		//数据库锁数量
		strSQL.Empty();
		//GetPrivateProfileString("locks", "locks", "", strSQL.GetBuffer(4096), 4096, strSQLFilePath);
		//strSQL.Format("%s", strSQL);
		strSQL="SELECT COUNT(*) FROM SYS.V_$LOCK WHERE TYPE IN ('UL','TM','TX')";

		i.open(200, // buffer size
              strSQL.GetBuffer(0),
                 // SELECT statement
              db // connect object
             );
		int nTotalLock = 0;
		i>>nTotalLock;
		i.close();
//printf("5");
		//是否有死锁
		strSQL.Empty();
		//GetPrivateProfileString("deadlocks", "deadlocks", "", strSQL.GetBuffer(4096), 4096, strSQLFilePath);
		//strSQL.Format("%s", strSQL);
		strSQL="select count(*) from v$locked_object";
		i.open(200, // buffer size
              strSQL.GetBuffer(0),
                 // SELECT statement
              db // connect object
             );

		//CString strlastOra;

		//GetPrivateProfileString("deadlocks", "deadlocks", "", lastOra, 255, strOracleRec);

		//int nlastOra =GetPrivateProfileInt(strService,"deadlocks",-1,strOracleRec);
		int nDeadLock = 0;
		i>>nDeadLock;
		//strlastOra.Format("%d",nDeadLock);
		//WritePrivateProfileString(strService,"deadlocks",strlastOra.GetBuffer(strlastOra.GetLength()),strOracleRec);

		//if(nlastOra ==-1) nDeadLock =0;
		//else nDeadLock=nDeadLock-nlastOra;

		i.close();
//printf("6");
		//共享池连接数
		
		//buffer cache hit rate
		strSQL.Empty();
		//GetPrivateProfileString("bufhitrate", "bufhitrate", "", strSQL.GetBuffer(4096), 4096, strSQLFilePath);
		//strSQL.Format("%s", strSQL);
		strSQL="select 100*(a.value+b.value-c.value) / (a.value+b.value)  from v$sysstat a, ";
		strSQL+=" v$sysstat b, v$sysstat c where a.name='db block gets' and b.name='consistent gets' and ";
		strSQL+=" c.name='physical reads'";
		i.open(200, // buffer size
              strSQL.GetBuffer(0),
                 // SELECT statement
              db // connect object
             );
		float fBufferHitRate = 0.0;
		i>>fBufferHitRate;
		i.close();
//printf("7");
		//library cache hit rate
		strSQL.Empty();
		//GetPrivateProfileString("libhitrate", "libhitrate", "", strSQL.GetBuffer(4096), 4096, strSQLFilePath);
		//strSQL.Format("%s", strSQL);
		strSQL=" Select 100*sum(pins -reloads)/sum(pins) from v$librarycache";
		i.open(200, // buffer size
              strSQL.GetBuffer(0),
                 // SELECT statement
              db // connect object
             );
		float fLibHitRate = 0.0;
		i>>fLibHitRate;
		i.close();
		/*
//printf("8");
		//总进程内存利用率
		strSQL.Empty();
		//GetPrivateProfileString("procmem", "procmem", "", strSQL.GetBuffer(4096), 4096, strSQLFilePath);
		//strSQL.Format("%s", strSQL);
		strSQL=" select 100*sum(PGA_USED_MEM)/sum(PGA_ALLOC_MEM) from v$process";
		i.open(200, // buffer size
              strSQL.GetBuffer(0),
                 // SELECT statement
              db // connect object
             );
		float fProcMemPre = 0.0;
		i>>fProcMemPre;
		i.close();
		*/
//printf("9");		
		int nEnd = GetTickCount();

		/*sprintf(szReturn, "SV_Cursor=%d$SV_Session=%d$SV_Transaction=%d$SV_Lock=%d$" \
			"SV_DeadLock=%d$SV_BufHitRate=%f$SV_LibHitRate=%f$SV_fProcMemPre=%f$SV_time=%d$", 
			nTotalCursor, nTotalSession, nTotalTransaction, nTotalLock,
			nDeadLock, fBufferHitRate, fLibHitRate, fProcMemPre, (nEnd-nBegin)/1000);
			*/
		sprintf(szReturn, "SV_Cursor=%d$SV_Session=%d$SV_Transaction=%d$SV_Lock=%d$" \
			"SV_DeadLock=%d$SV_BufHitRate=%f$SV_LibHitRate=%f$SV_time=%d$", 
			nTotalCursor, nTotalSession, nTotalTransaction, nTotalLock,
			nDeadLock, fBufferHitRate, fLibHitRate,  (nEnd-nBegin)/1000);
		CString strInput ;
		strInput =szReturn;
		MakeCharByString(szReturn,nSize,strInput);
	}
	catch(otl_exception& p)
	{ // intercept OTL exceptions
//		cerr<<p.msg<<endl; // print out error message
//		cerr<<p.stm_text<<endl; // print out SQL that caused the error
//		cerr<<p.var_info<<endl; // print out the variable that caused the error
		sprintf(szReturn, "error=%s,%s$", p.msg, p.var_info);
		strLog.Format("GetOracleInfo ExceptionMsg=%s ExceptionInfo=%s", p.msg, p.var_info);
		db.logoff(); // disconnect from ODBC
		return FALSE;
	}
	db.logoff(); // disconnect from ODBC

	return TRUE;
}


//数据库表空间监测
extern "C" __declspec(dllexport) 
BOOL OracleDB(const char * strParas, char * szReturn, int& nSize)
//(CStringList &paramList, char *szReturn)
{
	AFX_MANAGE_STATE(AfxGetStaticModuleState());

	CString		strDBConn = _T(""), 
				strQuery = _T("");

	CString		strDBMatch = _T(""), 
				strDBUser = _T(""), 
				strDBPass = _T(""), 
				strDBDriver = _T(""), 
				strTableSpaceName= _T(""), 
				strQFile = _T("");
	CString strLog = _T("");
	CStringList paramList;
	MakeStringListByChar(paramList,strParas);

	POSITION pos = paramList.FindIndex(0);
	while(pos)
	{
		CString strTemp = paramList.GetNext(pos);
		if(strTemp.Find(__ServiceName__) == 0)
		{
			strDBConn = strTemp.Right(strTemp.GetLength() - strlen(__ServiceName__));
		}

		else if(strTemp.Find(__USERACCOUNT__) == 0)
		{
			strDBUser = strTemp.Right(strTemp.GetLength() - strlen(__USERACCOUNT__));
		}
		else if(strTemp.Find(__PASSWORD__) == 0)
		{
			strDBPass = strTemp.Right(strTemp.GetLength() - strlen(__PASSWORD__));
		}
		else if(strTemp.Find(__TABLESPACENAME__) == 0) 
		{
			strTableSpaceName=strTemp.Right(strTemp.GetLength() - strlen(__TABLESPACENAME__));			
		}
	}

	strLog.Format("_ServiceName=%s _UserAccount=%s _PassWord=%s strTableSpaceName=%s", 
		strDBConn, strDBUser, strDBPass, strTableSpaceName);

	if(strDBConn.IsEmpty()) 
	{
		//IDS_ORACLEPERF_1
		sprintf(szReturn,"error=%s", FuncGetStringFromIDS("SV_DATABASE",
            "DATABASE_NAME_NULL"));
		return FALSE;
	}
	
	otl_connect db;
	CString strConn = _T("");

	otl_conn::initialize();
//	strConn.Format("%s/%s@%s", strDBUser, strDBPass, strDBConn);
	//UID=%s;PWD=%s;DSN=%s;QTO=F;APA=T;  UID=%s;PWD=%s;DSN=%s;QTO=F;APA=T;;QTO=F;APA=T;
	//strConn.Format("UID=%s;PWD=%s;DSN=%s;QTO=F;APA=T;", strDBUser, strDBPass, strDBConn);
	strConn.Format("UID=%s;PWD=%s;DSN=%s;QTO=F;APA=T;PLSQLRSet=1;", strDBUser, strDBPass, strDBConn);
	puts(strConn);
	try
	{
		db.rlogon(strConn.GetBuffer(1));
		//CString strSQLFilePath = _T("");
		//strSQLFilePath.Format("%s\\groups\\sql.ini", FuncGetInstallPath());
		CString strSQL = _T("");
		//GetPrivateProfileString("TabList", "TabList", "", strSQL.GetBuffer(4096), 4096, strSQLFilePath);

		//strSQL="SELECT A.BYTES/1024/1024 TOTAL, (A.BYTES-C.bytes)/1024/1024 USED, C.BYTES/1024/1024 FREE, (A.BYTES-C.bytes)*100/A.BYTES USED_PERCENT, ";
		//strSQL+=" (C.BYTES*100)/A.BYTES FREE_PERCENT, A.TABLESPACE_NAME FROM SYS.SM$TS_AVAIL A, SYS.SM$TS_FREE C WHERE A.TABLESPACE_NAME=C.TABLESPACE_NAME";

  //      //strSQL.Format("%s", strSQL);

		//strSQL += " and C.TABLESPACE_NAME='";
		//strSQL += strTableSpaceName;
		//strSQL += "'";

		strSQL.Format("SELECT  case v1.contents when 'TEMPORARY' then nvl(v4.bytes, 0) / 1024 / 1024 \
			else nvl(v2.bytes, 0) / 1024 / 1024 end as TotalMB,\
			case v1.contents when 'TEMPORARY' then nvl(v5.bytes, 0) / 1024 / 1024 \
			else (nvl(v2.bytes, 0) - nvl(v3.bytes, 0)) / 1024 / 1024 end as UsedMB,\
			case v1.contents when 'TEMPORARY' then nvl(nvl(v5.bytes, 0) / v4.bytes * 100, 0) \
			else nvl((nvl(v2.bytes, 0) - nvl(v3.bytes, 0)) / v2.bytes * 100, 0) end as UsedPer,\
			case v1.contents when 'TEMPORARY' then (nvl(v4.bytes, 0) - nvl(v5.bytes, 0)) / 1024 / 1024 \
			else nvl(v3.bytes, 0) / 1024 / 1024 end as FreeMB,\
			case v1.contents when 'TEMPORARY' then nvl((nvl(v4.bytes, 0) - nvl(v5.bytes, 0)) / v4.bytes * 100, 0) \
			else nvl(nvl(v3.bytes, 0) / v2.bytes * 100, 0) end as FreePer \
			FROM sys.dba_tablespaces v1,\
			(select sum(bytes) bytes from dba_data_files where tablespace_name = '%s') v2,\
			(select sum(bytes) bytes from dba_free_space where tablespace_name = '%s') v3,\
			(select sum(bytes) bytes from dba_temp_files where tablespace_name = '%s') v4,\
			(select sum(bytes_cached) bytes from v$temp_extent_pool where tablespace_name = '%s') v5 \
			where v1.tablespace_name = '%s'", strTableSpaceName, strTableSpaceName, strTableSpaceName, strTableSpaceName, strTableSpaceName);

		strSQL.Replace("\t", NULL);
		OutputDebugString(strSQL);

		otl_stream i(200, // buffer size
              strSQL.GetBuffer(0),
                 // SELECT statement
              db // connect object
             );
		double	fTotal = 0.0,
				fUsed = 0.0,
				fFree = 0.0,
				fUsedPer = 0.0,
				fFreePer = 0.0;
		//i>>fTotal>>fUsed>>fFree>>fUsedPer>>fFreePer;
		//fFreePer =100-fUsedPer;
		//fFree=fTotal-fUsed;
		i>>fTotal>>fUsed>>fUsedPer>>fFree>>fFreePer;

		i.close();
		sprintf(szReturn, "Total=%f$Used=%f$UsedPercent=%f$FreePercent=%f$FreeSpace=%f$", fTotal, fUsed, fUsedPer, fFreePer, fFree);

		CString strInput ;
		strInput =szReturn;
		MakeCharByString(szReturn,nSize,strInput);
	}
	catch(otl_exception& p)
	{ // intercept OTL exceptions
//		cerr<<p.msg<<endl; // print out error message
//		cerr<<p.stm_text<<endl; // print out SQL that caused the error
//		cerr<<p.var_info<<endl; // print out the variable that caused the error
		printf("error=%s\r\n%s$", p.msg, p.var_info);
		strLog.Format("OracleDB ExceptionMsg=%s ExceptionInfo=%s", p.msg, p.var_info);
		db.logoff(); // disconnect from ODBC
		CString sReturn;
		sReturn.Format("error=%s:%s$",p.msg,p.var_info);
		sReturn.Replace("\r\n",",");
		sprintf(szReturn,"%s",sReturn);
		return FALSE;
	}
	db.logoff(); // disconnect from ODBC
	
	return TRUE;
}
extern "C" __declspec(dllexport)
BOOL	GetSessions(const char * strParas, char * szReturn, int& nSize)
{
	AFX_MANAGE_STATE(AfxGetStaticModuleState());

	CString strUser = _T("");
	CString strPwd = _T("");
	CString strService = _T("");
	CString strLog = _T("");
	CString strMonitorID = _T("");

	CStringList paramList;
	MakeStringListByChar(paramList,strParas);
	
	POSITION pos = paramList.GetHeadPosition();
	while(pos)
	{
		CString strTmp = _T("");
		strTmp = paramList.GetNext(pos);
		if (0 == strTmp.Find(__USERACCOUNT__))
		{
			strUser = strTmp.Right(strTmp.GetLength() - strlen(__USERACCOUNT__));
		}
		else if (0 == strTmp.Find(__PASSWORD__))
		{
			strPwd = strTmp.Right(strTmp.GetLength() - strlen(__PASSWORD__));
		}
		else if (0 == strTmp.Find(__ServiceName__))
		{
			strService = strTmp.Right(strTmp.GetLength() - strlen(__ServiceName__));
		}
		else if(strTmp.Find(__MONITORID__) == 0) 
		{
			strMonitorID=strTmp.Right(strTmp.GetLength() - strlen(__MONITORID__));			
		}
	}

	CString strPath, strTime;
	strPath.Format("..\\data\\TmpIniFile\\ora_%s.ini", strMonitorID);
	COleDateTime tCurrTime =  COleDateTime::GetCurrentTime();
	strTime = tCurrTime.Format("%Y-%m-%d %H:%M:%S");

	strLog.Format("_ServiceName=%s _UserAccount=%s _PassWord=%s", 
		strService, strUser, strPwd);
	//WriteLog(strLog, 220);

	if (strService.IsEmpty())
	{
		sprintf(szReturn, "error=%s", FuncGetStringFromIDS("SV_ORACLE",
            "ORACLE_SERVERNAME_NOT_FOUND"));
		return FALSE;
	}

	otl_connect db;
	CString strConn = _T("");

	otl_conn::initialize();
//	strConn.Format("%s/%s@%s", strUser, strPwd, strService);
	strConn.Format("UID=%s;PWD=%s;DSN=%s", strUser, strPwd, strService);

	try
	{
		char pOldValue[256];
		COleDateTime tTime;
		BOOL bFlag = TRUE;
		db.rlogon(strConn.GetBuffer(1));
		CString strSQL = _T("");
		CString strSQLFilePath = _T("");
		//strSQLFilePath.Format("%s\\groups\\sql.ini", FuncGetInstallPath());
		//GetPrivateProfileString("Connections", "Connections", "", strSQL.GetBuffer(4096), 4096, strSQLFilePath);
		//strSQL.Format("%s", strSQL);
		//strSQL.Format("SELECT A.BYTES/1024/1024 TOTAL, \
		//	B.BYTES/1024/1024 USED, C.BYTES/1024/1024 FREE, (B.BYTES*100)/A.BYTES USED_PERCENT, \
		//	(C.BYTES*100)/A.BYTES FREE_PERCENT, A.TABLESPACE_NAME FROM SYS.SM$TS_AVAIL A, \
		//	SYS.SM$TS_USED B,SYS.SM$TS_FREE C \
		//	WHERE A.TABLESPACE_NAME=B.TABLESPACE_NAME \
		//	AND A.TABLESPACE_NAME=C.TABLESPACE_NAME");
		strSQL.Format("select value from v$sysstat where name='logons current'");



		otl_stream i(200, // buffer size
              strSQL.GetBuffer(0),
                 // SELECT statement
              db // connect object
             );
		int nSessionSum = 0;
		i>>nSessionSum;

		strSQL.Empty();
		strSQL="select value from v$sysstat where name='logons cumulative'";
		i.open(200, // buffer size
              strSQL.GetBuffer(0),
                 // SELECT statement
              db // connect object
             );
		unsigned int nLogonsCum = 0;
		i>>nLogonsCum;
		
		int nLogonsCum1m = -1;
		float fInterval = -1;
		int nSub = -1;
		GetPrivateProfileString("LogonsCum", "Time", "-1", pOldValue, 256, strPath);
		if(tTime.ParseDateTime(pOldValue))
		{
			GetPrivateProfileString("LogonsCum", "HistoryValue", "-1", pOldValue, 256, strPath);
			fInterval = (tCurrTime - tTime).GetTotalSeconds()/60;
			nSub = nLogonsCum - atoi(pOldValue);
			nLogonsCum1m = nSub/fInterval;
		}
		else if(atoi(pOldValue) != -1)
			return FALSE;

		if(nSub < 0 || fInterval < 0)
			bFlag = FALSE;		

		i.close();
		
		//写历史数据
		CString strTemp;
		WritePrivateProfileString("LogonsCum", "Time", strTime, strPath);
		strTemp.Format("%u", nLogonsCum);
		WritePrivateProfileString("LogonsCum", "HistoryValue", strTemp, strPath);

		if(bFlag)
		{
			sprintf(szReturn, "SV_sessions=%d$LogonsCum=%d$LogonsCum1m=%d$", nSessionSum, nLogonsCum, nLogonsCum1m);
			CString strInput ;
			strInput =szReturn;
			MakeCharByString(szReturn,nSize,strInput);
		}
		else
		{
			Sleep(1000);		
			GetSessions(strParas, szReturn, nSize);
		}
	}
	catch(otl_exception& p)
	{ // intercept OTL exceptions
//		cerr<<p.msg<<endl; // print out error message
//		cerr<<p.stm_text<<endl; // print out SQL that caused the error
//		cerr<<p.var_info<<endl; // print out the variable that caused the error
		sprintf(szReturn, "error=%s\r\n%s", p.msg, p.var_info);
		strLog.Format("GetSessions ExceptionMsg=%s ExceptionInfo=%s", p.msg, p.var_info);
//		WriteLog(strLog, 220);
		db.logoff(); // disconnect from ODBC
		return FALSE;
	}
	db.logoff(); // disconnect from ODBC
	
	return TRUE;
}

extern "C" __declspec(dllexport) 
BOOL OracleTableName(const char * strParas, char * strReturn, int& nSize)
					 //CStringList &paramList, CStringList &lstTexts, CStringList &lstValues)
{	
	AFX_MANAGE_STATE(AfxGetStaticModuleState());

	CString		strDBConn = _T(""), 
				strQuery = _T("");

	CString		strDBMatch = _T(""), 
				strDBUser = _T(""), 
				strDBPass = _T(""), 
				strDBDriver = _T(""), 
				strQFile = _T("");
	CString strLog = _T("");

	CStringList paramList;
	MakeStringListByChar(paramList,strParas);

//	WriteResultFile("===================================",szoutFile,1);
	POSITION pos = paramList.FindIndex(0);
	while(pos)
	{

		CString strTemp = paramList.GetNext(pos);
		//WriteResultFile(strTemp,szoutFile,1);
		if(strTemp.Find(__ServiceName__) == 0)
		{
			strDBConn = strTemp.Right(strTemp.GetLength() - strlen(__ServiceName__));
//			WriteResultFile(strDBConn,szoutFile,1);
		}

		else if(strTemp.Find(__USERACCOUNT__) == 0)
		{
			strDBUser = strTemp.Right(strTemp.GetLength() - strlen(__USERACCOUNT__));
		//	WriteResultFile(strDBUser,szoutFile,1);
		}
		else if(strTemp.Find(__PASSWORD__) == 0)
		{
			strDBPass = strTemp.Right(strTemp.GetLength() - strlen(__PASSWORD__));
//			WriteResultFile(strDBPass,szoutFile,1);
		}
	}

//	WriteResultFile("===================================",szoutFile,1);
	
	strLog.Format("_ServiceName=%s _UserAccount=%s _PassWord=%s", 
		strDBConn, strDBUser, strDBPass);

//	WriteResultFile(strLog,szoutFile,1) ;
	if(strDBConn.IsEmpty())
	{
		//IDS_ORACLEPERF_1
		//sprintf(szReturn,"error=%s", FuncGetStringFromIDS("SV_DATABASE",
        //    "DATABASE_NAME_NULL"));
		return FALSE;
	}
//	WriteResultFile(strLog,szoutFile,1) ;

	BOOL bResult = DB_TABLESPACENAME(strDBConn.GetBuffer(strDBConn.GetLength()), 
							strDBUser.GetBuffer(strDBUser.GetLength()), 
							strDBPass.GetBuffer(strDBPass.GetLength()), 
							strReturn,
							nSize);

	return bResult ;
	
}

BOOL DB_TABLESPACENAME(char *dbconn, char *uid, char *pwd, char * strReturn, int& nSize)
					   //CStringList &lstTexts,		   CStringList &lstValues)
{
	otl_connect db;
	CString strConn = _T("");
	CString strLog = _T("");
	otl_conn::initialize();
//	strConn.Format("%s/%s@%s", uid, pwd, dbconn);


	strConn.Format("UID=%s;PWD=%s;DSN=%s;QTO=F;APA=T;", uid, pwd, dbconn);
	try
	{
		db.rlogon(strConn.GetBuffer(1));
		otl_stream i(100, // buffer size
              "SELECT tablespace_name FROM DBA_TABLESPACES",
                 // SELECT statement
              db // connect object
             );
		char szTableSpaceName[256] = {0};
		char* p=strReturn;
		while(!i.eof())
		{
			i>>szTableSpaceName;
			//lstTexts.AddTail(szTableSpaceName);
			//lstValues.AddTail(szTableSpaceName);
			sprintf(p,"%s=%s",szTableSpaceName,szTableSpaceName);
			p+= 2* strlen(szTableSpaceName)+2;
		}
		i.close();
	}
	catch(otl_exception& p)
	{ // intercept OTL exceptions
//		cerr<<p.msg<<endl; // print out error message
//		cerr<<p.stm_text<<endl; // print out SQL that caused the error
//		cerr<<p.var_info<<endl; // print out the variable that caused the error
		printf("error=%s\r\n%s", p.msg, p.var_info);
		strLog.Format("DB_TABLESPACENAME ExceptionMsg=%s ExceptionInfo=%s", p.msg, p.var_info);
		db.logoff(); // disconnect from ODBC
		sprintf(strReturn,"%s=%s",p.msg,p.var_info);
		//lstTexts.AddTail(p.msg);
		//lstValues.AddTail(p.var_info);
		
		
		return FALSE;
	}
	db.logoff(); // disconnect from ODBC
			
	return TRUE;
}

extern "C" __declspec(dllexport) 
BOOL OracleProcName(const char * strParas, char * strReturn, int& nSize)
//(CStringList &paramList, CStringList &lstTexts, CStringList &lstValues)
{	
	AFX_MANAGE_STATE(AfxGetStaticModuleState());

	CString		strDBConn = _T(""), 
				strQuery = _T("");

	CString		strDBMatch = _T(""), 
				strDBUser = _T(""), 
				strDBPass = _T(""), 
				strDBDriver = _T(""), 
				strQFile = _T("");
	CString strLog = _T("");

	CStringList paramList;
	MakeStringListByChar(paramList,strParas);


	POSITION pos = paramList.FindIndex(0);
	while(pos)
	{
		CString strTemp = paramList.GetNext(pos);
		if(strTemp.Find(__ServiceName__) == 0)
		{
			strDBConn = strTemp.Right(strTemp.GetLength() - strlen(__ServiceName__));
		}

		else if(strTemp.Find(__USERACCOUNT__) == 0)
		{
			strDBUser = strTemp.Right(strTemp.GetLength() - strlen(__USERACCOUNT__));
		}
		else if(strTemp.Find(__PASSWORD__) == 0)
		{
			strDBPass = strTemp.Right(strTemp.GetLength() - strlen(__PASSWORD__));
		}
	}

	strLog.Format("_ServiceName=%s _UserAccount=%s _PassWord=%s", 
		strDBConn, strDBUser, strDBPass);

	if(strDBConn.IsEmpty())
	{
		//IDS_ORACLEPERF_1
		//sprintf(szReturn,"error=%s", FuncGetStringFromIDS("SV_DATABASE",
        //    "DATABASE_NAME_NULL"));
		return FALSE;
	}

    PrintDebugString(strLog.GetBuffer(0));


	BOOL bResult = DB_PROCESSNAME(strDBConn.GetBuffer(strDBConn.GetLength()), 
							strDBUser.GetBuffer(strDBUser.GetLength()), 
							strDBPass.GetBuffer(strDBPass.GetLength()), 
							strReturn,
							nSize);

	return bResult ;
	
}

BOOL DB_PROCESSNAME(char *dbconn, char *uid, char *pwd,     char* szReturn,	   int nSize)
{
	otl_connect db;
	CString strConn = _T("");
	CString strLog = _T("");

    PrintDebugString("otl_conn initialize!");

	otl_conn::initialize();
//	strConn.Format("%s/%s@%s", uid, pwd, dbconn);
	strConn.Format("UID=%s;PWD=%s;DSN=%s;QTO=F;APA=T;", uid, pwd, dbconn);
	try
	{
        PrintDebugString(strConn.GetBuffer(0));

        
		db.rlogon(strConn.GetBuffer(1));
		otl_stream i(100, // buffer size
              "SELECT Program, pid FROM v$process",
                 // SELECT statement
              db // connect object
             );
		char szTableSpaceName[256] = {0};
		int	nPid = 0;
		CString strName = _T("");
		char *p;
		int j=0;
		p=szReturn;
	
        PrintDebugString("get data succ!");
		while(!i.eof())
		{
			
			i>>szTableSpaceName>>nPid;
			strName.Format("%s(PID:%d)", szTableSpaceName, nPid);

			if(j>0)
			{
				sprintf(p,"%s=%s",strName,strName);
				//printf("%s\n", p);
				p+= 2* strlen(strName)+2;
			}
			j++;
			//lstTexts.AddTail(strName);
			//lstValues.AddTail(strName);
		}
		
        PrintDebugString("close recordset");
		i.close();
/*
		/////////////////////////////去头去尾,chenxingang
		if((!lstTexts.IsEmpty())&&(!lstValues.IsEmpty()))
		{
			lstTexts.RemoveHead();
			lstValues.RemoveHead();
		}
		
		if((!lstTexts.IsEmpty())&&(!lstValues.IsEmpty()))
		{
			lstTexts.RemoveTail();
			lstValues.RemoveTail();
		}
		////////////////////////////////
		*/
	}
	catch(otl_exception& p)
	{ // intercept OTL exceptions
//		cerr<<p.msg<<endl; // print out error message
//		cerr<<p.stm_text<<endl; // print out SQL that caused the error
//		cerr<<p.var_info<<endl; // print out the variable that caused the error
		printf("error=%s\r\n%s", p.msg, p.var_info);
		strLog.Format("DB_PROCESSNAME ExceptionMsg=%s ExceptionInfo=%s", p.msg, p.var_info);
		db.logoff(); // disconnect from ODBC
		sprintf(szReturn,"error=%s:%s",p.msg,p.var_info);
//		lstTexts.AddTail(p.msg);
//		lstValues.AddTail(p.var_info);
		return FALSE;
	}
	db.logoff(); // disconnect from ODBC
			
	return TRUE;
}

//进程内存利用率
extern "C" __declspec(dllexport)
BOOL GetProcMemUsePercent(const char * strParas, char * szReturn, int& nSize)
//(CStringList &paramList, char *szReturn)
{
	AFX_MANAGE_STATE(AfxGetStaticModuleState());

	/*for debug zjw
	CStringList strlist;
	MakeStringListByChar(strlist, strParas);
	POSITION pos1=NULL;
	pos1 = strlist.GetHeadPosition();
	CString strItem = "";

	CString str, sTime;
	sTime = COleDateTime::GetCurrentTime().Format("%Y-%m-%d %H:%M:%S");
	while(pos1 != NULL)
	{
		strItem = strlist.GetNext(pos1);
		strItem += " for debug zjw";
		ofstream fout("orapara.txt",ios::app);
		fout << strItem <<"\r\n"; 
		fout << flush; 
		fout.close(); 
	}
	ofstream fout("orapara.txt",ios::app);
	fout <<"\r\n"; 
	fout << flush; 
	fout.close(); 
	*/

	CString		strDBConn = _T(""), 
				strQuery = _T("");
	CString strLog = _T("");

	CString		strDBMatch = _T(""), 
				strDBUser = _T(""), 
				strDBPass = _T(""), 
				strDBDriver = _T(""), 
				strProcName= _T(""), 
				strQFile = _T(""),
				strMonitorID = _T("");

	CStringList paramList;
	MakeStringListByChar(paramList,strParas);

	POSITION pos = paramList.FindIndex(0);
	while(pos)
	{
		CString strTemp = paramList.GetNext(pos);
		if(strTemp.Find(__ServiceName__) == 0)
		{
			strDBConn = strTemp.Right(strTemp.GetLength() - strlen(__ServiceName__));
		}

		else if(strTemp.Find(__USERACCOUNT__) == 0)
		{
			strDBUser = strTemp.Right(strTemp.GetLength() - strlen(__USERACCOUNT__));
		}
		else if(strTemp.Find(__PASSWORD__) == 0)
		{
			strDBPass = strTemp.Right(strTemp.GetLength() - strlen(__PASSWORD__));
		}
		else if(strTemp.Find(__PROCESSNAME__) == 0) 
		{
			strProcName=strTemp.Right(strTemp.GetLength() - strlen(__PROCESSNAME__));			
		}
		else if(strTemp.Find(__MONITORID__) == 0) 
		{
			strMonitorID=strTemp.Right(strTemp.GetLength() - strlen(__MONITORID__));			
		}
	}

	CString strPath, strTime;
	strPath.Format("..\\data\\TmpIniFile\\ora_%s.ini", strMonitorID);
	COleDateTime tCurrTime =  COleDateTime::GetCurrentTime();
	strTime = tCurrTime.Format("%Y-%m-%d %H:%M:%S");

	strLog.Format("_ServiceName=%s _UserAccount=%s _PassWord=%s", 
		strDBConn, strDBUser, strDBPass);

	if(strDBConn.IsEmpty())
	{
		//IDS_ORACLEPERF_1
		sprintf(szReturn,"error=%s", FuncGetStringFromIDS("SV_DATABASE",
            "DATABASE_NAME_NULL"));
		return FALSE;
	}
	//if (strProcName.IsEmpty())
	//{
	//	sprintf(szReturn,"error=%s",FuncGetStringFromIDS("SV_BASIC",
 //           "BASIC_NOT_SELECT_MONITOR_PROCESS"));
	//	return FALSE;
	//}

	//CString strProcessName = _T("");
	//strProcessName = strProcName.Mid(strProcName.Find(":")+1, strProcName.GetLength()-strProcName.Find(":")-2);
	
	otl_connect db;
	CString strConn = _T("");

	otl_conn::initialize();
//	strConn.Format("%s/%s@%s", strDBUser, strDBPass, strDBConn);
	strConn.Format("UID=%s;PWD=%s;DSN=%s;QTO=F;APA=T;", strDBUser, strDBPass, strDBConn);
	try
	{
		char pOldValue[256];
		COleDateTime tTime;
		BOOL bFlag = TRUE;
		db.rlogon(strConn.GetBuffer(1));		
		CString strSQL = _T("");
		strSQL.Format("select 100*sum(PGA_USED_MEM)/sum(PGA_ALLOC_MEM) from v$process");// where PID=%s", strProcessName);
		try
		{
			otl_stream i(200, // buffer size
              strSQL.GetBuffer(0),
                 // SELECT statement
              db // connect object
             );
		}
		catch(otl_exception& p)
		{
			strSQL.Empty();
			strSQL="select (select sum(value) from sys.v_$sesstat where statistic#=20)/\
			(select (select count(*) from sys.v_$session)*(2048576+a.value+b.value)   pga_size \
			from v$parameter a,v$parameter b where a.name = 'sort_area_size' and b.name = 'hash_area_size')*100 from dual";
			strSQL.Replace("\t", NULL);
		}

		otl_stream i(200, // buffer size
              strSQL.GetBuffer(0),
                 // SELECT statement
              db // connect object
             );
		float fProcMemPer = 0.0;
		i>>fProcMemPer;

		strSQL.Empty();
		strSQL="select sum(value) total_physical_io from v$sysstat where name in ('physical reads','physical reads direct',\
		'physical reads direct (lob)','physical writes','physical writes direct','physical writes direct (lob)',\
		'physical writes non checkpoint')";
		strSQL.Replace("\t", NULL);
		i.open(200, // buffer size
              strSQL.GetBuffer(0),
                 // SELECT statement
              db // connect object
             );
		unsigned  int nTotalPhysicalIO = 0;
		i>>nTotalPhysicalIO;
		
		int nPhysicalIO1m = -1;
		float fInterval = -1;
		int nSub = -1;
		GetPrivateProfileString("TotalPhysicalIO", "Time", "-1", pOldValue, 256, strPath);
		if(tTime.ParseDateTime(pOldValue))
		{
			GetPrivateProfileString("TotalPhysicalIO", "HistoryValue", "-1", pOldValue, 256, strPath);
			fInterval = (tCurrTime - tTime).GetTotalSeconds()/60;
			nSub = nTotalPhysicalIO - atoi(pOldValue);
			nPhysicalIO1m = nSub/fInterval;
		}
		else if(atoi(pOldValue) != -1)
			return FALSE;

		if(nSub < 0 || fInterval < 0)
			bFlag = FALSE;		

		strSQL.Empty();
		strSQL="select sum(value) total_logical_io from v$sysstat where name in ('db block gets','db block changes',\
		'consistent gets','consistent changes')";
		strSQL.Replace("\t", NULL);
		i.open(200, // buffer size
              strSQL.GetBuffer(0),
                 // SELECT statement
              db // connect object
             );
		unsigned int nTotalLogicalIO = 0;
		i>>nTotalLogicalIO;

		int nLogicalIO1m = -1;
		GetPrivateProfileString("TotalLogicalIO", "Time", "-1", pOldValue, 256, strPath);
		if(tTime.ParseDateTime(pOldValue))
		{
			GetPrivateProfileString("TotalLogicalIO", "HistoryValue", "-1", pOldValue, 256, strPath);
			fInterval = (tCurrTime - tTime).GetTotalSeconds()/60;
			nSub = nTotalLogicalIO - atoi(pOldValue);
			nLogicalIO1m = nSub/fInterval;
		}
		else if(atoi(pOldValue) != -1)
			return FALSE;

		if(nSub < 0 || fInterval < 0)
			bFlag = FALSE;		

		strSQL.Empty();
		strSQL="select value from v$sysstat where name='sorts (disk)'";
		i.open(200, // buffer size
              strSQL.GetBuffer(0),
                 // SELECT statement
              db // connect object
             );
		unsigned  int nSortsDisk = 0;
		i>>nSortsDisk;
		
		int nSortsDisk1m = -1;
		GetPrivateProfileString("SortsDisk", "Time", "-1", pOldValue, 256, strPath);
		if(tTime.ParseDateTime(pOldValue))
		{
			GetPrivateProfileString("SortsDisk", "HistoryValue", "-1", pOldValue, 256, strPath);
			float fInterval = (tCurrTime - tTime).GetTotalSeconds()/60;
			nSub = nSortsDisk - atoi(pOldValue);
			nSortsDisk1m = nSub/fInterval;
		}
		else if(atoi(pOldValue) != -1)
			return FALSE;

		if(nSub < 0)
			bFlag = FALSE;		

		strSQL.Empty();
		strSQL="select value from v$sysstat where name='sorts (memory)'";
		i.open(200, // buffer size
              strSQL.GetBuffer(0),
                 // SELECT statement
              db // connect object
             );
		unsigned int nSortsMem = 0;
		i>>nSortsMem;

		int nSortsMem1m = -1;
		GetPrivateProfileString("SortsMem", "Time", "-1", pOldValue, 256, strPath);
		if(tTime.ParseDateTime(pOldValue))
		{
			GetPrivateProfileString("SortsMem", "HistoryValue", "-1", pOldValue, 256, strPath);
			float fInterval = (tCurrTime - tTime).GetTotalSeconds()/60;
			nSub = nSortsMem - atoi(pOldValue);
			nSortsMem1m = nSub/fInterval;
		}
		else if(atoi(pOldValue) != -1)
			return FALSE;

		if(nSub < 0 || fInterval < 0)
			bFlag = FALSE;		

		strSQL.Empty();
		strSQL="select value from v$sysstat where name='user commits'";
		i.open(200, // buffer size
              strSQL.GetBuffer(0),
                 // SELECT statement
              db // connect object
             );
		unsigned  int nUserCommits = 0;
		i>>nUserCommits;
		
		int nUserCommits1m = -1;
		GetPrivateProfileString("UserCommits", "Time", "-1", pOldValue, 256, strPath);
		if(tTime.ParseDateTime(pOldValue))
		{
			GetPrivateProfileString("UserCommits", "HistoryValue", "-1", pOldValue, 256, strPath);
			float fInterval = (tCurrTime - tTime).GetTotalSeconds()/60;
			nSub = nUserCommits - atoi(pOldValue);
			nUserCommits1m = nSub/fInterval;
		}
		else if(atoi(pOldValue) != -1)
			return FALSE;

		if(nSub < 0 || fInterval < 0)
			bFlag = FALSE;		

		strSQL.Empty();
		strSQL="select value from v$sysstat where name='user rollbacks'";
		i.open(200, // buffer size
              strSQL.GetBuffer(0),
                 // SELECT statement
              db // connect object
             );
		unsigned int nUserRollbacks = -1;
		i>>nUserRollbacks;

		int nUserRollbacks1m = -1;
		GetPrivateProfileString("UserRollbacks", "Time", "-1", pOldValue, 256, strPath);
		if(tTime.ParseDateTime(pOldValue))
		{
			GetPrivateProfileString("UserRollbacks", "HistoryValue", "-1", pOldValue, 256, strPath);
			float fInterval = (tCurrTime - tTime).GetTotalSeconds()/60;
			nSub = nUserRollbacks - atoi(pOldValue);
			nUserRollbacks1m = nSub/fInterval;
		}
		else if(atoi(pOldValue) != -1)
			return FALSE;

		if(nSub < 0 || fInterval < 0)
			bFlag = FALSE;		

		i.close();

		//写历史数据
		CString strTemp;
		WritePrivateProfileString("TotalPhysicalIO", "Time", strTime, strPath);
		strTemp.Format("%u", nTotalPhysicalIO);
		WritePrivateProfileString("TotalPhysicalIO", "HistoryValue", strTemp, strPath);

		WritePrivateProfileString("TotalLogicalIO", "Time", strTime, strPath);
		strTemp.Format("%u", nTotalLogicalIO);
		WritePrivateProfileString("TotalLogicalIO", "HistoryValue", strTemp, strPath);

		WritePrivateProfileString("SortsDisk", "Time", strTime, strPath);	
		strTemp.Format("%u", nSortsDisk);
		WritePrivateProfileString("SortsDisk", "HistoryValue", strTemp, strPath);

		WritePrivateProfileString("SortsMem", "Time", strTime, strPath);
		strTemp.Format("%u", nSortsMem);
		WritePrivateProfileString("SortsMem", "HistoryValue", strTemp, strPath);

		WritePrivateProfileString("UserCommits", "Time", strTime, strPath);	
		strTemp.Format("%u", nUserCommits);
		WritePrivateProfileString("UserCommits", "HistoryValue", strTemp, strPath);

		WritePrivateProfileString("UserRollbacks", "Time", strTime, strPath);
		strTemp.Format("%u", nUserRollbacks);
		WritePrivateProfileString("UserRollbacks", "HistoryValue", strTemp, strPath);

		if(bFlag)
		{
			CString strReturn;
			strReturn.Format("ProcMemPer=%.2f$TotalPhysicalIO=%d$PhysicalIO1m=%d$TotalLogicalIO=%d$LogicalIO1m=%d$\
				SortsDisk=%d$SortsDisk1m=%d$SortsMem=%d$SortsMem1m=%d$UserCommits=%d$UserCommits1m=%d$\
				UserRollbacks=%d$UserRollbacks1m=%d$", 
				fProcMemPer, nTotalPhysicalIO, nPhysicalIO1m, nTotalLogicalIO,nLogicalIO1m,nSortsDisk,nSortsDisk1m,
				nSortsMem,nSortsMem1m,nUserCommits,nUserCommits1m,nUserRollbacks,nUserRollbacks1m);
			strReturn.Replace("\t", NULL);
			strcpy(szReturn, strReturn);
			CString strInput ;
			strInput =szReturn;
			MakeCharByString(szReturn,nSize,strInput);
		}
		else
		{
			Sleep(1000);		
			GetProcMemUsePercent(strParas, szReturn, nSize);
		}
	}
	catch(otl_exception& p)
	{ // intercept OTL exceptions
//		cerr<<p.msg<<endl; // print out error message
//		cerr<<p.stm_text<<endl; // print out SQL that caused the error
//		cerr<<p.var_info<<endl; // print out the variable that caused the error
		printf("error=%s\r\n%s$", p.msg, p.var_info);
		strLog.Format("GetProcMemUsePercent ExceptionMsg=%s ExceptionInfo=%s", p.msg, p.var_info);
		db.logoff(); // disconnect from ODBC
		CString sReturn;
		sReturn.Format("error=%s:%s$",p.msg,p.var_info);
		sReturn.Replace("\r\n",",");
		sprintf(szReturn,"%s",sReturn);
		return FALSE;
	}
	db.logoff(); // disconnect from ODBC
	
	return TRUE;
}


int GetRetMum(int nTplId)
{
    char szTplFile[MAX_PATH] = {0};
    sprintf(szTplFile, "%s\\templates.monitor\\%d.tpl", FuncGetInstallPath(), nTplId);

    int nNum = 0;
    nNum = GetPrivateProfileInt("properties", "_num", 0, szTplFile);
    return nNum;
}

void GetRetType(int nTplId, char* szSession, char* szType)
{
    char szTplFile[MAX_PATH] = {0};
    sprintf(szTplFile, "%s\\templates.monitor\\%d.tpl", FuncGetInstallPath(), nTplId);

    GetPrivateProfileString(szSession, "_type", "", szType, 16, szTplFile);
}

void GetRetName(int nTplId, char* szSession, char* szName)
{
    char szTplFile[MAX_PATH] = {0};
    sprintf(szTplFile, "%s\\templates.monitor\\%d.tpl", FuncGetInstallPath(), nTplId);

    GetPrivateProfileString(szSession, "_name", "", szName, 128, szTplFile);
}

extern "C" __declspec(dllexport) 
BOOL OracleGeneral(const char * strParas, char * szReturn, int& nSize)
//(CStringList &paramList, char *szReturn)
{
    AFX_MANAGE_STATE(AfxGetStaticModuleState());

	CString		strDBConn = _T(""), 
				strQuery = _T("");
	CString     strLog = _T("");

	CString		strDBMatch = _T(""), 
				strDBUser = _T(""), 
				strDBPass = _T(""), 
				strDBDriver = _T(""), 
				strProcName= _T(""), 
				strQFile = _T("");
    int nTplId = 0;

	CStringList paramList;
	MakeStringListByChar(paramList,strParas);
	POSITION pos = paramList.FindIndex(0);
	while(pos)
	{
		CString strTemp = paramList.GetNext(pos);
		if(strTemp.Find(__ServiceName__) == 0)
		{
			strDBConn = strTemp.Right(strTemp.GetLength() - strlen(__ServiceName__));
		}

		else if(strTemp.Find(__USERACCOUNT__) == 0)
		{
			strDBUser = strTemp.Right(strTemp.GetLength() - strlen(__USERACCOUNT__));
		}
		else if(strTemp.Find(__PASSWORD__) == 0)
		{
			strDBPass = strTemp.Right(strTemp.GetLength() - strlen(__PASSWORD__));
		}
		else if(strTemp.Find(__SQL__) == 0) 
		{
			strQuery=strTemp.Right(strTemp.GetLength() - strlen(__SQL__));
		}
        else if(strTemp.Find(__TEMPLATEID__) == 0) 
		{
			nTplId=atoi(strTemp.Right(strTemp.GetLength() - strlen(__TEMPLATEID__)).GetBuffer(0));
		}
	}

	strLog.Format("_ServiceName=%s _UserAccount=%s _PassWord=%s", 
		strDBConn, strDBUser, strDBPass);
	WriteLog(strLog, 223);

	if(strDBConn.IsEmpty())
	{
		//IDS_ORACLEPERF_1
		sprintf(szReturn,"error=%s", FuncGetStringFromIDS("SV_DATABASE",
            "DATABASE_NAME_NULL"));
		return FALSE;
	}
	
	otl_connect db;
	CString strConn = _T("");

	otl_conn::initialize();
//	strConn.Format("%s/%s@%s", strDBUser, strDBPass, strDBConn);
	strConn.Format("UID=%s;PWD=%s;DSN=%s;QTO=F;APA=T;", strDBUser, strDBPass, strDBConn);
	try
	{
		db.rlogon(strConn.GetBuffer(1));		
		otl_stream i(200, // buffer size
              strQuery.GetBuffer(0),
                 // SELECT statement
              db // connect object
             );
		
        //从配置文件中取得返回值个数
        int nRets = GetRetMum(nTplId);

        //根据返回值个数，循环取得返回值类型、字段名称
        char    szSession[32] = {0};
        char    szType[16] = {0};
        char    szName[128] = {0};
        string  strReturn;
        char    szTmp[1024] = {0};
        double  lValue = 0;
        char    szValue[1024] = {0};

        for (int j=1; j<=nRets; j++)
        {
            sprintf(szSession, "property%d", j);
            GetRetType(nTplId, szSession, szType); 
            GetRetName(nTplId, szSession, szName);
            if (0 == stricmp("int", szType))
            {
                i>>lValue;
                sprintf(szTmp, "%.0f", lValue);
                strReturn += szName;
                strReturn += "=";
                strReturn += szTmp;
                strReturn += "$";
            }
            else if (0 == stricmp("float", szType))
            {
                i>>lValue;
                strReturn += szName;
                strReturn += "=";
                sprintf(szTmp, "%.2f", lValue);
                strReturn += szTmp;
                strReturn += "$";
            }
            else if (0 == stricmp("string", szType))
            {
                i>>szValue;
                strReturn += szName;
                strReturn += "=";
                sprintf(szTmp, "%s", szValue);
                strReturn += szTmp;
                strReturn += "$";
            }
            else
            {
            }
        }
        
		i.close();
        sprintf(szReturn, "%s", strReturn);
		CString strInput = strReturn.c_str();
		MakeCharByString(szReturn,nSize,strInput);
	}
	catch(otl_exception& p)
	{ // intercept OTL exceptions
//		cerr<<p.msg<<endl; // print out error message
//		cerr<<p.stm_text<<endl; // print out SQL that caused the error
//		cerr<<p.var_info<<endl; // print out the variable that caused the error
		printf("error=%s\r\n%s$", p.msg, p.var_info);
		strLog.Format("OracleGeneral ExceptionMsg=%s ExceptionInfo=%s", p.msg, p.var_info);
		WriteLog(strLog, 223);
		db.logoff(); // disconnect from ODBC
		CString sReturn;
		sReturn.Format("error=%s:%s$",p.msg,p.var_info);
		sReturn.Replace("\r\n",",");
		sprintf(szReturn,"%s",sReturn);
		return FALSE;
	}
	db.logoff(); // disconnect from ODBC
	
	return TRUE;
}

/****************************************************************************
	Export Function Of DatabaseSourceNames (Fetch DSN)
****************************************************************************/
extern "C" __declspec(dllexport) 
BOOL SYSTEMDSN(const char * strParas, char * strReturn, int& nSize)
{	
	AFX_MANAGE_STATE(AfxGetStaticModuleState());

	HKEY hKey = NULL;
	LONG lRet = NULL;	
	lRet = ::RegOpenKeyEx(HKEY_LOCAL_MACHINE, "SOFTWARE\\ODBC\\ODBC.INI\\ODBC Data Sources", 0, KEY_QUERY_VALUE , &hKey);
	char *p =strReturn;
	if (lRet == ERROR_SUCCESS) {
		// TODO: 
		DWORD dwIndex = 0;
		while (1) {
			char szValueName[512] = {0};
			DWORD dwSizeValue = sizeof(szValueName) - 1;
			
			char szVal[512] = {0};
			DWORD len = 512;

			//lRet = RegEnumValue(hKey, dwIndex, szValueName, &dwSizeValue, NULL, NULL, NULL, NULL);				
			lRet = RegEnumValue(hKey, dwIndex, szValueName, &dwSizeValue, NULL, NULL, (LPBYTE)szVal, &len);
			if (lRet != ERROR_SUCCESS)
				break;

			if(strstr(szVal, "Oracle") != NULL)
			{
			sprintf(p,"%s=%s",szValueName,szValueName);
			p+= 2* strlen(szValueName)+2;
			}
			//lstTexts.AddTail(szValueName);
			//lstValues.AddTail(szValueName);
			dwIndex++;
		}
		RegCloseKey(hKey);
	}

	return TRUE;
	
}

extern "C" __declspec(dllexport) 
BOOL ODBCTest(const char *inFileName,const char *outFileName)
{
	FILE *stream;
    char chTime[250] = {0};
    //打开文件

	strcpy(szoutFile,outFileName);

	stream=fopen(inFileName,"r");

//	WriteResultFile(inFileName,outFileName,0);

	CString strTemp,aa;
	CStringList paramlist,lsValue,lsText;
	while(fgets(chTime,249,stream)!=NULL)
	{
		strTemp=chTime;
		strTemp.Replace("\n","");
		aa+=strTemp;
		paramlist.AddHead(strTemp);
	//	WriteResultFile(chTime,outFileName,1);
	}
	
	char szReturn[2014]={0};
	//GetOracleInfo(paramlist,szReturn);
	//OracleTableName(paramlist,);
	
	WriteResultFile(FuncGetStringFromIDS("SV_ORACLE","SHOWTABLENAME"),szoutFile,1);
	POSITION pos;
	pos =lsText.GetHeadPosition();
	while(pos!=NULL)
	{
		strTemp=lsText.GetNext(pos);
		WriteResultFile(strTemp,outFileName,1);
		
	}

	WriteResultFile(szReturn,outFileName,1);
	
	return TRUE;
}

void WriteResultFile(const char* chMsg, const char* filename, int nFlag)
{
	
    FILE *stream;
    char chTime[50] = {0};
    //打开文件
    if (nFlag == 0)
        stream=fopen(filename,"w");
    else if(nFlag ==1)
        stream=fopen(filename,"a");
	fputs(chMsg,stream);
    fputs("\n",stream);
	fclose(stream);
	
}


BOOL DBrlogon( CString strConn, otl_connect* pdb,char * szReturn)
{
	
	int i =0;
	try
	{
//LOOP:
		i++;
		int nBegin = GetTickCount();
		pdb->rlogon(strConn.GetBuffer(1));		
		int nEnd = GetTickCount();
		sprintf(szReturn, "status=OK$time=%d", nEnd-nBegin);
		
	}
	catch(otl_exception& p)
	{ 
		/*
		if(i<2) {
			//Sleep(5000);
			//goto LOOP;
		}*/
		sprintf(szReturn, "error=%s\r\n%s$", p.msg, p.var_info);
		//strLog.Format("TestDBConnect ExceptionMsg=%s ExceptionInfo=%s", p.msg, p.var_info);
		pdb->logoff(); // disconnect from ODBC
		return FALSE;
	}
	return TRUE;

	

}