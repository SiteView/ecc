#include "stdafx.h"
#include <list>
#include <fstream>
//#include "../../kennel/svdb/svapi/svdbapi.h"
#include "svdbapi.h"

#include<stdio.h> 
#include<winsock2.h>
#pragma comment(lib,"ws2_32.lib")

#import "progid:WbemScripting.SWbemLocator" named_guids

#include <fstream>
using namespace std;
typedef struct std::list<char*> StringList;

int PrintLog(const char * strReceive)
{
	char datebuf[128]=_T("");
	char timebuf[128]=_T("");
	char tempbuf[1024*10]=_T("");

	_strdate(datebuf);
	_strtime(timebuf);
	sprintf(tempbuf,"%s-%s",datebuf,timebuf);
	ofstream filestream;
	filestream.open("WMIMonitor.log",ios::app);
	filestream<<tempbuf<<"\t"<<strReceive<<endl;
	filestream.close();
	filestream.close();
	return 0;
}

bool MakeStringListByChar(StringList& pList, const char * pInput )
{
	const char * p;
	int nSize;
	p=pInput;
	while(*p!='\0')
	{
		nSize =strlen(p);
		if( nSize>0 )
		{	
			//pList.AddHead(p);
			pList.push_back((char*)p);

		}
		p=p+nSize+1;
	}

	return true;
}

int GetCharLength(const char * pInput)
{
	const char * p;
	int nSize = 0;
	p=pInput;
	while(*p!='\0')
	{
		nSize += strlen(p) + 1;
		p += strlen(p)+1;
	}

	 return nSize;
}

bool MakeCharByString(char *pOut,int &nOutSize,CString strInput )
{
	 char *p;
	
	int nSize=strInput.GetLength();
	if(nSize+2 <nOutSize)
	{
		strcpy(pOut,strInput.GetBuffer(strInput.GetLength()));
	}else return false;
	p=pOut;
	//printf("%d\n",nSize);23028830 13602067678 王波
	for(int i=0;i<nSize;i++)
	{
		if(*p=='$') 	
			*p='\0';
		p++;
	}
	nOutSize=nSize+1;
	return true;
	
}

char *FindStrValue(const char *strParas, CString str)
{
	char *pValue = NULL;
	string m_TempStr;

	std::list<char*> strList;
	MakeStringListByChar(strList, strParas);
	std::list<char *>::iterator pos = strList.begin();

	 while(pos != strList.end())
	{
		//CString strTemp = strList.GetNext(pos);
		char * strTemp = *pos;
		std::string strTemp1 = *pos;
		int m_Fpos = 0;
		
		if((m_Fpos = strTemp1.find(str, 0)) >= 0)
		{
			m_TempStr = strTemp1.substr( m_Fpos + strlen(str)+1, strTemp1.size() - strlen(str) - 1); 
			pValue=(char*)malloc(m_TempStr.size()+1);
			strcpy(pValue, m_TempStr.c_str());
			
		}
		pos++;
	}

	return pValue;
	
}

int GetConfirms(CString strMonitorID)
{
	CString strFileName, strDate;
	strFileName.Format("..\\data\\TmpIniFile\\Confirms_%s.ini", strMonitorID);
	strDate = COleDateTime::GetCurrentTime().Format("%Y%m%d");
	int nRet = GetPrivateProfileInt(strDate, "Times", -1, strFileName);
	if(nRet == -1)
	{
		WritePrivateProfileString(strDate, "Times", "0", strFileName);
		return 0;
	}

	return nRet;
}

BOOL IsLocalHost(CString sHostName)
{
	sHostName.TrimRight();
	sHostName.MakeLower();
	if(sHostName == "127.0.0.1" || sHostName == "localhost")
		return TRUE;

	WSADATA wsaData;
	char name[155];
	char *ip;
	PHOSTENT hostinfo; 
	if ( WSAStartup( MAKEWORD(2,0), &wsaData ) == 0 ) 
	{ 
		if( gethostname ( name, sizeof(name)) == 0) 
		{ 
			if(sHostName == name)
				return TRUE;
			if((hostinfo = gethostbyname(name)) != NULL) 
			{ //这些就是获得IP的函数
				ip = inet_ntoa (*(struct in_addr *)*hostinfo->h_addr_list); 

				if(sHostName == ip)
					return TRUE;
			} 
		} 
		WSACleanup( );
	}
	return FALSE;
}

BOOL ConnectServer(const char * strParas, char * szReturn, int& nSize,WbemScripting::ISWbemServicesPtr &services)
{
	char *machinename=NULL, *user=NULL, *pswd=NULL;

	machinename = FindStrValue(strParas, "_MachineName");
	user = FindStrValue(strParas, "_UserAccount");
	pswd = FindStrValue(strParas, "_PassWord");

	//CoInitialize(NULL);

	try 
	{ 
		WbemScripting::ISWbemLocatorPtr locator; 
		locator.CreateInstance(WbemScripting::CLSID_SWbemLocator);
		
		if (locator != NULL) 
		{ 
			if(IsLocalHost(machinename))
				services = locator->ConnectServer(".","root\\cimv2","","","","",0,NULL);
			else
				services = locator->ConnectServer(machinename,"root\\cimv2",user,pswd,"","",0,NULL);
		}
	}
	catch (_com_error err) 
	{ 
		char buf[200] = {0};
		IErrorInfo * ei = err.ErrorInfo();
		BSTR strDesEI;
		ei->GetDescription(&strDesEI);
		sprintf(szReturn, "error=Error ocured:%x: %s", (unsigned)err.Error(),_com_util::ConvertBSTRToString(strDesEI));
		return FALSE;
	} 
	catch(...) 
	{ 
		sprintf(szReturn, "error=Error ocured: %d",::GetLastError());
		return FALSE;
	} 

	return TRUE;
}

BOOL ConnectADServer(const char * strParas, char * szReturn, int& nSize,WbemScripting::ISWbemServicesPtr &services)
{
	char *machinename=NULL, *user=NULL, *pswd=NULL;

	machinename = FindStrValue(strParas, "_MachineName");
	user = FindStrValue(strParas, "_UserAccount");
	pswd = FindStrValue(strParas, "_PassWord");

	//CoInitialize(NULL);

	try 
	{ 
		WbemScripting::ISWbemLocatorPtr locator; 
		locator.CreateInstance(WbemScripting::CLSID_SWbemLocator);
		
		if (locator != NULL) 
		{ 
			if(IsLocalHost(machinename))
				services = locator->ConnectServer(".","root\\MicrosoftActiveDirectory","","","","",0,NULL);
			else
				services = locator->ConnectServer(machinename,"root\\MicrosoftActiveDirectory",user,pswd,"","",0,NULL);
		}
	}
	catch (_com_error err) 
	{ 
		char buf[200] = {0};
		IErrorInfo * ei = err.ErrorInfo();
		BSTR strDesEI;
		ei->GetDescription(&strDesEI);
		sprintf(szReturn, "error=Error ocured:%x: %s", (unsigned)err.Error(),_com_util::ConvertBSTRToString(strDesEI));
		return FALSE;
	} 
	catch(...) 
	{ 
		sprintf(szReturn, "error=Error ocured: %d",::GetLastError());
		return FALSE;
	} 

	return TRUE;
}

extern "C" _declspec(dllexport) 
BOOL GetCPURate(const char * strParas, char * szReturn, int& nSize)
{
	CoInitialize(NULL);
	{
		map<int, float> lPerProTime1;	//保存第一次获取的PID及其占用CPU时间
		map<int, float>::iterator pPerProTime1 = lPerProTime1.begin();
		map<int, float> lPerProTime2;	//保存第二次获取的PID及其战胜CPU时间
		map<int, float>::iterator pPerProTime2 = lPerProTime2.begin();
		map<int,CString> mPerProTime;	//保存PID和进程名

		int iPIDNum1=0,iPIDNum2=0;		//PID号
		CString strProName =_T("");	//获取到的进程名列表
		int iProNum1,iProNum2;		//两次分别获取到的进程数目
		float iTotalRate=0;
		CString strResult = _T("");
		CString strReturn = "detailutilization=";
		CString strWQL = _T("");
		strWQL.Format("%s","SELECT * FROM Win32_PerfRawData_PerfProc_Process where Name<>'_Total'");
		WbemScripting::ISWbemServicesPtr services;
		if(!ConnectServer(strParas, szReturn, nSize, services))
			return FALSE;
		try
		{
			WbemScripting::ISWbemObjectSetPtr objects = services->ExecQuery(LPCTSTR(strWQL),"WQL",0x10,NULL);
			IEnumVARIANTPtr obj_enum = objects->Get_NewEnum(); 
			ULONG fetched; 
			VARIANT var; 
/////////////////////////////////////取第一次进程CPU占用率数据//////////////////////////////////////////
			int nThreadCount = 0;
			char strTest[1024]={0};
			float fPercentProcessorTime=0, fWorkingsetsize=0, fPrivateBytes=0, fPercentProcessorTime1=0, fPercentProcessorTime2=0;	
			while (obj_enum->Next(1,&var,&fetched) == S_OK) 
			{ 
				WbemScripting::ISWbemObjectPtr object = var;
				WbemScripting::ISWbemPropertySetPtr properties = object->Properties_;
				WbemScripting::ISWbemPropertyPtr prop = properties->Item("PercentProcessorTime",0);
				fPercentProcessorTime1 = (float)prop->GetValue()/100000;
				prop = properties->Item("IDProcess",0);									//第一次获取PID，用于与下一次获取的对比。
				iPIDNum1 = (int)prop->GetValue();
				lPerProTime1[iPIDNum1] = fPercentProcessorTime1;
			}
			Sleep(1000);
//////////////////////////////////////////取第二次进程CPU占用率数据/////////////////////////////////////
			objects = services->ExecQuery(LPCTSTR(strWQL),"WQL",0x10,NULL); 		
			obj_enum = objects->Get_NewEnum(); 
			int i=0;
			while (obj_enum->Next(1,&var,&fetched) == S_OK) 
			{ 
				WbemScripting::ISWbemObjectPtr object = var;
				WbemScripting::ISWbemPropertySetPtr properties = object->Properties_;
				WbemScripting::ISWbemPropertyPtr prop = properties->Item("PercentProcessorTime",0);
				fPercentProcessorTime2 = (float)prop->GetValue()/100000;
				prop = properties->Item("IDProcess",0);									//第二次获取PID
				iPIDNum2 = prop->GetValue();
				prop = properties->Item("Name",0);										//获取进程名列表
				strProName = prop->GetValue();
				mPerProTime[iPIDNum2] = strProName;
				i++;
				lPerProTime2[iPIDNum2] = fPercentProcessorTime2;

			}
			iProNum1 = lPerProTime1.size();
			iProNum2 = lPerProTime2.size();
			pPerProTime1 = lPerProTime1.begin();
			pPerProTime2 = lPerProTime2.begin();
			CString cTemp;
			float fTempProTime=0.0;					//保存进程运行的时间差值。
			float fTotalTime=0.0;					//获取SLEEP的时候，CPU实际一共运行了多少时间。以这个作为除数替代原来SLEEP的一秒
			map<int, float>::iterator item = lPerProTime1.begin();
			map<int, float>::iterator item2 = lPerProTime2.begin();		
			map<int, CString>::iterator pmPerProTime = mPerProTime.begin();
			if(iProNum1>=iProNum2)
			{
				for(pPerProTime2 = lPerProTime2.begin();pPerProTime2!=lPerProTime2.end();pPerProTime2++)
				{
					fTotalTime+=fTempProTime;
					for(pPerProTime1 = item;pPerProTime1 !=lPerProTime1.end();)
					{
						if(pPerProTime1->first == pPerProTime2->first)
						{
							fTempProTime =pPerProTime2->second-pPerProTime1->second;					
							pPerProTime1++;
							item=pPerProTime1;
							break;
						}
						else
						{
							pPerProTime1++;
						}					
					}

				}
				item = lPerProTime1.begin();
				for(pPerProTime2 = lPerProTime2.begin(),pmPerProTime = mPerProTime.begin(); pPerProTime2 != lPerProTime2.end(); pPerProTime2++,pmPerProTime++)
				{					
					for(pPerProTime1 = item; pPerProTime1 != lPerProTime1.end();)
					{
						if(pPerProTime1->first == pmPerProTime->first)
						{							
							fTempProTime = pPerProTime2->second - pPerProTime1->second;
							if(fTempProTime!=0.0)
							{
								if(pPerProTime2->first!=0)
								{
									iTotalRate+=fTempProTime;	
									cTemp.Format("%s:%0.2f, ",pmPerProTime->second.GetBuffer(1),fTempProTime*100/fTotalTime);
									strResult+=cTemp;
								}
								

							}
							pPerProTime1++;
							item=pPerProTime1;
							break;
						}
						else
						{
							pPerProTime1++;
						}

					}
				}
			}			
			else
			{
				for(pPerProTime1 = lPerProTime1.begin();pPerProTime1!=lPerProTime2.end();pPerProTime1++)
				{
					fTotalTime+=fTempProTime;
					for(pPerProTime2 = item;pPerProTime2 !=lPerProTime2.end();)
					{
						if(pPerProTime1->first == pPerProTime2->first)
						{
							fTempProTime =pPerProTime2->second-pPerProTime1->second;					
							pPerProTime1++;
							item=pPerProTime1;
							break;
						}
						else
						{
							pPerProTime1++;
						}					
					}

				}
				item2 = lPerProTime2.begin();
				for(pPerProTime1 = lPerProTime1.begin(),pmPerProTime = mPerProTime.begin(); pPerProTime1 !=lPerProTime1.end(); pPerProTime1++,pmPerProTime++)
				{
					
					for(pPerProTime2 = item2; pPerProTime2 != lPerProTime2.end();)
					{
						if(pPerProTime2->first == pmPerProTime->first)
						{	
							fTempProTime = pPerProTime2->second - pPerProTime1->second;
							if(fTempProTime!=0.0)
							{
								if(pPerProTime2->first!=0)
								{
									iTotalRate+=fTempProTime;
									cTemp.Format("%s:%0.2f, ",pmPerProTime->second.GetBuffer(1),fTempProTime*100/fTotalTime);
									strResult+=cTemp;
								}
							}							
							pPerProTime2++;
							item2=pPerProTime2;
							break;
						}
						else
						{
							pPerProTime2++;
						}

					}
					
				}
			}
			
			strResult = "PerProUse="+strResult;
			strReturn=_T("");
			strReturn.Format("utilization=%0.2f$",iTotalRate*100/fTotalTime);
			strReturn+=strResult;
		}
		catch (_com_error err) 
		{ 
			char buf[200] = {0};
			IErrorInfo * ei = err.ErrorInfo();
			BSTR strDesEI;
			ei->GetDescription(&strDesEI);
			sprintf(szReturn, "error=Error ocured:%x: %s", (unsigned)err.Error(),_com_util::ConvertBSTRToString(strDesEI));
			return FALSE;
		} 
		catch(...) 
		{ 
			sprintf(szReturn, "error=Error ocured: %d",::GetLastError());
			OutputDebugString(szReturn);
			return FALSE;
		} 

		strcpy(szReturn,strReturn);
		CString strOutRet;
		strOutRet =szReturn;
		strOutRet.Delete(strOutRet.GetLength()-2,1);
		nSize = 4096;		
		MakeCharByString(szReturn,nSize,strOutRet);	 
	}
	CoUninitialize();
	return TRUE;
}

extern "C" _declspec(dllexport) 
BOOL GetAllCPURate(const char * strParas, char * szReturn, int& nSize)
{
	CoInitialize(NULL);
	{
		CString strReturn = "detailutilization=";
		WbemScripting::ISWbemServicesPtr services;
		if(!ConnectServer(strParas, szReturn, nSize, services))
			return FALSE;
		try
		{
			WbemScripting::ISWbemObjectSetPtr objects = services->InstancesOf("Win32_Processor", 0x10, NULL);
			IEnumVARIANTPtr obj_enum = objects->Get_NewEnum(); 
			ULONG fetched; 
			VARIANT var; 
			int nCpuRate = 0, nCount =0, nTotalRate = 0;
			CString strTemp;
			while (obj_enum->Next(1,&var,&fetched) == S_OK) 
			{ 
				WbemScripting::ISWbemObjectPtr object = var;
				WbemScripting::ISWbemPropertySetPtr properties = object->Properties_;
				WbemScripting::ISWbemPropertyPtr prop = properties->Item("LoadPercentage",0);
				_variant_t value = prop->GetValue();
				nCpuRate = (int)value;				
				prop = properties->Item("DeviceID",0);
							
				strTemp.Format("%s:%d,", (const char*)_bstr_t(prop->GetValue()), nCpuRate);
				strReturn += strTemp;
				nTotalRate += nCpuRate;
				nCount++;
			}
			strReturn.TrimRight(',');
			strTemp.Format("$utilization=%d$", nTotalRate/nCount);
			strReturn += strTemp;
		}
		catch (_com_error err) 
		{ 
			char buf[200] = {0};
			IErrorInfo * ei = err.ErrorInfo();
			BSTR strDesEI;
			ei->GetDescription(&strDesEI);
			sprintf(szReturn, "error=Error ocured:%x: %s", (unsigned)err.Error(),_com_util::ConvertBSTRToString(strDesEI));
			return FALSE;
		} 
		catch(...) 
		{ 
			sprintf(szReturn, "error=Error ocured: %d",::GetLastError());
			OutputDebugString(szReturn);
			return FALSE;
		} 

		strcpy(szReturn,strReturn);
		CString strOutRet;
		strOutRet =szReturn;
		nSize = 2048;
		MakeCharByString(szReturn,nSize,strOutRet);	 
	}
	CoUninitialize();

	return TRUE;
}

extern "C" _declspec(dllexport) 
BOOL EnumDisksInfo(const char * strParas, char * szReturn, int& nSize)
{
	CoInitialize(NULL);
	{
		CString strReturn = "";
		WbemScripting::ISWbemServicesPtr services;
		if(!ConnectServer(strParas, szReturn, nSize, services))
			return FALSE;

		try
		{
			_bstr_t com; 
			com="SELECT * FROM Win32_LogicalDisk where MediaType=12"; 
			WbemScripting::ISWbemObjectSetPtr objects = services->ExecQuery(com,"WQL",0x10,NULL); 			
			IEnumVARIANTPtr obj_enum = objects->Get_NewEnum(); 
			ULONG fetched; 
			VARIANT var; 
			CString strTemp;
			while (obj_enum->Next(1,&var,&fetched) == S_OK) 
			{ 
				WbemScripting::ISWbemObjectPtr object = var;
				WbemScripting::ISWbemPropertySetPtr properties = object->Properties_;
				WbemScripting::ISWbemPropertyPtr prop = properties->Item("DeviceID",0);
							
				strTemp.Format("%s=%s$", (const char*)_bstr_t(prop->GetValue()), (const char*)_bstr_t(prop->GetValue()));
				strReturn += strTemp;
			}
		}
		catch (_com_error err) 
		{ 
			char buf[200] = {0};
			IErrorInfo * ei = err.ErrorInfo();
			BSTR strDesEI;
			ei->GetDescription(&strDesEI);
			sprintf(szReturn, "error=Error ocured:%x: %s", (unsigned)err.Error(),_com_util::ConvertBSTRToString(strDesEI));
			return FALSE;
		} 
		catch(...) 
		{ 
			sprintf(szReturn, "error=Error ocured: %d",::GetLastError());
			OutputDebugString(szReturn);
			return FALSE;
		} 

		strcpy(szReturn,strReturn);
		CString strOutRet;
		strOutRet =szReturn;
		nSize = 2048;
		MakeCharByString(szReturn,nSize,strOutRet);	 
	}
	CoUninitialize();

	return TRUE;
}

extern "C" _declspec(dllexport) 
BOOL GetDiskInfo(const char * strParas, char * szReturn, int& nSize)
{
	CoInitialize(NULL);
	{
		CString strReturn = "";
		WbemScripting::ISWbemServicesPtr services;
		if(!ConnectServer(strParas, szReturn, nSize, services))
			return FALSE;
		try
		{
			char *disk=NULL;
			disk = FindStrValue(strParas, "_Disk");
			CString strCom;
			strCom.Format("SELECT * FROM Win32_LogicalDisk where MediaType=12 and DeviceID='%s'", disk);
			WbemScripting::ISWbemObjectSetPtr objects = services->ExecQuery(LPCTSTR(strCom),"WQL",0x10,NULL); 
			IEnumVARIANTPtr obj_enum = objects->Get_NewEnum(); 
			ULONG fetched; 
			VARIANT var; 
			float nSize = 0, nFreeSize =0;
			while (obj_enum->Next(1,&var,&fetched) == S_OK) 
			{ 
				WbemScripting::ISWbemObjectPtr object = var;
				WbemScripting::ISWbemPropertySetPtr properties = object->Properties_;
				WbemScripting::ISWbemPropertyPtr prop = properties->Item("Size",0);
				_variant_t value = prop->GetValue();
				nSize = (float)value;				
				prop = properties->Item("FreeSpace",0);
				value = prop->GetValue();
				nFreeSize = (float)value;	
				strReturn.Format("percentFull=%.2f$Mbfree=%.2f$TotalSize=%.2f$",
					((nSize - nFreeSize) / nSize) * 100 , nFreeSize/1048576, nSize/1048576);
			}
		}
		catch (_com_error err) 
		{ 
			char buf[200] = {0};
			IErrorInfo * ei = err.ErrorInfo();
			BSTR strDesEI;
			ei->GetDescription(&strDesEI);
			sprintf(szReturn, "error=Error ocured:%x: %s", (unsigned)err.Error(),_com_util::ConvertBSTRToString(strDesEI));
			return FALSE;
		} 
		catch(...) 
		{ 
			sprintf(szReturn, "error=Error ocured: %d",::GetLastError());
			OutputDebugString(szReturn);
			return FALSE;
		} 

		strcpy(szReturn,strReturn);
		CString strOutRet;
		strOutRet =szReturn;
		nSize = 2048;
		MakeCharByString(szReturn,nSize,strOutRet);	 
	}
	CoUninitialize();

	return TRUE;
}

extern "C" _declspec(dllexport) 
BOOL GetMemoryInfo(const char * strParas, char * szReturn, int& nSize)
{
	CoInitialize(NULL);
	{
		char strTMP[1024]=_T("");
		CString strReturn = "";
		WbemScripting::ISWbemServicesPtr services;
		if(!ConnectServer(strParas, szReturn, nSize, services))
			return FALSE;
		try
		{
			WbemScripting::ISWbemObjectSetPtr objects = services->InstancesOf("CIM_OperatingSystem", 0x10, NULL);
			IEnumVARIANTPtr obj_enum = objects->Get_NewEnum(); 
			ULONG fetched; 
			VARIANT var; 
			double fTotalPhyMem=0, fFreePhyMem=0, fTotalVirMem =0, fFreeVirMem=0;
			while (obj_enum->Next(1,&var,&fetched) == S_OK) 
			{ 
				WbemScripting::ISWbemObjectPtr object = var;
				WbemScripting::ISWbemPropertySetPtr properties = object->Properties_;
				WbemScripting::ISWbemPropertyPtr prop = properties->Item("TotalVisibleMemorySize",0);
				fTotalPhyMem = ((double)prop->GetValue())/1024;
				prop = properties->Item("TotalVirtualMemorySize",0);
				fTotalVirMem = ((double)prop->GetValue())/1024;
				prop = properties->Item("FreePhysicalMemory",0);
				fFreePhyMem = ((double)prop->GetValue())/1024;
				prop = properties->Item("FreeVirtualMemory",0);
				fFreeVirMem = ((double)prop->GetValue())/1024;
				strReturn.Format("TotalPhyMem=%.2f$TotalMemory=%.2f$FreePhyMem=%.2f$Mbfree=%.2f$PhyMemUsage=%.2f$percentUsed=%.2f$", 
					fTotalPhyMem, fTotalVirMem, fFreePhyMem, fFreeVirMem, (fTotalPhyMem-fFreePhyMem)/fTotalPhyMem*100, 
					(fTotalVirMem-fFreeVirMem)/fTotalVirMem*100);
			}
			//取第一次错误页面处理数据
			objects = services->InstancesOf("Win32_PerfRawData_PerfOS_Memory", 0x10, NULL);
			obj_enum = objects->Get_NewEnum(); 
			double fPagesPersec1=0, fTimestamp_PerfTime1=0, fPagesPersec2=0, fTimestamp_PerfTime2=0, fFrequency_PerfTime=0;
			while (obj_enum->Next(1,&var,&fetched) == S_OK) 
			{ 
				WbemScripting::ISWbemObjectPtr object = var;
				WbemScripting::ISWbemPropertySetPtr properties = object->Properties_;
				WbemScripting::ISWbemPropertyPtr prop = properties->Item("PagesPersec",0);
				fPagesPersec1 = (double)prop->GetValue();

				prop = properties->Item("Timestamp_PerfTime",0);
				fTimestamp_PerfTime1 = (double)prop->GetValue();
			}

			//取第二次错误页面处理数据
			objects = services->InstancesOf("Win32_PerfRawData_PerfOS_Memory", 0x10, NULL);
			obj_enum = objects->Get_NewEnum(); 
			while (obj_enum->Next(1,&var,&fetched) == S_OK) 
			{ 
				WbemScripting::ISWbemObjectPtr object = var;
				WbemScripting::ISWbemPropertySetPtr properties = object->Properties_;
				WbemScripting::ISWbemPropertyPtr prop = properties->Item("PagesPersec",0);
				fPagesPersec2 = (double)prop->GetValue();

				prop = properties->Item("Timestamp_PerfTime",0);
				fTimestamp_PerfTime2 = (double)prop->GetValue();

				prop = properties->Item("Frequency_PerfTime",0);
				fFrequency_PerfTime = (double)prop->GetValue();
			}
			double fPagesPersec = (fPagesPersec2-fPagesPersec1)/((fTimestamp_PerfTime2-fTimestamp_PerfTime1)/fFrequency_PerfTime);
			CString strTemp;
			strTemp.Format("pagesPerSec=%.2f$", fPagesPersec);
			strReturn += strTemp;
		}
		catch (_com_error err) 
		{ 
			char buf[200] = {0};
			IErrorInfo * ei = err.ErrorInfo();
			BSTR strDesEI;
			ei->GetDescription(&strDesEI);
			sprintf(szReturn, "error=Error ocured:%x: %s", (unsigned)err.Error(),_com_util::ConvertBSTRToString(strDesEI));
			return FALSE;
		} 
		catch(...) 
		{ 
			sprintf(szReturn, "error=Error ocured: %d",::GetLastError());
			OutputDebugString(szReturn);
			return FALSE;
		} 

		strcpy(szReturn,strReturn);
		CString strOutRet;
		strOutRet =szReturn;
		nSize = 1024*10;
		MakeCharByString(szReturn,nSize,strOutRet);	 
	}
	CoUninitialize();

	return TRUE;
}

extern "C" _declspec(dllexport) 
BOOL GetNTServices(const char * strParas, char * szReturn, int& nSize)
{
	CoInitialize(NULL);
	{
		CString strReturn = "";
		WbemScripting::ISWbemServicesPtr services;
		if(!ConnectServer(strParas, szReturn, nSize, services))
			return FALSE;
		try
		{
			WbemScripting::ISWbemObjectSetPtr objects = services->InstancesOf("Win32_Service", 0x10, NULL);
			IEnumVARIANTPtr obj_enum = objects->Get_NewEnum(); 
			ULONG fetched; 
			VARIANT var; 
			CString strTemp;
			while (obj_enum->Next(1,&var,&fetched) == S_OK) 
			{ 
				WbemScripting::ISWbemObjectPtr object = var;
				WbemScripting::ISWbemPropertySetPtr properties = object->Properties_;
				WbemScripting::ISWbemPropertyPtr prop = properties->Item("DisplayName",0);
				strTemp.Format("%s=%s$", (const char*)_bstr_t(prop->GetValue()), (const char*)_bstr_t(prop->GetValue()));
				strReturn += strTemp;
			}
		}
		catch (_com_error err) 
		{ 
			char buf[200] = {0};
			IErrorInfo * ei = err.ErrorInfo();
			BSTR strDesEI;
			ei->GetDescription(&strDesEI);
			sprintf(szReturn, "error=Error ocured:%x: %s", (unsigned)err.Error(),_com_util::ConvertBSTRToString(strDesEI));
			return FALSE;
		} 
		catch(...) 
		{ 
			sprintf(szReturn, "error=Error ocured: %d",::GetLastError());
			OutputDebugString(szReturn);
			return FALSE;
		} 

		strcpy(szReturn,strReturn);
		CString strOutRet;
		strOutRet =szReturn;
		nSize = 1048576;
		MakeCharByString(szReturn,nSize,strOutRet);	 
	}
	CoUninitialize();

	return TRUE;
}

extern "C" _declspec(dllexport) 
BOOL GetServiceInfo(const char * strParas, char * szReturn, int& nSize)
{
	CoInitialize(NULL);
	{
		CString strReturn = "";
		WbemScripting::ISWbemServicesPtr services;
		if(!ConnectServer(strParas, szReturn, nSize, services))
			return FALSE;
		try
		{
			char *servicename=NULL;
			servicename = FindStrValue(strParas, "_Service");
			CString strCom;
			strCom.Format("SELECT * FROM Win32_Service where DisplayName='%s'", servicename);
			WbemScripting::ISWbemObjectSetPtr objects = services->ExecQuery(LPCTSTR(strCom),"WQL",0x10,NULL); 
			IEnumVARIANTPtr obj_enum = objects->Get_NewEnum(); 
			ULONG fetched; 
			VARIANT var; 
			CString strTemp;
			int nProecssID = 0;
			while (obj_enum->Next(1,&var,&fetched) == S_OK) 
			{ 
				WbemScripting::ISWbemObjectPtr object = var;
				WbemScripting::ISWbemPropertySetPtr properties = object->Properties_;
				WbemScripting::ISWbemPropertyPtr prop = properties->Item("ProcessId",0);
				nProecssID = (int)prop->GetValue();
				if(nProecssID == 0)
					strReturn = "Processes=0$Started=False$ProcessName=NA$";
				else
				{
					strCom.Format("SELECT * FROM Win32_PerfRawData_PerfProc_Process where IDProcess=%d", nProecssID);
					objects = services->ExecQuery(LPCTSTR(strCom),"WQL",0x10,NULL); 
					obj_enum = objects->Get_NewEnum(); 
					while (obj_enum->Next(1,&var,&fetched) == S_OK) 
					{ 
						WbemScripting::ISWbemObjectPtr object = var;
						WbemScripting::ISWbemPropertySetPtr properties = object->Properties_;
						WbemScripting::ISWbemPropertyPtr prop = properties->Item("Name",0);
						strTemp.Format("ProcessName=%s$", (const char*)_bstr_t(prop->GetValue()));
						strReturn = "Processes=1$Started=True$";
						strReturn += strTemp;
					}
				}
				prop = properties->Item("State",0);
				strTemp.Format("State=%s$", (const char*)_bstr_t(prop->GetValue()));
				strReturn += strTemp;
				prop = properties->Item("Status",0);
				strTemp.Format("Status=%s$", (const char*)_bstr_t(prop->GetValue()));
				strReturn += strTemp;
			}
		}
		catch (_com_error err) 
		{ 
			char buf[200] = {0};
			IErrorInfo * ei = err.ErrorInfo();
			BSTR strDesEI;
			ei->GetDescription(&strDesEI);
			sprintf(szReturn, "error=Error ocured:%x: %s", (unsigned)err.Error(),_com_util::ConvertBSTRToString(strDesEI));
			return FALSE;
		} 
		catch(...) 
		{ 
			sprintf(szReturn, "error=Error ocured: %d",::GetLastError());
			OutputDebugString(szReturn);
			return FALSE;
		} 

		strcpy(szReturn,strReturn);
		CString strOutRet;
		strOutRet =szReturn;
		nSize = 2048;
		MakeCharByString(szReturn,nSize,strOutRet);	 
	}
	CoUninitialize();

	return TRUE;
}

BOOL GetDyn(std::string strMid , int& nState, std::string&  strDyn  )
{
	nState=0;
	strDyn ="";

	sv_dyn dyn;                
	if(GetSVDYN(strMid, dyn ))
    {
		nState= dyn.m_state;
		if(dyn.m_displaystr!=NULL)
			strDyn = dyn.m_displaystr;
     return TRUE;   
    }
	return FALSE;
}

extern "C" _declspec(dllexport) 
BOOL FilterEventLogInfo(const char * strParas, char * szReturn, int& nSize)
{
	CoInitialize(NULL);
	{
		CString strReturn = "";
		WbemScripting::ISWbemServicesPtr services;
		if(!ConnectServer(strParas, szReturn, nSize, services))
			return FALSE;
		try
		{
			CString strLogName="", strType="", strCodeList="", strSourceList="", strEventMachine="", strOnce="", strMonitorID="";
			strLogName = FindStrValue(strParas, "_logName");
			strLogName.TrimRight();
			strType = FindStrValue(strParas, "_eventType");
			strType.TrimRight();
			strCodeList = FindStrValue(strParas, "_codeList");
			strCodeList.TrimRight();
			strSourceList = FindStrValue(strParas, "_sourceList");
			strSourceList.TrimRight();
			strEventMachine = FindStrValue(strParas, "_eventMachine");
			strEventMachine.TrimRight();
			strMonitorID  = FindStrValue(strParas, "_MonitorID");
			strMonitorID.TrimRight();
			strOnce  = FindStrValue(strParas, "_monitorcondition");
			strOnce.TrimRight();
			
			CString strCom, strTemp;;
			strCom.Format("SELECT * FROM Win32_NTLogEvent where Logfile='%s'", strLogName);
			switch(atoi(strType))
			{
				case 2: 
					strCom += " and (Type='error' or Type='错误') ";
					break;
				case 3: 
					strCom += " and (Type='warning' or Type='警告') ";
					break;
				case 4: 
					strCom += " and (Type='warning' or Type='error' or Type='错误' or Type='警告') ";
					break;
				case 5: 
					strCom += " and (Type='information' or Type='信息') ";
					break;
			}

			CTime curTime = CTime::GetCurrentTime();
			//curTime -= 60*60*24; //one day ago
			strTemp.Format("and TimeWritten>='%s.000000+480'", curTime.Format("%Y%m%d000000"));
			strCom += strTemp;

			WbemScripting::ISWbemObjectSetPtr objects = services->ExecQuery(LPCTSTR(strCom),"WQL",0x10,NULL); 
			int nTotalEventCount = objects->Count;

			if(strCodeList != "")
			{
				strTemp.Format(" and EventCode<>'%s'", strCodeList);
				strTemp.Replace(";", "' and EventCode<>'");
				strCom += strTemp;
			}
			if(strSourceList != "")
			{
				strTemp.Format(" and SourceName<>'%s'", strSourceList);
				strTemp.Replace(";", "' and SourceName<>'");
				strCom += strTemp;
			}
			if(strEventMachine != "")
			{
				strTemp.Format(" and ComputerName='%s'", strEventMachine);
				strCom += strTemp;
			}

			objects = services->ExecQuery(LPCTSTR(strCom),"WQL",0x10,NULL); 
			int nFilterEventCount = objects->Count;

			int nConirms = GetConfirms(strMonitorID);
			if(nFilterEventCount < nConirms)
				nFilterEventCount = 0;
			else
				nFilterEventCount -= nConirms;

			strReturn.Format("checkcount=%d$filtercount=%d$", nTotalEventCount, nFilterEventCount);

			//int nState;  //  for new logic   stop monitor as soon as error state
			//std::string strDyn;
			//if(atoi(strOnce) != 3)
			//	GetDyn(LPCTSTR(strMonitorID),nState,strDyn);

		}
		catch (_com_error err) 
		{ 
			char buf[200] = {0};
			IErrorInfo * ei = err.ErrorInfo();
			BSTR strDesEI;
			ei->GetDescription(&strDesEI);
			sprintf(szReturn, "error=Error ocured:%x: %s", (unsigned)err.Error(),_com_util::ConvertBSTRToString(strDesEI));
			return FALSE;
		} 
		catch(...) 
		{ 
			sprintf(szReturn, "error=Error ocured: %d",::GetLastError());
			OutputDebugString(szReturn);
			return FALSE;
		} 

		strcpy(szReturn,strReturn);
		CString strOutRet;
		strOutRet =szReturn;
		nSize = 2048;
		MakeCharByString(szReturn,nSize,strOutRet);	 

		
	}
	CoUninitialize();

	return TRUE;
}

extern "C" _declspec(dllexport) 
BOOL EnumProcessInfo(const char * strParas, char * szReturn, int& nSize)
{
	CoInitialize(NULL);
	{
		CString strReturn = "";
		WbemScripting::ISWbemServicesPtr services;
		if(!ConnectServer(strParas, szReturn, nSize, services))
			return FALSE;

		try
		{
			_bstr_t com; 
			com="SELECT * FROM Win32_PerfRawData_PerfProc_Process where Name<>'_Total'"; 
			WbemScripting::ISWbemObjectSetPtr objects = services->ExecQuery(com,"WQL",0x10,NULL); 		
			IEnumVARIANTPtr obj_enum = objects->Get_NewEnum(); 
			ULONG fetched; 
			VARIANT var; 
			CString strTemp;
			while (obj_enum->Next(1,&var,&fetched) == S_OK) 
			{ 
				WbemScripting::ISWbemObjectPtr object = var;
				WbemScripting::ISWbemPropertySetPtr properties = object->Properties_;
				WbemScripting::ISWbemPropertyPtr prop = properties->Item("Name",0);
				_variant_t value = prop->GetValue();
				
				strTemp.Format("%s=%s$", (const char*)_bstr_t(value), (const char*)_bstr_t(value));
				strReturn += strTemp;
			}
		}
		catch (_com_error err) 
		{ 
			char buf[200] = {0};
			IErrorInfo * ei = err.ErrorInfo();
			BSTR strDesEI;
			ei->GetDescription(&strDesEI);
			sprintf(szReturn, "error=Error ocured:%x: %s", (unsigned)err.Error(),_com_util::ConvertBSTRToString(strDesEI));
			return FALSE;
		} 
		catch(...) 
		{ 
			sprintf(szReturn, "error=Error ocured: %d",::GetLastError());
			OutputDebugString(szReturn);
			return FALSE;
		} 

		strcpy(szReturn,strReturn);
		CString strOutRet;
		strOutRet =szReturn;
		nSize = 1048576;
		MakeCharByString(szReturn,nSize,strOutRet);	 
	}
	CoUninitialize();

	return TRUE;
}

CString ParseProcessID(CString strInput)
{
	int nFirst = strInput.Find('(');
	if (nFirst == -1)
		return "";

	int nLast = strInput.Find(')');
	if (nLast == -1)
		return "";

	return strInput.Mid(nFirst+1, nLast-nFirst-1);
}

extern "C" _declspec(dllexport) 
BOOL GetProcessInfo(const char * strParas, char * szReturn, int& nSize)
{
	CoInitialize(NULL);
	{
		CString strReturn = "";
		WbemScripting::ISWbemServicesPtr services;
		if(!ConnectServer(strParas, szReturn, nSize, services))
			return FALSE;

		try
		{
			CString strProcessName="";
			strProcessName = FindStrValue(strParas, "_monitorProcessList");
			
			//取第一次进程CPU占用率数据
			CString strCom;
			strCom.Format("SELECT * FROM Win32_PerfRawData_PerfProc_Process where Name='%s'", strProcessName); 
			WbemScripting::ISWbemObjectSetPtr objects = services->ExecQuery(LPCTSTR(strCom),"WQL",0x10,NULL); 		
			IEnumVARIANTPtr obj_enum = objects->Get_NewEnum(); 
			ULONG fetched; 
			VARIANT var; 
			CString strTemp;
			int nThreadCount = 0;
			float fPercentProcessorTime=0, fWorkingsetsize=0, fPrivateBytes=0, fPercentProcessorTime1=0, fPercentProcessorTime2=0;
			while (obj_enum->Next(1,&var,&fetched) == S_OK) 
			{ 
				WbemScripting::ISWbemObjectPtr object = var;
				WbemScripting::ISWbemPropertySetPtr properties = object->Properties_;
				WbemScripting::ISWbemPropertyPtr prop = properties->Item("PercentProcessorTime",0);
				fPercentProcessorTime1 += (float)prop->GetValue()/100000;
			}
			if(objects->Count == 0)
			{
				sprintf(szReturn, "error=The Process %s is not available!", strProcessName);
				return FALSE;
			}

			Sleep(1000);
			//取第二次进程CPU占用率数据
			objects = services->ExecQuery(LPCTSTR(strCom),"WQL",0x10,NULL); 		
			obj_enum = objects->Get_NewEnum(); 
			while (obj_enum->Next(1,&var,&fetched) == S_OK) 
			{ 
				WbemScripting::ISWbemObjectPtr object = var;
				WbemScripting::ISWbemPropertySetPtr properties = object->Properties_;
				WbemScripting::ISWbemPropertyPtr prop = properties->Item("ThreadCount",0);
				nThreadCount += (int)prop->GetValue(); 
				prop = properties->Item("PercentProcessorTime",0);
				fPercentProcessorTime2 += (float)prop->GetValue()/100000;
				prop = properties->Item("WorkingSet",0);
				fWorkingsetsize += (float)prop->GetValue()/1024;
				prop = properties->Item("PrivateBytes",0);
				fPrivateBytes += (float)prop->GetValue()/1024;
			}

			fPercentProcessorTime = fPercentProcessorTime2 - fPercentProcessorTime1;
			if(fPercentProcessorTime < 0)
				fPercentProcessorTime = 0;
			if(fPercentProcessorTime > 100)
				fPercentProcessorTime = 100;
			
			float fTotalPhyMem=0, fTotalVirMem=0;
			objects = services->InstancesOf("CIM_OperatingSystem", 0x10, NULL);
			obj_enum = objects->Get_NewEnum(); 
			while (obj_enum->Next(1,&var,&fetched) == S_OK) 
			{ 
				WbemScripting::ISWbemObjectPtr object = var;
				WbemScripting::ISWbemPropertySetPtr properties = object->Properties_;
				WbemScripting::ISWbemPropertyPtr prop = properties->Item("TotalVisibleMemorySize",0);
				fTotalPhyMem = (float)prop->GetValue();
				prop = properties->Item("TotalVirtualMemorySize",0);
				fTotalVirMem = (float)prop->GetValue();
			}
			strReturn.Format("ProcessCount=%d$ThreadCount=%d$PercentProcessorTime=%.2f$WorkingSet=%.2f$\
							MemUtilization=%.2f$PrivateBytes=%.2f$VirUtilization=%.2f$",
				objects->Count, nThreadCount, fPercentProcessorTime, fWorkingsetsize, fWorkingsetsize/fTotalPhyMem*100,
				fPrivateBytes, fPrivateBytes/fTotalVirMem*100);
			strReturn.Replace("\t", NULL);
		}
		catch (_com_error err) 
		{ 
			char buf[200] = {0};
			IErrorInfo * ei = err.ErrorInfo();
			BSTR strDesEI;
			ei->GetDescription(&strDesEI);
			sprintf(szReturn, "error=Error ocured:%x: %s", (unsigned)err.Error(),_com_util::ConvertBSTRToString(strDesEI));
			return FALSE;
		} 
		catch(...) 
		{ 
			sprintf(szReturn, "error=Error ocured: %d",::GetLastError());
			OutputDebugString(szReturn);
			return FALSE;
		} 

		strcpy(szReturn,strReturn);
		CString strOutRet;
		strOutRet =szReturn;
		nSize = 1048576;
		MakeCharByString(szReturn,nSize,strOutRet);	 
	}
	CoUninitialize();

	return TRUE;
}

extern "C" _declspec(dllexport) 
BOOL GetIISInfo(const char * strParas, char * szReturn, int& nSize)
{
		CoInitialize(NULL);
	{
		CString strReturn = "";
		WbemScripting::ISWbemServicesPtr services;
		if(!ConnectServer(strParas, szReturn, nSize, services))
			return FALSE;
		try
		{
			//取第一次IIS服务器数据
			_bstr_t com; 
			com="SELECT * FROM Win32_PerfRawData_W3SVC_WebService where Name='_Total'"; 
			WbemScripting::ISWbemObjectSetPtr objects = services->ExecQuery(com,"WQL",0x10,NULL); 		
			IEnumVARIANTPtr obj_enum = objects->Get_NewEnum(); 
			ULONG fetched; 
			VARIANT var; 
			int nMaximumConnections = 0,nCurrentAnonymousUsers = 0,nCurrentConnections = 0, nTotalNotFoundErrors = 0;
			float fPostRequestsPersec1 = 0, fGetRequestsPersec1 = 0, fBytesSentPerSec1 = 0, fBytesReceivedPerSec1 = 0,
				fBytesTotalPersec1 = 0, fTimestamp_PerfTime1 = 0, fFrequency_PerfTime = 0;
			while (obj_enum->Next(1,&var,&fetched) == S_OK) 
			{ 
				WbemScripting::ISWbemObjectPtr object = var;
				WbemScripting::ISWbemPropertySetPtr properties = object->Properties_;
				WbemScripting::ISWbemPropertyPtr prop = properties->Item("PostRequestsPersec",0);
				fPostRequestsPersec1 = (float)prop->GetValue();
				prop = properties->Item("GetRequestsPersec",0);
				fGetRequestsPersec1 = (float)prop->GetValue();
				prop = properties->Item("BytesSentPerSec",0);
				fBytesSentPerSec1 = (float)prop->GetValue();
				prop = properties->Item("BytesReceivedPerSec",0);
				fBytesReceivedPerSec1 = (float)prop->GetValue();
				prop = properties->Item("BytesTotalPersec",0);
				fBytesTotalPersec1 = (float)prop->GetValue();
				prop = properties->Item("Timestamp_PerfTime",0);
				fTimestamp_PerfTime1 = (float)prop->GetValue();
			}

			//取第二次IIS服务器数据
			com="SELECT * FROM Win32_PerfRawData_W3SVC_WebService where Name='_Total'"; 
			objects = services->ExecQuery(com,"WQL",0x10,NULL); 
			obj_enum = objects->Get_NewEnum(); 
			float fPostRequestsPersec2 = 0, fGetRequestsPersec2 = 0, fBytesSentPerSec2 = 0, fBytesReceivedPerSec2 = 0,
				fBytesTotalPersec2 = 0, fTimestamp_PerfTime2 = 0;
			while (obj_enum->Next(1,&var,&fetched) == S_OK) 
			{ 
				WbemScripting::ISWbemObjectPtr object = var;
				WbemScripting::ISWbemPropertySetPtr properties = object->Properties_;
				WbemScripting::ISWbemPropertyPtr prop = properties->Item("PostRequestsPersec",0);
				fPostRequestsPersec2 = (float)prop->GetValue();
				prop = properties->Item("GetRequestsPersec",0);
				fGetRequestsPersec2 = (float)prop->GetValue();
				prop = properties->Item("BytesSentPerSec",0);
				fBytesSentPerSec2 = (float)prop->GetValue();
				prop = properties->Item("BytesReceivedPerSec",0);
				fBytesReceivedPerSec2 = (float)prop->GetValue();
				prop = properties->Item("BytesTotalPersec",0);
				fBytesTotalPersec2 = (float)prop->GetValue();
				prop = properties->Item("Timestamp_PerfTime",0);
				fTimestamp_PerfTime2 = (float)prop->GetValue();
				prop = properties->Item("Frequency_PerfTime",0);
				fFrequency_PerfTime = (float)prop->GetValue();
				prop = properties->Item("MaximumConnections",0);
				nMaximumConnections = (int)prop->GetValue();
				prop = properties->Item("CurrentAnonymousUsers",0);
				nCurrentAnonymousUsers = (int)prop->GetValue();
				prop = properties->Item("CurrentConnections",0);
				nCurrentConnections = (int)prop->GetValue();
				prop = properties->Item("TotalNotFoundErrors",0);
				nTotalNotFoundErrors = (int)prop->GetValue();
			}
			float fInterval = (fTimestamp_PerfTime2-fTimestamp_PerfTime1)/fFrequency_PerfTime;
			strReturn.Format("maxconnections=%d$currentnonanonymoususers=%d$currentconnections=%d$totalnotfounderrors=%d$\
				postrequestspersec=%.2f$getrequestspersec=%.2f$bytessentpersec=%.2f$bytesreceivedpersec=%.2f$\
				bytestotalpersec=%.2f$", nMaximumConnections, nCurrentAnonymousUsers, nCurrentConnections, nTotalNotFoundErrors,
				(fPostRequestsPersec2-fPostRequestsPersec1)/fInterval, (fGetRequestsPersec2-fGetRequestsPersec1)/fInterval,
				(fBytesSentPerSec2-fBytesSentPerSec1)/fInterval, (fBytesReceivedPerSec2-fBytesReceivedPerSec1)/fInterval,
				(fBytesTotalPersec2-fBytesTotalPersec1)/fInterval);
			strReturn.Replace("\t", NULL);
		}
		catch (_com_error err) 
		{ 
			char buf[200] = {0};
			IErrorInfo * ei = err.ErrorInfo();
			BSTR strDesEI;
			ei->GetDescription(&strDesEI);
			sprintf(szReturn, "error=Error ocured:%x: %s", (unsigned)err.Error(),_com_util::ConvertBSTRToString(strDesEI));
			return FALSE;
		} 
		catch(...) 
		{ 
			sprintf(szReturn, "error=Error ocured: %d",::GetLastError());
			OutputDebugString(szReturn);
			return FALSE;
		} 

		strcpy(szReturn,strReturn);
		CString strOutRet;
		strOutRet =szReturn;
		nSize = 2048;
		MakeCharByString(szReturn,nSize,strOutRet);	 
	}
	CoUninitialize();

	return TRUE;
}

extern "C" _declspec(dllexport) 
BOOL GetASPInfo(const char * strParas, char * szReturn, int& nSize)
{
		CoInitialize(NULL);
	{
		CString strReturn = "";
		WbemScripting::ISWbemServicesPtr services;
		if(!ConnectServer(strParas, szReturn, nSize, services))
			return FALSE;
		try
		{
			//取第一次ASP数据
			WbemScripting::ISWbemObjectSetPtr objects = services->InstancesOf("Win32_PerfRawData_ASP_ActiveServerPages", 0x10, NULL);
			IEnumVARIANTPtr obj_enum = objects->Get_NewEnum(); 
			ULONG fetched; 
			VARIANT var; 
			int nErrorsDuringScriptRuntime = 0,nErrorsFromASPPreprocessor = 0,nErrorsFromScriptCompilers = 0,
				nSessionsCurrent = 0, nRequestsSucceeded = 0, nTransactionsTotal = 0, nRequestsFailedTotal = 0, 
				nRequestsTotal = 0;
			float fErrorsPerSec1 = 0, fRequestBytesInTotal1 = 0,  fTimestamp_PerfTime1 = 0,
				fFrequency_PerfTime = 0;
			while (obj_enum->Next(1,&var,&fetched) == S_OK) 
			{ 
				WbemScripting::ISWbemObjectPtr object = var;
				WbemScripting::ISWbemPropertySetPtr properties = object->Properties_;
				WbemScripting::ISWbemPropertyPtr prop = properties->Item("ErrorsPerSec",0);
				fErrorsPerSec1 = (float)prop->GetValue();
				prop = properties->Item("RequestBytesInTotal",0);
				fRequestBytesInTotal1 = (float)prop->GetValue();
				prop = properties->Item("Timestamp_PerfTime",0);
				fTimestamp_PerfTime1 = (float)prop->GetValue();
			}

			//取第二次ASP数据
			objects = services->InstancesOf("Win32_PerfRawData_ASP_ActiveServerPages", 0x10, NULL);
			obj_enum = objects->Get_NewEnum(); 
			float fErrorsPerSec2 = 0, fRequestBytesInTotal2 = 0, fRequestsFailedTotal2 = 0, fRequestsTotal2 = 0, fTimestamp_PerfTime2 = 0;
			while (obj_enum->Next(1,&var,&fetched) == S_OK) 
			{ 
				WbemScripting::ISWbemObjectPtr object = var;
				WbemScripting::ISWbemPropertySetPtr properties = object->Properties_;
				WbemScripting::ISWbemPropertyPtr prop = properties->Item("ErrorsPerSec",0);
				fErrorsPerSec2 = (float)prop->GetValue();
				prop = properties->Item("RequestBytesInTotal",0);
				fRequestBytesInTotal2 = (float)prop->GetValue();
				prop = properties->Item("RequestsFailedTotal",0);
				nRequestsFailedTotal = (int)prop->GetValue();
				prop = properties->Item("RequestsTotal",0);
				nRequestsTotal = (int)prop->GetValue();
				prop = properties->Item("SessionsCurrent",0);
				nSessionsCurrent = (int)prop->GetValue();
				prop = properties->Item("RequestsSucceeded",0);
				nRequestsSucceeded = (int)prop->GetValue();
				prop = properties->Item("TransactionsTotal",0);
				nTransactionsTotal = (int)prop->GetValue();
				prop = properties->Item("Timestamp_PerfTime",0);
				fTimestamp_PerfTime2 = (float)prop->GetValue();
				prop = properties->Item("Frequency_PerfTime",0);
				fFrequency_PerfTime = (float)prop->GetValue();
				prop = properties->Item("ErrorsDuringScriptRuntime",0);
				nErrorsDuringScriptRuntime = (int)prop->GetValue();
				prop = properties->Item("ErrorsFromASPPreprocessor",0);
				nErrorsFromASPPreprocessor = (int)prop->GetValue();
				prop = properties->Item("ErrorsFromScriptCompilers",0);
				nErrorsFromScriptCompilers = (int)prop->GetValue();
			}
			float fInterval = (fTimestamp_PerfTime2-fTimestamp_PerfTime1)/fFrequency_PerfTime;
			strReturn.Format("ErrorsDuringScriptRuntime=%d$ErrorsFromASPPreprocessor=%d$ErrorsFromScriptCompilers=%d$\
				ErrorsPerSec=%.2f$RequestBytesInTotal=%.2f$RequestsFailedTotal=%d$RequestsTotal=%d$\
				SessionsCurrent=%d$RequestsSucceeded=%d$TransactionsTotal=%d$", 
				nErrorsDuringScriptRuntime, nErrorsFromASPPreprocessor, nErrorsFromScriptCompilers,
				(fErrorsPerSec2-fErrorsPerSec1)/fInterval, (fRequestBytesInTotal2-fRequestBytesInTotal1)/fInterval,
				nRequestsFailedTotal, nRequestsTotal,nSessionsCurrent, nRequestsSucceeded,nTransactionsTotal);
			strReturn.Replace("\t", NULL);
		}
		catch (_com_error err) 
		{ 
			char buf[200] = {0};
			IErrorInfo * ei = err.ErrorInfo();
			BSTR strDesEI;
			ei->GetDescription(&strDesEI);
			sprintf(szReturn, "error=Error ocured:%x: %s", (unsigned)err.Error(),_com_util::ConvertBSTRToString(strDesEI));
			return FALSE;
		} 
		catch(...) 
		{ 
			sprintf(szReturn, "error=Error ocured: %d",::GetLastError());
			OutputDebugString(szReturn);
			return FALSE;
		} 

		strcpy(szReturn,strReturn);
		CString strOutRet;
		strOutRet =szReturn;
		nSize = 2048;
		MakeCharByString(szReturn,nSize,strOutRet);	 
	}
	CoUninitialize();

	return TRUE;
}

extern "C" _declspec(dllexport) 
BOOL GetAspNetApplicationInfo(const char * strParas, char * szReturn, int& nSize)
{
		CoInitialize(NULL);
	{
		CString strReturn = "";
		WbemScripting::ISWbemServicesPtr services;
		if(!ConnectServer(strParas, szReturn, nSize, services))
			return FALSE;
		try
		{
			//取第一次AspNet数据
			_bstr_t com; 
			com="SELECT * FROM Win32_PerfRawData_ASPNET_ASPNETApplications where Name='__Total__'"; 
			WbemScripting::ISWbemObjectSetPtr objects = services->ExecQuery(com,"WQL",0x10,NULL); 	
			IEnumVARIANTPtr obj_enum = objects->Get_NewEnum(); 
			ULONG fetched; 
			VARIANT var; 
			int nCompilationsTotal = 0, nRequestsFailed = 0,nSessionsTimedOut = 0,
				nSessionsActive = 0, nRequestsSucceeded = 0;
			float fTransactionsPerSec1 = 0, fRequestsPerSec1 = 0,  fErrorsTotalPerSec1 = 0,
				fAnonymousRequestsPerSec1 = 0, fCacheAPITurnoverRate1 = 0, fTimestamp_PerfTime1 = 0,
				fFrequency_PerfTime = 0;
			while (obj_enum->Next(1,&var,&fetched) == S_OK) 
			{ 
				WbemScripting::ISWbemObjectPtr object = var;
				WbemScripting::ISWbemPropertySetPtr properties = object->Properties_;
				WbemScripting::ISWbemPropertyPtr prop = properties->Item("TransactionsPerSec",0);
				fTransactionsPerSec1 = (float)prop->GetValue();
				prop = properties->Item("RequestsPerSec",0);
				fRequestsPerSec1 = (float)prop->GetValue();
				prop = properties->Item("ErrorsTotalPerSec",0);
				fErrorsTotalPerSec1 = (float)prop->GetValue();
				prop = properties->Item("AnonymousRequestsPerSec",0);
				fAnonymousRequestsPerSec1 = (float)prop->GetValue();
				prop = properties->Item("CacheAPITurnoverRate",0);
				fCacheAPITurnoverRate1 = (float)prop->GetValue();
				prop = properties->Item("Timestamp_PerfTime",0);
				fTimestamp_PerfTime1 = (float)prop->GetValue();
			}

			//取第二次AspNet数据
			com="SELECT * FROM Win32_PerfRawData_ASPNET_ASPNETApplications where Name='__Total__'"; 
			objects = services->ExecQuery(com,"WQL",0x10,NULL); 
			obj_enum = objects->Get_NewEnum(); 
			float fTransactionsPerSec2 = 0, fRequestsPerSec2 = 0,  fErrorsTotalPerSec2 = 0,
				fAnonymousRequestsPerSec2 = 0, fCacheAPITurnoverRate2 = 0, fTimestamp_PerfTime2 = 0;
			while (obj_enum->Next(1,&var,&fetched) == S_OK) 
			{ 
				WbemScripting::ISWbemObjectPtr object = var;
				WbemScripting::ISWbemPropertySetPtr properties = object->Properties_;
				WbemScripting::ISWbemPropertyPtr prop = properties->Item("TransactionsPerSec",0);
				fTransactionsPerSec2 = (float)prop->GetValue();
				prop = properties->Item("RequestsPerSec",0);
				fRequestsPerSec2 = (float)prop->GetValue();
				prop = properties->Item("ErrorsTotalPerSec",0);
				fErrorsTotalPerSec2 = (float)prop->GetValue();
				prop = properties->Item("AnonymousRequestsPerSec",0);
				fAnonymousRequestsPerSec2 = (float)prop->GetValue();
				prop = properties->Item("CacheAPITurnoverRate",0);
				fCacheAPITurnoverRate2 = (float)prop->GetValue();
				prop = properties->Item("Timestamp_PerfTime",0);
				fTimestamp_PerfTime2 = (float)prop->GetValue();
				prop = properties->Item("CompilationsTotal",0);
				nCompilationsTotal = (int)prop->GetValue();
				prop = properties->Item("RequestsFailed",0);
				nRequestsFailed = (int)prop->GetValue();
				prop = properties->Item("SessionsTimedOut",0);
				nSessionsTimedOut = (int)prop->GetValue();
				prop = properties->Item("SessionsActive",0);
				nSessionsActive = (int)prop->GetValue();
				prop = properties->Item("RequestsSucceeded",0);
				nRequestsSucceeded = (int)prop->GetValue();
				prop = properties->Item("Frequency_PerfTime",0);
				fFrequency_PerfTime = (float)prop->GetValue();
			}
			float fInterval = (fTimestamp_PerfTime2-fTimestamp_PerfTime1)/fFrequency_PerfTime;
			strReturn.Format("CompilationsTotal=%d$RequestsFailed=%d$SessionsTimedOut=%d$\
				SessionsActive=%d$RequestsSucceeded=%d$TransactionsPerSec=%.2f$RequestsPerSec=%.2f$\
				ErrorsTotalPerSec=%.2f$AnonymousRequestsPerSec=%.2f$CacheAPITurnoverRate=%.2f$", 
				nCompilationsTotal, nRequestsFailed, nSessionsTimedOut, nSessionsActive, nRequestsSucceeded,
				(fTransactionsPerSec2-fTransactionsPerSec1)/fInterval, (fRequestsPerSec2-fRequestsPerSec1)/fInterval,
				(fErrorsTotalPerSec2-fErrorsTotalPerSec1)/fInterval, (fAnonymousRequestsPerSec2-fAnonymousRequestsPerSec1)/fInterval,
				(fCacheAPITurnoverRate2-fCacheAPITurnoverRate1)/fInterval);
			strReturn.Replace("\t", NULL);
		}
		catch (_com_error err) 
		{ 
			char buf[200] = {0};
			IErrorInfo * ei = err.ErrorInfo();
			BSTR strDesEI;
			ei->GetDescription(&strDesEI);
			sprintf(szReturn, "error=Error ocured:%x: %s", (unsigned)err.Error(),_com_util::ConvertBSTRToString(strDesEI));
			return FALSE;
		} 
		catch(...) 
		{ 
			sprintf(szReturn, "error=Error ocured: %d",::GetLastError());
			OutputDebugString(szReturn);
			return FALSE;
		} 

		strcpy(szReturn,strReturn);
		CString strOutRet;
		strOutRet =szReturn;
		nSize = 2048;
		MakeCharByString(szReturn,nSize,strOutRet);	 
	}
	CoUninitialize();

	return TRUE;
}

extern "C" _declspec(dllexport) 
BOOL GetAspNetInfo(const char * strParas, char * szReturn, int& nSize)
{
		CoInitialize(NULL);
	{
		CString strReturn = "";
		WbemScripting::ISWbemServicesPtr services;
		if(!ConnectServer(strParas, szReturn, nSize, services))
			return FALSE;
		try
		{
			WbemScripting::ISWbemObjectSetPtr objects = services->InstancesOf("Win32_PerfRawData_ASPNET_ASPNET", 0x10, NULL);
			IEnumVARIANTPtr obj_enum = objects->Get_NewEnum(); 
			ULONG fetched; 
			VARIANT var; 
			while (obj_enum->Next(1,&var,&fetched) == S_OK) 
			{ 
				WbemScripting::ISWbemObjectPtr object = var;
				WbemScripting::ISWbemPropertySetPtr properties = object->Properties_;
				obj_enum = properties->Get_NewEnum(); 
				while (obj_enum->Next(1,&var,&fetched) == S_OK) 
				{
					WbemScripting::ISWbemPropertyPtr prot=var.punkVal; 
					_variant_t value = prot->GetValue();
					if(value.vt != VT_NULL)
					{
						strReturn += (const char*)_bstr_t(prot->GetName()) ;
						strReturn += "=";
						strReturn += (const char*)_bstr_t(value) ;
						strReturn += "$";
					}
				}
			}

		}
		catch (_com_error err) 
		{ 
			char buf[200] = {0};
			IErrorInfo * ei = err.ErrorInfo();
			BSTR strDesEI;
			ei->GetDescription(&strDesEI);
			sprintf(szReturn, "error=Error ocured:%x: %s", (unsigned)err.Error(),_com_util::ConvertBSTRToString(strDesEI));
			return FALSE;
		} 
		catch(...) 
		{ 
			sprintf(szReturn, "error=Error ocured: %d",::GetLastError());
			OutputDebugString(szReturn);
			return FALSE;
		} 

		strcpy(szReturn,strReturn);
		CString strOutRet;
		strOutRet =szReturn;
		nSize = 4096;
		MakeCharByString(szReturn,nSize,strOutRet);	 
	}
	CoUninitialize();

	return TRUE;
}

extern "C" _declspec(dllexport) 
BOOL EnumNetWorks(const char * strParas, char * szReturn, int& nSize)
{
	CoInitialize(NULL);
	{
		CString strReturn = "";
		WbemScripting::ISWbemServicesPtr services;
		if(!ConnectServer(strParas, szReturn, nSize, services))
			return FALSE;

		try
		{
			WbemScripting::ISWbemObjectSetPtr objects = services->InstancesOf("Win32_PerfRawData_Tcpip_NetworkInterface", 0x10, NULL);	
			IEnumVARIANTPtr obj_enum = objects->Get_NewEnum(); 
			ULONG fetched; 
			VARIANT var; 
			CString strTemp;
			while (obj_enum->Next(1,&var,&fetched) == S_OK) 
			{ 
				WbemScripting::ISWbemObjectPtr object = var;
				WbemScripting::ISWbemPropertySetPtr properties = object->Properties_;
				WbemScripting::ISWbemPropertyPtr prop = properties->Item("Name",0);
							
				strTemp.Format("%s=%s$", (const char*)_bstr_t(prop->GetValue()), (const char*)_bstr_t(prop->GetValue()));
				strReturn += strTemp;
			}
		}
		catch (_com_error err) 
		{ 
			char buf[200] = {0};
			IErrorInfo * ei = err.ErrorInfo();
			BSTR strDesEI;
			ei->GetDescription(&strDesEI);
			sprintf(szReturn, "error=Error ocured:%x: %s", (unsigned)err.Error(),_com_util::ConvertBSTRToString(strDesEI));
			return FALSE;
		} 
		catch(...) 
		{ 
			sprintf(szReturn, "error=Error ocured: %d",::GetLastError());
			OutputDebugString(szReturn);
			return FALSE;
		} 

		strcpy(szReturn,strReturn);
		CString strOutRet;
		strOutRet =szReturn;
		nSize = 2048;
		MakeCharByString(szReturn,nSize,strOutRet);	 
	}
	CoUninitialize();

	return TRUE;
}

extern "C" _declspec(dllexport) 
BOOL GetNetWorkInfo(const char * strParas, char * szReturn, int& nSize)
{
	CoInitialize(NULL);
	{
		CString strReturn = "";
		WbemScripting::ISWbemServicesPtr services;
		if(!ConnectServer(strParas, szReturn, nSize, services))
			return FALSE;
		try
		{
			char *networkname=NULL;
			networkname = FindStrValue(strParas, "_NetWorkName");
			CString strCom;

			//取第一次网络设备数据
			strCom.Format("SELECT * FROM Win32_PerfRawData_Tcpip_NetworkInterface where Name='%s'", networkname);
			WbemScripting::ISWbemObjectSetPtr objects = services->ExecQuery(LPCTSTR(strCom),"WQL",0x10,NULL); 
			IEnumVARIANTPtr obj_enum = objects->Get_NewEnum(); 
			ULONG fetched; 
			VARIANT var; 
			int nPacketsOutboundErrors = 0, nPacketsReceivedErrors = 0; 
			float fBytesReceivedPerSec1 = 0, fBytesSentPerSec1 =0, fTimestamp_PerfTime1 = 0,
				fFrequency_PerfTime = 0;
			while (obj_enum->Next(1,&var,&fetched) == S_OK) 
			{ 
				WbemScripting::ISWbemObjectPtr object = var;
				WbemScripting::ISWbemPropertySetPtr properties = object->Properties_;
				WbemScripting::ISWbemPropertyPtr prop = properties->Item("BytesReceivedPerSec",0);
				fBytesReceivedPerSec1 = (float)prop->GetValue();				
				prop = properties->Item("BytesSentPerSec",0);
				fBytesSentPerSec1 = (float)prop->GetValue();
				prop = properties->Item("Timestamp_PerfTime",0);
				fTimestamp_PerfTime1 = (float)prop->GetValue();
			}

			//取第二次网络设备数据
			objects = services->ExecQuery(LPCTSTR(strCom),"WQL",0x10,NULL); 
			obj_enum = objects->Get_NewEnum(); 
			float fBytesReceivedPerSec2 = 0, fBytesSentPerSec2 =0, fTimestamp_PerfTime2 = 0;
			while (obj_enum->Next(1,&var,&fetched) == S_OK) 
			{ 
				WbemScripting::ISWbemObjectPtr object = var;
				WbemScripting::ISWbemPropertySetPtr properties = object->Properties_;
				WbemScripting::ISWbemPropertyPtr prop = properties->Item("BytesReceivedPerSec",0);
				fBytesReceivedPerSec2 = (float)prop->GetValue();				
				prop = properties->Item("BytesSentPerSec",0);
				fBytesSentPerSec2 = (float)prop->GetValue();	
				prop = properties->Item("Timestamp_PerfTime",0);
				fTimestamp_PerfTime2 = (float)prop->GetValue();
				prop = properties->Item("PacketsOutboundErrors",0);
				nPacketsOutboundErrors = (float)prop->GetValue();
				prop = properties->Item("PacketsReceivedErrors",0);
				nPacketsReceivedErrors = (float)prop->GetValue();
				prop = properties->Item("Frequency_PerfTime",0);
				fFrequency_PerfTime = (float)prop->GetValue();

			}
			float fInterval = (fTimestamp_PerfTime2-fTimestamp_PerfTime1)/fFrequency_PerfTime;
			strReturn.Format("BytesReceivedPerSec=%.2f$BytesSentPerSec=%.2f$PacketsErrors=%d", 
				(fBytesReceivedPerSec2-fBytesReceivedPerSec1)/fInterval, (fBytesSentPerSec2-fBytesSentPerSec1)/fInterval,
				nPacketsOutboundErrors+nPacketsReceivedErrors);
		}
		catch (_com_error err) 
		{ 
			char buf[200] = {0};
			IErrorInfo * ei = err.ErrorInfo();
			BSTR strDesEI;
			ei->GetDescription(&strDesEI);
			sprintf(szReturn, "error=Error ocured:%x: %s", (unsigned)err.Error(),_com_util::ConvertBSTRToString(strDesEI));
			return FALSE;
		} 
		catch(...) 
		{ 
			sprintf(szReturn, "error=Error ocured: %d",::GetLastError());
			OutputDebugString(szReturn);
			return FALSE;
		} 

		strcpy(szReturn,strReturn);
		CString strOutRet;
		strOutRet =szReturn;
		nSize = 2048;
		MakeCharByString(szReturn,nSize,strOutRet);	 
	}
	CoUninitialize();

	return TRUE;
}

extern "C" _declspec(dllexport) 
BOOL GetADInfo(const char * strParas, char * szReturn, int& nSize)
{	
	CoInitialize(NULL);
	{
		CString strReturn = "";
		WbemScripting::ISWbemServicesPtr services;
		if(!ConnectADServer(strParas, szReturn, nSize, services))
			return FALSE;
		try
		{
			WbemScripting::ISWbemObjectSetPtr objects = services->InstancesOf("MSAD_DomainController", 0x10, NULL);
			IEnumVARIANTPtr obj_enum = objects->Get_NewEnum(); 
			ULONG fetched; 
			VARIANT var; 
			int nPecentUsage = 0;
			bool bState;
			while (obj_enum->Next(1,&var,&fetched) == S_OK) 
			{ 
				WbemScripting::ISWbemObjectPtr object = var;
				WbemScripting::ISWbemPropertySetPtr properties = object->Properties_;
				WbemScripting::ISWbemPropertyPtr prop = properties->Item("PercentOfRIDsLeft",0);
				nPecentUsage = 100-(int)prop->GetValue();
				prop = properties->Item("IsAdvertisingToLocator",0);
				bState = (bool)prop->GetValue();
				if(bState)
					strReturn.Format("PercentOfRIDsUsed=%d$ServiceState=True$", nPecentUsage);
				else
					strReturn.Format("PercentOfRIDsUsed=%d$ServiceState=False$", nPecentUsage);
			}

		}
		catch (_com_error err) 
		{ 
			char buf[200] = {0};
			IErrorInfo * ei = err.ErrorInfo();
			BSTR strDesEI;
			ei->GetDescription(&strDesEI);
			sprintf(szReturn, "error=Error ocured:%x: %s", (unsigned)err.Error(),_com_util::ConvertBSTRToString(strDesEI));
			return FALSE;
		} 
		catch(...) 
		{ 
			sprintf(szReturn, "error=Error ocured: %d",::GetLastError());
			OutputDebugString(szReturn);
			return FALSE;
		} 

		strcpy(szReturn,strReturn);
		CString strOutRet;
		strOutRet =szReturn;
		nSize = 4096;
		MakeCharByString(szReturn,nSize,strOutRet);	 
	}
	CoUninitialize();

	return TRUE;
}