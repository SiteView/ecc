#include "stdafx.h"
#include <list>
#include <stdio.h>
#include <string.h>           
#include <stdlib.h>          
#include <ctype.h>              

#include <cmqc.h>                          /* MQI                             */
#include <cmqcfc.h>                        /* PCF                             */
#include <cmqbc.h>                         /* MQAI                            */
#include <cmqxc.h>						   /* For MQCD definition             */

#include <fstream>
#include <stdlib.h>

using namespace std;
typedef struct std::list<char*> StringList;


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

/******************************************************************************/
/*                                                                            */
/* Function: CheckCallResult                                                  */ 
/*                                                                            */
/******************************************************************************/
/*                                                                            */
/* Input Parameters:  Description of call                                     */
/*                    Completion code                                         */
/*                    Reason code                                             */
/*                                                                            */
/* Output Parameters: CString                                                    */
/*                                                                            */
/* Logic: Display the description of the call, the completion code and the    */
/*        reason code if the completion code is not successful                */
/*                                                                            */
/******************************************************************************/
BOOL  CheckCallResult(char *callText, MQLONG cc, MQLONG rc, char * szReturn)
{
   CString strReturn="";
   if (cc != MQCC_OK)
   {
	   strReturn.Format("error=%s failed: Completion Code = %d : Reason = %d$", callText, cc, rc);
	   strcpy(szReturn,strReturn);
	   return FALSE;
   }
   return TRUE;

}

CString GetEnvironmentValue(CString strKeyName)
{
	char chKeyValue[256]="";
	GetEnvironmentVariable(strKeyName, chKeyValue, 256);
	return chKeyValue;
}

LONG SetGlobalEnvironment(CString strKeyName, CString strKeyValue)
{
	HKEY hOpenedKey;
	LONG ret;
	ret = ::RegOpenKeyEx(HKEY_LOCAL_MACHINE, "SYSTEM\\CurrentControlSet\\Control\\Session Manager\\Environment", 0, KEY_ALL_ACCESS, &hOpenedKey);
	if(ret == ERROR_SUCCESS)
	{
		BYTE buf[256] = {0};
		BYTE* p_buf = buf;
		wsprintf((char*)p_buf,"%s",strKeyValue);
		ret = ::RegSetValueEx( hOpenedKey, strKeyName, 0, REG_SZ, p_buf, strKeyValue.GetLength());
		DWORD dwReturnValue;
		if(ret == ERROR_SUCCESS)
			SendMessageTimeout(HWND_BROADCAST, WM_SETTINGCHANGE, 0,(LPARAM) "Environment", SMTO_ABORTIFHUNG, 5000, &dwReturnValue);
		RegCloseKey(hOpenedKey);
	}

	return ret;
}

BOOL Amqsailq(const char *strParas, char * szReturn, int & nSize)
{
	/***************************************************************************/
   /* MQAI variables                                                          */
   /***************************************************************************/
   MQCNO    Connect_options = {MQCNO_DEFAULT};
                                    /* MQCONNX options               */
   MQCD     ClientConn = {MQCD_CLIENT_CONN_DEFAULT};
                                    /* Client connection channel     */
                                    /* definition                    */
   MQHCONN hConn;                          /* handle to MQ connection         */
   MQCHAR qmName[MQ_Q_MGR_NAME_LENGTH+1]=""; /* default QMgr name             */
   MQLONG reason;                          /* reason code                     */
   MQLONG connReason;                      /* MQCONN reason code              */
   MQLONG compCode;                        /* completion code                 */
   MQHBAG adminBag = MQHB_UNUSABLE_HBAG;   /* admin bag for mqExecute         */
   MQHBAG responseBag = MQHB_UNUSABLE_HBAG;/* response bag for mqExecute      */ 
   MQHBAG qAttrsBag;                       /* bag containing q attributes     */
   MQHBAG errorBag;                        /* bag containing cmd server error */
   MQLONG mqExecuteCC;                     /* mqExecute completion code       */
   MQLONG mqExecuteRC;                     /* mqExecute reason code           */
   MQLONG qDepth;                          /* depth of queue                  */
   MQLONG qMaxDepth;                       /* max depth of queue              */
   MQLONG qMaxMsgLength;                   /* max length of message           */
   MQLONG qType;                           /* type of queue                   */
   MQCHAR qCreateDate[MQ_CREATION_DATE_LENGTH+1]="";/* create date of queue   */
   MQCHAR qCreateTime[MQ_CREATION_TIME_LENGTH+1]="";/* create time of queue   */
   MQCHAR qAlterDate[MQ_CREATION_DATE_LENGTH+1]="";/* alter date of queue     */
   MQCHAR qAlterTime[MQ_CREATION_TIME_LENGTH+1]="";/* alter time of queue     */
   MQLONG StringLength;					   /* Length of string returned       */
   MQLONG CodedCharSetId;				   /* Coded Character Set ID          */
   MQLONG qInputOpens;					   /* count of input opens            */
   MQLONG qOutputOpens;;				   /* count of output opens           */

   CString strReturn="";
   
   char *cQMName=NULL;
   cQMName = FindStrValue(strParas, "_MQQueueManager");
   char *cConnName=NULL;
	cConnName = FindStrValue(strParas, "_MachineName");

   /***************************************************************************/
   /* Connect to the queue manager                                            */
   /***************************************************************************/
   strncpy(ClientConn.ConnectionName,
             cConnName,
             MQ_CONN_NAME_LENGTH);
   strncpy(ClientConn.ChannelName,
             "SYSTEM.DEF.SVRCONN",
             MQ_CHANNEL_NAME_LENGTH);
   /* Point the MQCNO to the client connection definition */
   Connect_options.ClientConnPtr = &ClientConn;

   /* Client connection fields are in the version 2 part of the
      MQCNO so we must set the version number to 2 or they will
      be ignored */
   Connect_options.Version = MQCNO_VERSION_2;

   strncpy(qmName, cQMName, (size_t)MQ_Q_MGR_NAME_LENGTH);
   MQCONNX(qmName, &Connect_options, &hConn, &compCode, &connReason); 

   /***************************************************************************/
   /* Report the reason and stop if the connection failed.                    */
   /***************************************************************************/
   if (compCode == MQCC_FAILED)
   {
	  CheckCallResult("Queue Manager connection", compCode, connReason, szReturn);
	  return FALSE;
   }
   
   /***************************************************************************/
   /* Create an admin bag for the mqExecute call                              */
   /***************************************************************************/
   mqCreateBag(MQCBO_ADMIN_BAG, &adminBag, &compCode, &reason);
   if(!CheckCallResult("Create admin bag", compCode, reason, szReturn))
	   return FALSE;

   /***************************************************************************/
   /* Create a response bag for the mqExecute call                            */
   /***************************************************************************/
   mqCreateBag(MQCBO_ADMIN_BAG, &responseBag, &compCode, &reason);
   if(!CheckCallResult("Create response bag", compCode, reason, szReturn))
	   return FALSE;

   char *cQName=NULL;
   cQName = FindStrValue(strParas, "_MQQueueName");
	
   /* Inquire about a queue by supplying its name */
   /* (other parameters are optional) */
   mqAddString(adminBag, MQCA_Q_NAME, MQBL_NULL_TERMINATED, cQName, &compCode, &reason);
   if(!CheckCallResult("Add q name", compCode, reason, szReturn))
	   return FALSE;


   /***************************************************************************/
   /* Put the generic queue name into the admin bag                           */
   /***************************************************************************/
   //mqAddString(adminBag, MQCA_Q_NAME, MQBL_NULL_TERMINATED, "*", &compCode, &reason);
   //if(!CheckCallResult("Add q name", compCode, reason, szReturn))
   //   return FALSE;

   /***************************************************************************/
   /* Add an inquiry for queue type                                     */
   /***************************************************************************/
   mqAddInquiry(adminBag, MQIA_Q_TYPE, &compCode, &reason);
   if(!CheckCallResult("Add inquiry", compCode, reason, szReturn))
	   return FALSE;

   /***************************************************************************/
   /* Add an inquiry for max queue depths                                     */
   /***************************************************************************/
   mqAddInquiry(adminBag, MQIA_MAX_Q_DEPTH, &compCode, &reason);
   if(!CheckCallResult("Add inquiry", compCode, reason, szReturn))
	   return FALSE;

   /***************************************************************************/
   /* Add an inquiry for max message length                                     */
   /***************************************************************************/
   mqAddInquiry(adminBag, MQIA_MAX_MSG_LENGTH, &compCode, &reason);
   if(!CheckCallResult("Add inquiry", compCode, reason, szReturn))
	   return FALSE;
   
   /***************************************************************************/
   /* Add an inquiry for current queue depths                                 */
   /***************************************************************************/
   mqAddInquiry(adminBag, MQIA_CURRENT_Q_DEPTH, &compCode, &reason);
   if(!CheckCallResult("Add inquiry", compCode, reason, szReturn))
	   return FALSE;

   /***************************************************************************/
   /* Add an inquiry for create date of queue                                 */
   /***************************************************************************/
   mqAddInquiry(adminBag, MQCA_CREATION_DATE, &compCode, &reason);
   if(!CheckCallResult("Add inquiry", compCode, reason, szReturn))
	   return FALSE;

   /***************************************************************************/
   /* Add an inquiry for create time of queue                                 */
   /***************************************************************************/
   mqAddInquiry(adminBag, MQCA_CREATION_TIME, &compCode, &reason);
   if(!CheckCallResult("Add inquiry", compCode, reason, szReturn))
	   return FALSE;

   /***************************************************************************/
   /* Add an inquiry for alter date of queue                                 */
   /***************************************************************************/
   mqAddInquiry(adminBag, MQCA_ALTERATION_DATE, &compCode, &reason);
   if(!CheckCallResult("Add inquiry", compCode, reason, szReturn))
	   return FALSE;

   /***************************************************************************/
   /* Add an inquiry for alter time of queue                                 */
   /***************************************************************************/
   mqAddInquiry(adminBag, MQCA_ALTERATION_TIME, &compCode, &reason);
   if(!CheckCallResult("Add inquiry", compCode, reason, szReturn))
	   return FALSE;

   /***************************************************************************/
   /* Add an inquiry for input opens of queue                                 */
   /***************************************************************************/
   mqAddInquiry(adminBag, MQIA_OPEN_INPUT_COUNT, &compCode, &reason);
   if(!CheckCallResult("Add inquiry", compCode, reason, szReturn))
	   return FALSE;

   /***************************************************************************/
   /* Add an inquiry for output opens of queue                                */
   /***************************************************************************/
   mqAddInquiry(adminBag, MQIA_OPEN_OUTPUT_COUNT, &compCode, &reason);
   if(!CheckCallResult("Add inquiry", compCode, reason, szReturn))
	   return FALSE;

   /***************************************************************************/
   /* Send the command to find all the local queue names and queue depths.    */
   /* The mqExecute call creates the PCF structure required, sends it to      */
   /* the command server, and receives the reply from the command server into */
   /* the response bag. The attributes are contained in system bags that are  */
   /* embedded in the response bag, one set of attributes per bag.            */
   /***************************************************************************/
   mqExecute(hConn,                   /* MQ connection handle                 */
             MQCMD_INQUIRE_Q,         /* Command to be executed               */
             MQHB_NONE,               /* No options bag                       */
             adminBag,                /* Handle to bag containing commands    */
             responseBag,             /* Handle to bag to receive the response*/
             MQHO_NONE,               /* Put msg on SYSTEM.ADMIN.COMMAND.QUEUE*/  
             MQHO_NONE,               /* Create a dynamic q for the response  */ 
             &compCode,               /* Completion code from the mqexecute   */ 
             &reason);                /* Reason code from mqexecute call      */
   
   
   /***************************************************************************/
   /* Check the command server is started. If not exit.                       */
   /***************************************************************************/
   if (reason == MQRC_CMD_SERVER_NOT_AVAILABLE)
   {
		strReturn = "Please start the command server: <strmqcsv QMgrName>$";
		MQDISC(&hConn, &compCode, &reason);
		if(!CheckCallResult("Disconnect from Queue Manager", compCode, reason, szReturn))
			return FALSE;      
		strcpy(szReturn,strReturn);
		return FALSE;
   }  

   /***************************************************************************/
   /* Check the result from mqExecute call. If successful find the current    */
   /* depths of all the local queues. If failed find the error.               */
   /***************************************************************************/
   if ( compCode == MQCC_OK )                      /* Successful mqExecute    */
   {
		/***********************************************************************/
		/* Get the next system bag handle out of the mqExecute response bag.   */
		/* This bag contains the queue attributes                              */ 
		/***********************************************************************/
		mqInquireBag(responseBag, MQHA_BAG_HANDLE, 0, &qAttrsBag, &compCode, &reason);
		if(!CheckCallResult("Get the result bag handle", compCode, reason, szReturn))
			return FALSE;

		/***********************************************************************/
		/* Get the type out of the queue attributes bag                       */
		/***********************************************************************/
		mqInquireInteger(qAttrsBag, MQIA_Q_TYPE, MQIND_NONE, &qType, 
							&compCode, &reason); 
		if(!CheckCallResult("Get type", compCode, reason, szReturn))
			return FALSE; 

		/***********************************************************************/
		/* Get the depth out of the queue attributes bag                       */
		/***********************************************************************/
		mqInquireInteger(qAttrsBag, MQIA_CURRENT_Q_DEPTH, MQIND_NONE, &qDepth, 
							&compCode, &reason);
		if(!CheckCallResult("Get depth", compCode, reason, szReturn))
			return FALSE; 

		/***********************************************************************/
		/* Get the max depth out of the queue attributes bag                       */
		/***********************************************************************/
		mqInquireInteger(qAttrsBag, MQIA_MAX_Q_DEPTH, MQIND_NONE, &qMaxDepth, 
							&compCode, &reason);
		if(!CheckCallResult("Get max depth", compCode, reason, szReturn))
			return FALSE; 

		/***********************************************************************/
		/* Get the max msg length out of the queue attributes bag              */
		/***********************************************************************/
		mqInquireInteger(qAttrsBag, MQIA_MAX_MSG_LENGTH, MQIND_NONE, &qMaxMsgLength, 
							&compCode, &reason);
		if(!CheckCallResult("Get msg length", compCode, reason, szReturn))
			return FALSE; 

		/***********************************************************************/
		/* Get the create date of the queue attributes bag                     */
		/***********************************************************************/
		mqInquireString(qAttrsBag, MQCA_CREATION_DATE, MQIND_NONE, MQ_CREATION_DATE_LENGTH, 
							qCreateDate, &StringLength, &CodedCharSetId, &compCode, &reason);
		if(!CheckCallResult("Get create date", compCode, reason, szReturn))
			return FALSE; 

		/***********************************************************************/
		/* Get the create time of the queue attributes bag                     */
		/***********************************************************************/
		mqInquireString(qAttrsBag, MQCA_CREATION_TIME, MQIND_NONE, MQ_CREATION_TIME_LENGTH, 
							qCreateTime, &StringLength, &CodedCharSetId, &compCode, &reason);
		if(!CheckCallResult("Get create time", compCode, reason, szReturn))
			return FALSE; 

		/***********************************************************************/
		/* Get the alter date of the queue attributes bag                     */
		/***********************************************************************/
		mqInquireString(qAttrsBag, MQCA_ALTERATION_DATE, MQIND_NONE, MQ_CREATION_DATE_LENGTH, 
							qAlterDate, &StringLength, &CodedCharSetId, &compCode, &reason);
		if(!CheckCallResult("Get alter date", compCode, reason, szReturn))
			return FALSE; 

		/***********************************************************************/
		/* Get the alter time of the queue attributes bag                      */
		/***********************************************************************/
		mqInquireString(qAttrsBag, MQCA_ALTERATION_TIME, MQIND_NONE, MQ_CREATION_TIME_LENGTH, 
							qAlterTime, &StringLength, &CodedCharSetId, &compCode, &reason);
		if(!CheckCallResult("Get alter time", compCode, reason, szReturn))
			return FALSE; 

		/***********************************************************************/
		/* Get the input opens of the queue attributes bag                     */
		/***********************************************************************/
		mqInquireInteger(qAttrsBag, MQIA_OPEN_INPUT_COUNT, MQIND_NONE, &qInputOpens, 
							&compCode, &reason);
		if(!CheckCallResult("Get max depth", compCode, reason, szReturn))
			return FALSE; 

		/***********************************************************************/
		/* Get the output opens of the queue attributes bag                    */
		/***********************************************************************/
		mqInquireInteger(qAttrsBag, MQIA_OPEN_OUTPUT_COUNT, MQIND_NONE, &qOutputOpens, 
							&compCode, &reason);
		if(!CheckCallResult("Get msg length", compCode, reason, szReturn))
			return FALSE; 

		CString strQType;
		switch(qType)
		{
		case MQQT_LOCAL: 
			strQType = "本地队列";
			break;
		case MQQT_MODEL: 
			strQType = "模型队列";
			break;
		case MQQT_ALIAS: 
			strQType = "别名队列";
			break;
		case MQQT_REMOTE: 
			strQType = "远程队列";
			break;
		case MQQT_CLUSTER: 
			strQType = "群集队列";
			break;
		}
		strReturn.Format("szCurrentDepth=%d$szMaximumDepth=%d$szQueueType=%s$szMaxMsgLength=%d$szCreateDate=%s%s$szAlterDate=%s%s$\
						szInputOpens=%d$szOutputOpens=%d$", qDepth, qMaxDepth, strQType, 
						qMaxMsgLength, qCreateDate, qCreateTime, qAlterDate, qAlterTime, qInputOpens, qOutputOpens);
		strReturn.Replace("\t", NULL);
		strReturn.Replace(".", ":");
		strcpy(szReturn,strReturn);
   }

   else                                               /* Failed mqExecute     */
   {
		CString strReturn="";
		strReturn.Format("Call to get queue attributes failed: Completion Code = %d : Reason = %d$",
					compCode, reason);
		/*************************************************************************/
		/* If the command fails get the system bag handle out of the mqexecute   */
		/* response bag.This bag contains the reason from the command server     */
		/* why the command failed.                                               */ 
		/*************************************************************************/
		if (reason == MQRCCF_COMMAND_FAILED)
		{
		mqInquireBag(responseBag, MQHA_BAG_HANDLE, 0, &errorBag, &compCode, &reason);
		if(!CheckCallResult("Get the result bag handle", compCode, reason, szReturn))
			return FALSE;
	      
		/***********************************************************************/
		/* Get the completion code and reason code, returned by the command    */
		/* server, from the embedded error bag.                                */
		/***********************************************************************/
		mqInquireInteger(errorBag, MQIASY_COMP_CODE, MQIND_NONE, &mqExecuteCC, 
							&compCode, &reason );
		if(!CheckCallResult("Get the completion code from the result bag", compCode, reason, szReturn))
			return FALSE; 
		mqInquireInteger(errorBag, MQIASY_REASON, MQIND_NONE, &mqExecuteRC,
							&compCode, &reason);
		if(!CheckCallResult("Get the reason code from the result bag", compCode, reason, szReturn))
			return FALSE;
		strReturn.Format("Error returned by the command server: Completion Code = %d : Reason = %d\n",
					mqExecuteCC, mqExecuteRC);       
		}
		strcpy(szReturn,strReturn);
		return FALSE;
   }  

   /***************************************************************************/
   /* Delete the admin bag if successfully created.                           */
   /***************************************************************************/
   if (adminBag != MQHB_UNUSABLE_HBAG)
   {
      mqDeleteBag(&adminBag, &compCode, &reason);
      if(!CheckCallResult("Delete the admin bag", compCode, reason, szReturn))
		  return FALSE;
   }

   /***************************************************************************/
   /* Delete the response bag if successfully created.                        */
   /***************************************************************************/
   if (responseBag != MQHB_UNUSABLE_HBAG)
   {
      mqDeleteBag(&responseBag, &compCode, &reason);
      if(!CheckCallResult("Delete the response bag", compCode, reason, szReturn))
		  return FALSE;
   }

   /***************************************************************************/
   /* Disconnect from the queue manager if not already connected              */
   /***************************************************************************/
   if (connReason != MQRC_ALREADY_CONNECTED)
   {
      MQDISC(&hConn, &compCode, &reason);
      if(!CheckCallResult("Disconnect from Queue Manager", compCode, reason, szReturn))
		  return FALSE;         
   } 
   return TRUE;
}

BOOL Amqsailc(const char *strParas, char * szReturn, int & nSize)
{
	/***************************************************************************/
   /* MQAI variables                                                          */
   /***************************************************************************/
   MQCNO    Connect_options = {MQCNO_DEFAULT};
                                    /* MQCONNX options               */
   MQCD     ClientConn = {MQCD_CLIENT_CONN_DEFAULT};
                                    /* Client connection channel     */
                                    /* definition                    */
   MQHCONN hConn;                          /* handle to MQ connection         */
   MQCHAR qmName[MQ_Q_MGR_NAME_LENGTH+1]=""; /* default QMgr name             */
   MQLONG reason;                          /* reason code                     */
   MQLONG connReason;                      /* MQCONN reason code              */
   MQLONG compCode;                        /* completion code                 */
   MQHBAG adminBag = MQHB_UNUSABLE_HBAG;   /* admin bag for mqExecute         */
   MQHBAG responseBag = MQHB_UNUSABLE_HBAG;/* response bag for mqExecute      */ 
   MQHBAG qAttrsBag;                       /* bag containing q attributes     */
   MQHBAG errorBag;                        /* bag containing cmd server error */
   MQLONG mqExecuteCC;                     /* mqExecute completion code       */
   MQLONG mqExecuteRC;                     /* mqExecute reason code           */
   MQLONG chstatus;                        /* status of channel                   */
   MQCHAR chName[MQ_CHANNEL_NAME_LENGTH+1]="";
   MQLONG StringLength;					   /* Length of string returned       */
   MQLONG CodedCharSetId;				   /* Coded Character Set ID          */

   CString strReturn="";
   
   char *cQMName=NULL;
   cQMName = FindStrValue(strParas, "_MQQueueManager");
   char *cConnName=NULL;
	cConnName = FindStrValue(strParas, "_MachineName");

   /***************************************************************************/
   /* Connect to the queue manager                                            */
   /***************************************************************************/
   strncpy(ClientConn.ConnectionName,
             cConnName,
             MQ_CONN_NAME_LENGTH);
   strncpy(ClientConn.ChannelName,
             "SYSTEM.DEF.SVRCONN",
             MQ_CHANNEL_NAME_LENGTH);
   /* Point the MQCNO to the client connection definition */
   Connect_options.ClientConnPtr = &ClientConn;

   /* Client connection fields are in the version 2 part of the
      MQCNO so we must set the version number to 2 or they will
      be ignored */
   Connect_options.Version = MQCNO_VERSION_2;
   strncpy(qmName, cQMName, (size_t)MQ_Q_MGR_NAME_LENGTH);
   MQCONNX(qmName, &Connect_options, &hConn, &compCode, &connReason); 

   /***************************************************************************/
   /* Report the reason and stop if the connection failed.                    */
   /***************************************************************************/
   if (compCode == MQCC_FAILED)
   {
	  CheckCallResult("Queue Manager connection", compCode, connReason, szReturn);
	  return FALSE;
   }
   
   /***************************************************************************/
   /* Create an admin bag for the mqExecute call                              */
   /***************************************************************************/
   mqCreateBag(MQCBO_ADMIN_BAG, &adminBag, &compCode, &reason);
   if(!CheckCallResult("Create admin bag", compCode, reason, szReturn))
	   return FALSE;

   /***************************************************************************/
   /* Create a response bag for the mqExecute call                            */
   /***************************************************************************/
   mqCreateBag(MQCBO_ADMIN_BAG, &responseBag, &compCode, &reason);
   if(!CheckCallResult("Create response bag", compCode, reason, szReturn))
	   return FALSE;

   char *cChName=NULL;
   cChName = FindStrValue(strParas, "_MQChannelName");
	
   /* Inquire about a channel by supplying its name */
   /* (other parameters are optional) */
   mqAddString(adminBag, MQCACH_CHANNEL_NAME, MQBL_NULL_TERMINATED, cChName, &compCode, &reason);
   if(!CheckCallResult("Add channel name", compCode, reason, szReturn))
	   return FALSE;

 
   /***************************************************************************/
   /* Add an inquiry for channel status                                     */
   /***************************************************************************/
   mqAddInteger(adminBag, MQIACH_CHANNEL_INSTANCE_TYPE, MQOT_CURRENT_CHANNEL, &compCode, &reason);
   if(!CheckCallResult("Add inquiry", compCode, reason, szReturn))
	   return FALSE;

    mqAddInquiry(adminBag, MQIACH_CHANNEL_STATUS, &compCode, &reason);
   if(!CheckCallResult("Add inquiry", compCode, reason, szReturn))
	   return FALSE;

   // mqAddInteger(adminBag, MQIACH_CHANNEL_INSTANCE_ATTRS, MQIACF_ALL, &compCode, &reason);
   //if(!CheckCallResult("Add inquiry", compCode, reason, szReturn))
	  // return FALSE;

   /***************************************************************************/
   /* Send the command to find all the local queue names and queue depths.    */
   /* The mqExecute call creates the PCF structure required, sends it to      */
   /* the command server, and receives the reply from the command server into */
   /* the response bag. The attributes are contained in system bags that are  */
   /* embedded in the response bag, one set of attributes per bag.            */
   /***************************************************************************/
   mqExecute(hConn,                   /* MQ connection handle                 */
             MQCMD_INQUIRE_CHANNEL_STATUS ,         /* Command to be executed               */
             MQHB_NONE,               /* No options bag                       */
             adminBag,                /* Handle to bag containing commands    */
             responseBag,             /* Handle to bag to receive the response*/
             MQHO_NONE,               /* Put msg on SYSTEM.ADMIN.COMMAND.QUEUE*/  
             MQHO_NONE,               /* Create a dynamic q for the response  */ 
             &compCode,               /* Completion code from the mqexecute   */ 
             &reason);                /* Reason code from mqexecute call      */
   
   
   /***************************************************************************/
   /* Check the command server is started. If not exit.                       */
   /***************************************************************************/
   if (reason == MQRC_CMD_SERVER_NOT_AVAILABLE)
   {
		strReturn = "Please start the command server: <strmqcsv QMgrName>$";
		MQDISC(&hConn, &compCode, &reason);
		if(!CheckCallResult("Disconnect from Queue Manager", compCode, reason, szReturn))
			return FALSE;      
		strcpy(szReturn,strReturn);
		return FALSE;
   }  

   /***************************************************************************/
   /* Check the result from mqExecute call. If successful find the current    */
   /* depths of all the local queues. If failed find the error.               */
   /***************************************************************************/
   if ( compCode == MQCC_OK )                      /* Successful mqExecute    */
   {
	   /***********************************************************************/
		/* Get the next system bag handle out of the mqExecute response bag.   */
		/* This bag contains the queue attributes                              */ 
		/***********************************************************************/
		mqInquireBag(responseBag, MQHA_BAG_HANDLE, 0, &qAttrsBag, &compCode, &reason);
		if(!CheckCallResult("Get the result bag handle", compCode, reason, szReturn))
			return FALSE;

		mqInquireString(qAttrsBag, MQCACH_CHANNEL_NAME, MQIND_NONE, MQ_CHANNEL_NAME_LENGTH, 
							chName, &StringLength, &CodedCharSetId, &compCode, &reason);
		if(!CheckCallResult("Get channel status", compCode, reason, szReturn))
			return FALSE; 

		/***********************************************************************/
		/* Get the status out of the channel attributes bag                       */
		/***********************************************************************/
		mqInquireInteger(qAttrsBag, MQIACH_CHANNEL_STATUS, MQIND_NONE, &chstatus, 
							&compCode, &reason); 
		if(!CheckCallResult("Get channel status", compCode, reason, szReturn))
			return FALSE; 

		CString strChType;
		switch(chstatus)
		{
		case MQCHS_BINDING: 
			strChType = "正在绑定";
			break;
		case MQCHS_STARTING: 
			strChType = "正在启动";
			break;
		case MQCHS_RUNNING: 
			strChType = "正在运行";
			break;
		case MQCHS_PAUSED: 
			strChType = "已经暂停";
			break;
		case MQCHS_STOPPING: 
			strChType = "正在停止";
			break;
		case MQCHS_RETRYING: 
			strChType = "正在重试";
			break;
		case MQCHS_STOPPED: 
			strChType = "已经停止";
			break;
		case MQCHS_REQUESTING: 
			strChType = "正在请求连接";
			break;
		case MQCHS_INITIALIZING: 
			strChType = "正在初始化";
			break;
		}
		strReturn.Format("szChannelStatus=%s$", strChType);
		strcpy(szReturn,strReturn);
   }

   else                                               /* Failed mqExecute     */
   {
		strcpy(szReturn,"szChannelStatus=不活动或不存在");
		return TRUE;
   }  

   /***************************************************************************/
   /* Delete the admin bag if successfully created.                           */
   /***************************************************************************/
   if (adminBag != MQHB_UNUSABLE_HBAG)
   {
      mqDeleteBag(&adminBag, &compCode, &reason);
      if(!CheckCallResult("Delete the admin bag", compCode, reason, szReturn))
		  return FALSE;
   }

   /***************************************************************************/
   /* Delete the response bag if successfully created.                        */
   /***************************************************************************/
   if (responseBag != MQHB_UNUSABLE_HBAG)
   {
      mqDeleteBag(&responseBag, &compCode, &reason);
      if(!CheckCallResult("Delete the response bag", compCode, reason, szReturn))
		  return FALSE;
   }

   /***************************************************************************/
   /* Disconnect from the queue manager if not already connected              */
   /***************************************************************************/
   if (connReason != MQRC_ALREADY_CONNECTED)
   {
      MQDISC(&hConn, &compCode, &reason);
      if(!CheckCallResult("Disconnect from Queue Manager", compCode, reason, szReturn))
		  return FALSE;         
   } 
   return TRUE;
}

//__declspec(dllexport)
extern "C" __declspec(dllexport) 
BOOL GetMQInfo(const char *strParas, char * szReturn, int & nSize)
{
	/*
	std::list<char*> paramList;
	MakeStringListByChar(paramList, strParas);
	std::list<char *>::iterator pos = paramList.begin();

	 while(pos != paramList.end())
	{
		char * strTemp = *pos;
		strcpy(szReturn, strTemp);
		strcat(szReturn, " for debug zjw");
		OutputDebugString(szReturn);
		pos++;
	}
	 return true;
	 */
	BOOL bRet = Amqsailq(strParas, szReturn, nSize);

	CString strOutRet;
	strOutRet =szReturn;
	nSize = 2048;
	MakeCharByString(szReturn,nSize,strOutRet);	 

	return bRet;
}

extern "C" __declspec(dllexport) 
BOOL GetMQChannelInfo(const char *strParas, char * szReturn, int & nSize)
{
	BOOL bRet = Amqsailc(strParas, szReturn, nSize);

	CString strOutRet;
	strOutRet =szReturn;
	nSize = 2048;
	MakeCharByString(szReturn,nSize,strOutRet);	 
	return bRet;
}