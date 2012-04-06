/*
  This code is a modification of the original Eventlog to Syslog Script written by
  Curtis Smith of Purdue University. The original copyright notice can be found below.
  
  The original program was modified by Sherwin Faria for Rochester Institute of Technology
  in July 2009 to provide bug fixes and add several new features. Additions include
  the ability to ignore specific events, add the event timestamp to outgoing messages,
  a service status file, and compatibility with the new Vista/2k8 Windows Events service.

     Sherwin Faria
	 Rochester Institute of Technology
	 Information & Technology Services Bldg. 10
	 1 Lomb Memorial Drive
	 Rochester, NY 14623 U.S.A.
	 
	Send all comments, suggestions, or bug reports to:
		seftch@rit.edu
*/
 
/*
  Copyright (c) 1998-2007, Purdue University
  All rights reserved.

  Redistribution and use in source and binary forms are permitted provided
  that:

  (1) source distributions retain this entire copyright notice and comment,
      and
  (2) distributions including binaries display the following acknowledgement:

         "This product includes software developed by Purdue University."

      in the documentation or other materials provided with the distribution
      and in all advertising materials mentioning features or use of this
      software.

  The name of the University may not be used to endorse or promote products
  derived from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR IMPLIED
  WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
  MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.

  This software was developed by:
     Curtis Smith

     Purdue University
     Engineering Computer Network
     465 Northwestern Avenue
     West Lafayette, Indiana 47907-2035 U.S.A.

  Send all comments, suggestions, or bug reports to:
     software@ecn.purdue.edu

*/

/* Basic include files */
#include <windows.h>
#include <winsock.h>
#include <lm.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

typedef struct EVENT_LIST EventList;

/* Macros */
#define COUNT_OF(x)	(sizeof(x)/sizeof(*x))

/* Constants */
#define ERRMSG_SZ	256
#define MAX_IGNORED_EVENTS	256
#define CONFIG_FILE	"evtsys.cfg"

/* Compatibility */
#define in_addr_t	unsigned long

/* Prototypes */
int CheckSyslogFacility(char * facility);
int CheckSyslogPort(char * port);
int CheckSyslogLogHost(char * loghost, int ID);
int CheckSyslogIgnoreFile(EventList * ignore_list, char * filename);
char * CollapseExpandMessage(char * message);
int IgnoreSyslogEvent(EventList * ignore_list, const char * E_SOURCE, int E_ID);
int EventlogCreate(char * name);
int WinEventlogCreate(char * name);
void EventlogsClose(void);
void WinEventlogsClose(void);
int EventlogsOpen(void);
int WinEventlogsOpen(void);
char * EventlogNext(EventList ignore_list[MAX_IGNORED_EVENTS], int log, int * level);
char * WinEventlogNext(EventList ignore_list[MAX_IGNORED_EVENTS], int log);
int GetOpt(int nargc, char ** nargv, char * ostr);
char * GetWinEvent(char * log, int recNum, int event_id);
int LogStart(void);
void LogStop(void);
void Log(int level, char * message, ...);
int MainLoop(void);
int RegistryInstall(void);
int RegistryUninstall(void);
int RegistryRead(void);
int RegistryGather(BOOL wEvents);
int ServiceInstall(void);
int ServiceRemove(void);
DWORD WINAPI ServiceStart(void);
void GetError(DWORD err_num, char * message, int len);
char * GetUsername(SID * sid);
char * GetTimeStamp(void);
char * LookupMessageFile(char * logtype, char * source, DWORD eventID);
char * FormatLibraryMessage(char * message_file, DWORD event_id, char ** string_array);
int SyslogOpen(int ID);
void SyslogClose(void);
int SyslogSend(char * message, int level);
char * TimeToString(DWORD dw);
char * WinEvtTimeToString(ULONGLONG fTime);
int WSockStart(void);
void WSockStop(void);
int WSockOpen(char * loghost, unsigned short port, int ID);
void WSockClose(void);
int WSockSend(char * message);
