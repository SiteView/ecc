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

/* Include files */
#include "main.h"
#include "syslog.h"

/* syslog */

/* Application data configuration */
char SyslogLogHost[SYSLOG_HOST_SZ+1];
char SyslogLogHost2[SYSLOG_HOST_SZ+1];
char SyslogConfigFile[MAX_CONFIG_FNAME+1];
DWORD SyslogPort = SYSLOG_DEF_PORT;
DWORD SyslogFacility = SYSLOG_DEF_FAC;

/* Open syslog connection */
int SyslogOpen(int ID)
{
	if (ID == 1)
		return WSockOpen(SyslogLogHost, (unsigned short) SyslogPort, 1);
	else
		return WSockOpen(SyslogLogHost2, (unsigned short) SyslogPort, 2);
}

/* Close syslog connection */
void SyslogClose()
{
	WSockClose();
}

/* Send a message to the syslog server */
int SyslogSend(char * message, int level)
{
	char error_message[SYSLOG_SZ];

	/* Write priority level */
	_snprintf_s(error_message, sizeof(error_message), _TRUNCATE,
		"<%d>%s",
		level,
		message
	);

	fputs(error_message, stderr);
	fputc('\n', stderr);

	/* Send result to syslog server */
	return WSockSend(error_message);
}
