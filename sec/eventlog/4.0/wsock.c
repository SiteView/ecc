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
#include "log.h"

/* WinSock */

/* Indicate if WSAStartup was called */
static WSADATA ws_data;
static BOOL WSockStarted = FALSE;

/* Connection socket */
SOCKET WSockSocket = INVALID_SOCKET;
SOCKET WSockSocket2 = INVALID_SOCKET;

/* Where to send syslog information */
static struct sockaddr_in WSockAddress;
static struct sockaddr_in WSockAddress2;

/* Start Winsock access */
int WSockStart()
{
	/* Check to see if started */
	if (WSockStarted == FALSE) {

		/* See if version 2.0 is available */
		if (WSAStartup(MAKEWORD(2, 0), &ws_data)) {
			Log(LOG_ERROR, "Cannot initialize WinSock interface");
			return 1;
		}

		/* Set indicator */
		WSockStarted = TRUE;
	}

	/* Success */
	return 0;
}

/* Stop Winsock access */
void WSockStop()
{
	/* Check to see if started */
	if (WSockStarted) {

		/* Clean up winsock interface */
		WSACleanup();

		/* Reset indicator */
		WSockStarted = FALSE;
	}
}

/* Open connection to syslog */
int WSockOpen(char * loghost, unsigned short port, int ID)
{
	in_addr_t ip;
	
	/* Convert IP number */
	ip = inet_addr(loghost);
	if (ip == (in_addr_t)(-1)) {
		Log(LOG_ERROR, "Invalid log host: \"%s\"", loghost);
		return 1;
	}

	/* Initialize remote address structure */
	/* Terrible workaround so that we don't have to rewrite */
	/* the old implementation. Needs to be fixed properly.  */
	/*                                             Sherwin  */
	if (ID == 1) {
		memset(&WSockAddress, 0, sizeof(WSockAddress));
		WSockAddress.sin_family = AF_INET;
		WSockAddress.sin_port = htons(port);
		WSockAddress.sin_addr.s_addr = ip;

		/* Create socket */
		WSockSocket = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
		if (WSockSocket == INVALID_SOCKET) {
			Log(LOG_ERROR|LOG_SYS, "Cannot create a datagram socket for primary host");
			return 1;
		}
	} else {
		/* Initialize remote address structure */
		memset(&WSockAddress2, 0, sizeof(WSockAddress2));
		WSockAddress2.sin_family = AF_INET;
		WSockAddress2.sin_port = htons(port);
		WSockAddress2.sin_addr.s_addr = ip;

		/* Create socket */
		WSockSocket2 = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
		if (WSockSocket2 == INVALID_SOCKET) {
			Log(LOG_ERROR|LOG_SYS, "Cannot create a datagram socket for secondary host");
			return 1;
		}
	}

	/* Success */
	return 0;
}

/* Close connection */
void WSockClose()
{
	/* Close if open */
	if (WSockSocket != INVALID_SOCKET) {
		closesocket(WSockSocket);
		WSockSocket = INVALID_SOCKET;
	}
	if (WSockSocket2 != INVALID_SOCKET) {
		closesocket(WSockSocket2);
		WSockSocket2 = INVALID_SOCKET;
	}
}

/* Send data to syslog */
int WSockSend(char * message)
{
	int len;

	/* Get message length */
	len = (int) strlen(message);

	/* Send to syslog server */
	if (sendto(WSockSocket, message, len, 0, (struct sockaddr *) &WSockAddress, sizeof(WSockAddress)) != len) {
		if (h_errno != WSAEHOSTUNREACH && h_errno != WSAENETUNREACH) {
			Log(LOG_ERROR|LOG_SYS, "Cannot send message through socket for host 1");
			return 1;
		}
	}
	if (WSockSocket2 != INVALID_SOCKET)
		if (sendto(WSockSocket2, message, len, 0, (struct sockaddr *) &WSockAddress2, sizeof(WSockAddress2)) != len) {
			if (h_errno != WSAEHOSTUNREACH && h_errno != WSAENETUNREACH) {
				Log(LOG_ERROR|LOG_SYS, "Cannot send message through socket for host 2");
				return 1;
			}
		}

	/* Success */
	return 0;
}
