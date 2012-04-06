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
#include "syslog.h"
#include "check.h"

int IGNORED_LINES;

/* Facility conversion table */
static struct {
	char * name;
	int id;
} FacilityTable[] = {
	{ "auth", SYSLOG_AUTH },
	{ "authpriv", SYSLOG_AUTHPRIV },
	{ "cron", SYSLOG_CRON },
	{ "daemon", SYSLOG_DAEMON },
	{ "ftp", SYSLOG_FTP },
	{ "kern", SYSLOG_KERN },
	{ "local0", SYSLOG_LOCAL0 },
	{ "local1", SYSLOG_LOCAL1 },
	{ "local2", SYSLOG_LOCAL2 },
	{ "local3", SYSLOG_LOCAL3 },
	{ "local4", SYSLOG_LOCAL4 },
	{ "local5", SYSLOG_LOCAL5 },
	{ "local6", SYSLOG_LOCAL6 },
	{ "local7", SYSLOG_LOCAL7 },
	{ "lpr", SYSLOG_LPR },
	{ "mail", SYSLOG_MAIL },
	{ "news", SYSLOG_NEWS },
	{ "ntp", SYSLOG_NTP },
	{ "security", SYSLOG_SECURITY },
	{ "user", SYSLOG_USER },
	{ "uucp", SYSLOG_UUCP }
};

/* Check facility name */
int CheckSyslogFacility(char * facility)
{
	int i;

	/* Try looking up name */
	for (i = 0; i < COUNT_OF(FacilityTable); i++)
		if (_stricmp(FacilityTable[i].name, facility) == 0)
			break;
	if (i == COUNT_OF(FacilityTable)) {
		Log(LOG_ERROR, "Invalid facility name: \"%s\"", facility);
		return 1;
	}

	/* Store new value */
	SyslogFacility = FacilityTable[i].id;

	/* Success */
	return 0;
}

/* Check port number */
int CheckSyslogPort(char * port)
{
	DWORD value;
	char * eos;
	struct servent * service;

	/* Try converting to integer */
	value = strtoul(port, &eos, 10);
	if (eos == port || *eos != '\0') {

		/* Try looking up name */
		service = getservbyname(port, "udp");
		if (service == NULL) {
			Log(LOG_ERROR, "Invalid service name: \"%s\"", port);
			return 1;
		}

		/* Convert back to host order */
		value = ntohs(service->s_port);
	} else {

		/* Check for valid number */
		if (value <= 0 || value > 0xffff) {
			Log(LOG_ERROR, "Invalid service number: %u", value);
			return 1;
		}
	}

	/* Store new value */
	SyslogPort = value;

	/* Success */
	return 0;
}

/* Check log host */
int CheckSyslogLogHost(char * loghost, int ID)
{
	char * ipstr;
	in_addr_t ip;
	struct hostent * host;
	struct in_addr ia;

	/* Attempt to convert IP number */
	ip = inet_addr(loghost);
	if (ip == (in_addr_t)(-1)) {

		/* Attempt to convert host name */
		host = gethostbyname(loghost);
		if (host == NULL) {
			Log(LOG_ERROR, "Invalid log host: \"%s\"", loghost);
			return 1;
		}

		/* Set ip */
		ip = *(in_addr_t *)host->h_addr;
	}

	/* Convert to IP */
	ia.s_addr = ip;
	ipstr = inet_ntoa(ia);

	/* Check size */
	if (strlen(ipstr) > sizeof(SyslogLogHost)-1) {
		Log(LOG_ERROR, "Log host address too long: \"%s\"", ipstr);
		return 1;
	}

	/* Store new value */
	if (ID == 1)
		strncpy_s(SyslogLogHost, sizeof(SyslogLogHost), ipstr, _TRUNCATE);
	else
		strncpy_s(SyslogLogHost2, sizeof(SyslogLogHost2), ipstr, _TRUNCATE);

	/* Success */
	return 0;
}

/* Check ignore file */
int CheckSyslogIgnoreFile(EventList * ignore_list, char * filename)
{
	FILE *file;
	fopen_s(&file, filename, "r");

	if (file != NULL)
	{
		char line[100];
		char strDelim[] = ":";
		char strComment[] = "'";
		char *strID,
			 *strSource,
			 *next_token;
		int comments = 1;
		int i = 0;

		while (fgets(line, sizeof(line), file) != NULL) { /* read a line */
			if (!(strncmp(line, strComment, 1))) {
				comments++;
			}
			else {
				strSource = strtok_s(line, strDelim, &next_token);
				strID = strtok_s(NULL, strDelim, &next_token);
				if (strSource == NULL || strID == NULL) {
					Log(LOG_ERROR,"File format incorrect: %s line: %i", filename, i + comments);
					Log(LOG_ERROR,"Format should be \"EventSource:EventID\" w/o quotes.");
					return -1;
				}

				/* Stop at MAX lines */
				if (i < MAX_IGNORED_EVENTS) {
					ignore_list[i].id = atoi(strID); /* Enter id into array */
					strncpy_s(ignore_list[i].source, sizeof(ignore_list[i].source), strSource, _TRUNCATE); /* Enter source into array */

					//if(LogInteractive)
						//printf("IgnoredEvents[%i].id=%i \tIgnoredEvents[%i].source=%s\n",i,ignore_list[i].id,i,ignore_list[i].source);
				} else {
					/* Notify if there are too many lines */
					Log(LOG_ERROR,"Config file too large. Max size is %i lines. Truncating...", MAX_IGNORED_EVENTS);
					break;
				}
				i++;
			}
		}
		fclose (file);
		IGNORED_LINES = i;

	} else {
		Log(LOG_ERROR|LOG_SYS,"Error opening file: %s", filename);
		Log(LOG_INFO,"Creating file with filename: %s", filename);

		if (fopen_s(&file, filename, "w") != 0) {
			Log(LOG_ERROR|LOG_SYS,"File could not be created: %s", filename);
			return -1;
		}

		fprintf_s(file, "'!!!!THIS FILE IS REQUIRED FOR THE SERVICE TO FUNCTION!!!!\n'\n");
		fprintf_s(file, "'Comments must start with an apostrophe and\n");
		fprintf_s(file, "'must be the only thing on that line.\n'\n");
		fprintf_s(file, "'Do not combine comments and definitions on the same line!\n'\n");
		fprintf_s(file, "'Format is as follows - EventSource:EventID\n");
		fprintf_s(file, "'In Vista/2k8 and upwards remove the 'Microsoft-Windows-' prefix\n");
		fprintf_s(file, "'**********************:**************************");

		fclose (file);
	}

	/* Success */
	return 0;
}
