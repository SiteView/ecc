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
#include "check.h"
#include "eventlog.h"
#include "log.h"
#include "service.h"
#include "syslog.h"
#include "ver.h"
#include "winevent.h"

/* Main eventlog monitoring loop */
int MainLoop()
{
	char * output = NULL;
	EventList IgnoredEvents[MAX_IGNORED_EVENTS];
	BOOL winEvents = FALSE;
	HKEY hkey = NULL;
	int level;
	int log;
	int stat_counter = 0;
	FILE *fp = NULL;

	/* Grab Ignore List From File */
	CheckSyslogIgnoreFile(IgnoredEvents, CONFIG_FILE);

	/* Check if the new Windows Events Service is in use */
	/* If so we will use the new API's to sift through events */
	if (RegOpenKeyEx(HKEY_LOCAL_MACHINE, "SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\WINEVT\\Channels\\ForwardedEvents", 0, KEY_READ, &hkey) != ERROR_SUCCESS)
		winEvents = FALSE;
	else
		winEvents = TRUE;
		
	if (hkey)
		RegCloseKey(hkey);

	/* Gather eventlog names */
	if (RegistryGather(winEvents))
		return 1;

	/* Open all eventlogs */
	if (winEvents == FALSE) {
		if (EventlogsOpen())
			return 1;
	} else
		if (WinEventlogsOpen())
			return 1;

	/* Service is now running */
	Log(LOG_INFO, "Eventlog to Syslog Service Started: Version %s (%s-bit)", VERSION,
#ifdef _WIN64
		"64"
#else
		"32"
#endif
	);

	/* Loop while service is running */
	do {

		/* Process records */
		if (winEvents == FALSE) {
			for (log = 0; log < EventlogCount; log++) {
				/* Loop for all messages */
				while ((output = EventlogNext(IgnoredEvents, log, &level)))
					if ((_strnicmp(output, "Skip!!!", strlen(output))) != 0)
						if (SyslogSend(output, level)) {
							ServiceIsRunning = FALSE;
							break;
						}
			}
		} else {
			for (log = 0; log < WinEventlogCount; log++) {
				if (WinEventlogNext(IgnoredEvents, log) == NULL) {
					ServiceIsRunning = FALSE;
					break;
				}
			}
		}
		
		/* Modify file every ~2 minutes to let others know that service is functional */
		if (++stat_counter == 24) {
			stat_counter = 0; /* Reset Counter */

			if(fopen_s(&fp, "evtsys.stat", "a+") != 0)
				Log(LOG_ERROR|LOG_SYS, "Status file did not open!!!");
			else {
				fprintf_s(fp,"%s - Eventlog to Syslog Service Running\n",GetTimeStamp());
				if(fclose(fp) != 0)
					Log(LOG_ERROR|LOG_SYS, "Error closing status file!!!");
			}
		}

		/* Sleep five seconds */
		Sleep(5000);

	} while (ServiceIsRunning);

	/* Service is stopped */
	Log(LOG_INFO, "Eventlog to Syslog Service Stopped");

	/* Close eventlogs */
	EventlogsClose();

	/* Success */
	return 0;
}
