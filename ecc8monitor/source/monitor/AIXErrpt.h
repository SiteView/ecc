
#include "stdafx.h"

bool AIXERRPT_TEL_VALUE(char *server, int port, char *uid, char *pwd, 
						char *pserver, int pport, char *puid, char *ppwd, 
						char *lprom, char *pprom, char *prom, char *inifile, 
						int ngid, char* strmid, char *custpath, char *szReturn);
BOOL AIXERRPT_SSH_VALUE(char *server, int port, char *uid, char *pwd, 
						char *inifile, int ngid, char* strmid, char *custpath, char *privatekeyfile,
						char *szReturn);
BOOL AIXERRPT_HTTP_VALUE(char *url, char *uid, char *pwd, char *pserver, int pport, 
						 char *puid, char *ppwd, char *inifile, 
						 int ngid, int nmid, char *custpath, char *szReturn);
BOOL AIXERRPT_RLOGIN_VALUE(char *server, int port, char *uid, char *pwd, 
						   char *pserver, int pport, char *puid, char *ppwd, 
						   char *pprom, char *prom, char *inifile, 
						   int ngid, int nmid, char *custpath, char *szReturn);


int convert_date(char *month, int day, int hours, int mins, int year, char *curdate);
