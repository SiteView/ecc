
#include "stdafx.h"

int WSA_Init();
void WSA_Free();

int connect_timeo(int sockfd, const struct sockaddr *saptr, int salen, int nsec);
int RESPONSE_WAIT(SOCKET s, int nsec);
BOOL GetUrlData(char *url, int timeout, char *uid, char *pwd, 
				char *pserver, int pport, char *puid, char *ppwd, 
				int dlen, char *szReturn);
BOOL GetIniFileName(const char *FileName, char *g_IniFileName);
BOOL MONITOR_PARSE(int monitortype, char *databuffer, char *inifile, char *extraparam, char *szReturn);
BOOL MONITOR_PARSE_zw(int monitortype, char *databuffer, char *inifile, char *extraparam, char* szTotalMem, char *szReturn);

extern "C" __declspec (dllexport)
BOOL RETURN_COMMAND(int monitortype, char *inifile, char *cmd);
int socks5_protocol(SOCKET s, char *server, int port, char *pserver, int pport, 
					char *puid, char *ppwd, int timeout);
double diff(char *a, char *b);
int get_ipaddrs(char *domain, char ipaddrs[64][20]);
double std_dev(double *dl_array, int n);
double std_var(double *dl_array, int n);
double std_err(double *dl_array, int n);
void OsDetect(CString &strOsInfo);
