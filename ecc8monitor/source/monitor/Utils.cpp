
#include "Utils.h"
#include "math.h"
#include <iostream>

using namespace std;


HINSTANCE g_hInstance = NULL;
HINSTANCE g_hInstance2 = NULL;

#ifdef WIN32
#pragma warning (disable : 4267)
#endif




int WSA_Init()
{
	WORD wVersionRequested;  
	WSADATA wsaData; 
	int err; 
	wVersionRequested = MAKEWORD(1, 1); 

	err = WSAStartup(wVersionRequested, &wsaData); 

	if (err != 0) 
		return -1; 

	if ( LOBYTE( wsaData.wVersion ) != 1 || 
	   HIBYTE( wsaData.wVersion ) != 1 ) 
	{ 
		WSACleanup(); 
		return -2;
	}

	return 0;
}

void WSA_Free()
{
	WSACleanup();
}

int socks5_protocol(SOCKET s, char *server, int port, char *pserver, int pport, 
					char *puid, char *ppwd, int timeout)
{
	struct hostent *hent;
	unsigned long	ip;
	sockaddr_in		sa;

	hent = gethostbyname(pserver);
	if(hent == NULL)
		return 1;

	memcpy(&sa.sin_addr, hent->h_addr_list[0], hent->h_length);
	sa.sin_family = AF_INET;
	sa.sin_port = htons(pport);

	if(connect_timeo(s, (sockaddr*)&sa, sizeof(sockaddr), timeout) < 0)
		return 1;

	unsigned char request[22];
	int request_len = 0;
	unsigned char response[22];
	int response_len = 0;

	request[0] = 0x05; // SOCKS version 5
	request[1] = 0x03; // number of auth procotols we recognize
	request[2] = 0x00; // no authentication required
	request[3] = 0x01; // GSSAPI
	request[4] = 0x02; // username/password

	request_len = 2 + request[1];
	int write_len = send(s, (const char *)request, request_len, 0);
	if (write_len != request_len) 
		return 1;

	response_len = 22;
	response_len = recv(s, (char *)response, response_len, 0);

	if (response_len <= 0) 
		return 1;

	if (response[0] != 0x05) 
		return 1;

	switch (response[1]) 
	{
		case 0x00:
			// no auth
			break;
		case 0x01:
			// GSSAPI
			return 1;
		case 0x02:
			{
			// username/password
			int i = 0;
			request[0] = 0x01;
			if(*puid)
			{
				request[1] = strlen(puid);
				for(i = 0;i < (int)strlen(puid);i ++)
					request[2+i] = puid[i];
			}
			else
			{
				request[1] = 0x01;
				request[2] = 0x00;
			}
			request_len = 2 + i;

			if(*ppwd)
			{
				request[request_len++] = strlen(ppwd);
				for(i = 0;i < (int)strlen(ppwd);i ++)
					request[request_len+i] = ppwd[i];
				request_len += strlen(ppwd);
			}
			else
			{
				request[request_len++] = 0x01;
				request[request_len++] = 0x00;
			}

			write_len = send(s, (const char *)request, request_len, 0);
			response_len = 22;
			response_len = recv(s, (char *)response, response_len, 0);
			if(response[0] != 1)
				return 1;
			if(response[1] != 0)
				return 1;
			}
			break;
		default:
			return 1;
	}

	// we are now authenticated, so lets tell
	// the server where to connect to
	request_len = 6;

	request[0] = 0x05; // SOCKS version 5
	request[1] = 0x01; // CONNECT command
	request[2] = 0x00; // obligatory reserved field (perfect for MS tampering!)
    
	request[3] = 0x01; // encoding of destination address (1 == IPv4)
	request_len += 4;

	char	ipaddr[20] = {0};
	ip = inet_addr(server);
	if (ip == INADDR_NONE)
	{
		hent = gethostbyname(server);
		if (hent == NULL)
			return 1;

		strcpy(ipaddr, inet_ntoa(*((struct in_addr *)hent->h_addr)));
	}
	else
	{
		strcpy(ipaddr, server);
	}
	
	if(sscanf(ipaddr, "%d.%d.%d.%d", &request[4], &request[5], &request[6], &request[7]) != 4)
		return 1;

	// destination port
	unsigned int destPort = htons(port);

	request[request_len-1] = (unsigned char)(destPort >> 8);
	request[request_len-2] = (unsigned char)destPort;

	if (send(s, (const char *)request, request_len, 0) != request_len)
		return 1;

	response_len = 22;
	response_len = recv(s, (char *)response, response_len, 0);
	if (response_len <= 0)
		return 1;

	if (response[0] != 0x05)
		return 1;

	if (response[1] != 0x00)
		return 1;

	return 0;
}

#define MAXL	200	/* Maximum length of last in & out fields */

double diff(char *a, char *b)
{
    char res[MAXL], *a1, *b1, *r1;
    int c,x,m;
    if (*a == '-' && *b == '-') 
	{
       b1 = b + 1;
       b = a + 1;
       a = b1;
    }
    
    while (!isdigit((int)*a)) a++;
    while (!isdigit((int)*b)) b++;
    a1 = &a[strlen(a)-1];
    m = max(strlen(a),strlen(b));
    r1 = &res[m+1];
    for (b1 = res;b1 <= r1; b1++) *b1 = ' ';
    b1 = &b[strlen(b)-1];
    r1[1] = 0;	/* Null terminate result */
    c = 0;
    for (x=0; x<m; x++) 
	{
		if (a1 >= a && b1 >= b) 
		{
			*r1 = ((*a1 - c) - *b1) + '0';
		} 
		else if (a1 >= a) 
		{
			*r1 = (*a1 - c);
		} 
		else 
		{
			*r1 = ('0' - *b1 - c) + '0';
		}
	
		if (*r1 < '0') 
		{
			*r1 += 10; 
			c=1;
		} 
		else 
		{
			c=0;
		}

		a1--;b1--;r1--;
    }

    if (c) 
	{
		r1 = &res[m+1];
		for (x=0; isdigit((int)*r1) && x<m; x++,r1--)  
		{
			*r1 = ('9' - *r1 + c) + '0';
			if (*r1 > '9') 
			{
				*r1 -= 10; 
				c=1;
			} 
			else 
			{
				c=0;
			}
		}

		return(-atof(res));
    } 
	else
		return(atof(res));
}

int get_ipaddrs(char *domain, char ipaddrs[64][20])
{
	int		i = 0;
	struct	hostent *hp;

	memset(ipaddrs, 0, sizeof(ipaddrs));

	if(WSA_Init())
	{
		return 1;
	}

	hp = gethostbyname(domain);
	if(hp == NULL)
	{
		return 1;
	}

	while(1)
	{
		if(hp->h_addr_list[i])
		{
			strcpy(ipaddrs[i], inet_ntoa(*((struct in_addr *)hp->h_addr_list[i])));
		}
		else
		{
			break;
		}

		if(i++ >= 20) 
			break;
	}

	WSA_Free();

	return 0;
}

/********************************************************
 *	standard average: 
 *		v = (a1+a2+....+an)/n 
 * 
 *	standard deviation: 
 *		v^2 = [(a1-a)^2+(a2-a)^2+....+(an-a)^2 ]/n 
 * 
 *	standard variation: 
 *		v = [(a1-a)^2+(a2-a)^2+....+(an-a)^2 ]/n 
 * 
 *	standard error: 
 *		v = [|a1-a|+|a2-a|+....+|an-a|]/n 
 ********************************************************/
double std_avg(double *dl_array, int n)
{
	int		i = 0;
	double	dl_ave = 0.0;

	for(i = 0;i < n;i ++)
		dl_ave += dl_array[i] / n;

	return dl_ave;
}

double std_dev(double *dl_array, int n)
{
	int		i = 0;
	double	dl_ave = 0.0;
	double	dl_tot = 0.0;

	dl_ave = std_avg(dl_array, n);

	for(i = 0;i < n;i ++)
		dl_tot += pow((dl_array[i] - dl_ave), 2) / n;

	return sqrt(dl_tot);
}

double std_var(double *dl_array, int n)
{
	return pow(std_dev(dl_array, n), 2);
}

double std_err(double *dl_array, int n)
{
	int		i = 0;
	double	dl_ave = 0.0;
	double	dl_tot = 0.0;

	dl_ave = std_avg(dl_array, n);

	for(i = 0;i < n;i ++)
		dl_tot += fabs(dl_array[i] - dl_ave) / n;

	return dl_tot;
}

void OsDetect(CString &strOsInfo) 
{
	OSVERSIONINFO	OSversion;
	
	OSversion.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);

	::GetVersionEx(&OSversion);

	switch(OSversion.dwPlatformId)
	{
    case VER_PLATFORM_WIN32s: 
		strOsInfo.Format("Windows %d.%d",OSversion.dwMajorVersion, OSversion.dwMinorVersion);
		break;
	case VER_PLATFORM_WIN32_WINDOWS:
		if(OSversion.dwMinorVersion == 0)
			strOsInfo = "Windows 95";  
		else if(OSversion.dwMinorVersion == 10)  
			strOsInfo = "Windows 98";
		else if(OSversion.dwMinorVersion == 90)  
			strOsInfo = "Windows Me";	
		break;        
	case VER_PLATFORM_WIN32_NT:
		if(OSversion.dwMajorVersion == 5 && OSversion.dwMinorVersion == 0)
			strOsInfo.Format("Windows 2000 With %s",OSversion.szCSDVersion);
		else if(OSversion.dwMajorVersion == 5 && OSversion.dwMinorVersion == 1)
			strOsInfo.Format("Windows XP %s",OSversion.szCSDVersion);
		else if(OSversion.dwMajorVersion <= 4) 	  
			strOsInfo.Format("Windows NT %d.%d with %s",OSversion.dwMajorVersion,
			   OSversion.dwMinorVersion,OSversion.szCSDVersion);
		else	
			// for unknown windows/newest windows version	  
			strOsInfo.Format("Windows %d.%d ",OSversion.dwMajorVersion,
				OSversion.dwMinorVersion);
		break;
	default:
		strOsInfo.Format("%s", "Unknown Operating System");
		break;
	}
}
