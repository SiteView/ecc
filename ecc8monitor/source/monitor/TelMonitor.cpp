
#include "TelMonitor.h"
#include "Utils.h"
#include <fstream>
#include <iostream>
using namespace std;

#ifdef WIN32
#pragma warning (disable : 4267)
#endif


struct OL ol[] = {
	{TOPT_ECHO,   ddww_echo,  NULL},
	{TOPT_SUPP,   ddww_supp,  NULL},
	{TOPT_TERM,   ddww_term,  sbproc_term},
	{TOPT_ERROR,  ddww_error, NULL}
};

struct TERMINAL terminal[NUM_TERMINALS] = {
	{ "NVT", nvt }, 
	{ "ANSI", ansi }
};

struct CODEC codec[NUM_CODEC] = {
	{'m',ansi_set_screen_attribute},
	{'H',ansi_set_position},
	{'K',ansi_erase_line},
	{'J',ansi_erase_screen},
	{'A',ansi_move_up},
	{0,0}
};


SOCKET telnet_init(char *server, int port, char *proxy_server, int proxy_port, char *proxy_uid, char *proxy_pwd)
{
	unsigned long	ip;
	hostent			*ent;
	sockaddr_in		sa;
	SOCKET			hSocket;
	
	ent = gethostbyname(server);
	if(ent == NULL) 
		return -1;
	
	ip = *(unsigned long*)(ent->h_addr);

	sa.sin_family = AF_INET;
	sa.sin_port = htons(port);
	sa.sin_addr = *(in_addr*)&ip;

	hSocket = socket(PF_INET,SOCK_STREAM,IPPROTO_TCP);
	if (hSocket == INVALID_SOCKET) 
		return -2;

	if(connect_timeo(hSocket, (sockaddr*)&sa, sizeof(sockaddr), CONNECT_TIMEOUT) < 0)
	//if(connect(hSocket, (sockaddr *)&sa, sizeof(sockaddr)) < 0)
		return -3;

	
	return hSocket;
}

int telnet_connect(SOCKET hSocket, char *uid, char *pwd, 
				   char *logprompt, char *pwdprompt, char arprompt[PR_NUM][256],
                   LPGDATA pgd)
{
	int		n = 0;
	char	buffer[4096] = {0};
	char	*scan = NULL;
	int		telnetprogress = 0;
	int		count = 0;
	memset(pgd->databuffer, 0, BUF_SIZE);
	pgd->datalen = 0;
	int nTime=0;

	while(1)
	{
LOOP:
		memset(buffer, 0, 4096);
		if(RESPONSE_WAIT(hSocket, READ_TIMEOUT) <= 0)
		{
			if((telnetprogress==2)&&(nTime<3))
			{
				char buf[10]={0};
				sprintf(buf,"\r\n");
				send(hSocket, buf, strlen(buf),0);
				nTime++;
				goto LOOP;
			}

			return -1;
		} 
		Sleep(500);
		n = recv(hSocket, buffer, 4095, 0);
		
		if(n == SOCKET_ERROR)
		{
			int wserror;
			wserror=WSAGetLastError();
			CString stmp;
			stmp.Format("telnet connect failed --error code =%d",wserror);
			printf(stmp);
			return -2;
		}
		buffer[n] = 0;
		scan = buffer;
		while(n--)telnet_protocol(hSocket, *scan++,pgd);
		switch(telnetprogress)
		{
		case 0:
            if(logprompt)
            {
			    if(strstr(pgd->databuffer, logprompt))
			    {
				    char	bufUser[255] = {0};
				    strcpy(bufUser, uid);
					for(int nS=0;nS<strlen(bufUser);nS++)
					{
						Sleep(20);
						send(hSocket, &bufUser[nS], 1,0);	
					}
					 send(hSocket,"\r\n",2,0);

//				    strcat(bufUser, "\r\n");
					
//				    Sleep(100);			// delay
//				    send(hSocket, bufUser, strlen(bufUser),0);


				    telnetprogress ++;
				    count = 0;
				    memset(pgd->databuffer, 0, BUF_SIZE);
				    pgd->datalen = 0;
			    }
            }
            else
            {
                printf("Read Data Failed from Server(1)");
                return -7;
            }
			break;
		case 1:
            if(pwdprompt)
            {
			    if(strstr(pgd->databuffer, pwdprompt))
			    {
				    char	bufPwd[255] = {0};
				    strcpy(bufPwd, pwd);
				    strcat(bufPwd, "\r\n");
				    Sleep(100);			// delay
				    send(hSocket, bufPwd, strlen(bufPwd), 0);
				    telnetprogress ++;
				    count = 0;
				    memset(pgd->databuffer, 0, BUF_SIZE);
				    pgd->datalen = 0;
			    }
            }
            else
            {
                printf("Read Data Failed from Server(2)");
                return -7;
            }
			break;
		case 2:
			if(strstr(pgd->databuffer, "ogin incorrect"))
			{
				memset(pgd->databuffer, 0, BUF_SIZE);
				pgd->datalen = 0;
				return -6;
			}

			int		i = 0;
			char	prompt[256];
//			telnet_setlinewidth(hSocket, 0x00, 0xef);
			while(i < PR_NUM && *arprompt[i])
			{
				memset(prompt, 0, sizeof(prompt));
				strcpy(prompt, arprompt[i]);
				if(strstr(pgd->databuffer, prompt))
				{
					memset(pgd->databuffer, 0, BUF_SIZE);
					pgd->datalen = 0;
					return 0;
				}
				i ++;
			}
			
			break;
		}
		if(++count > 80)
		{
			if(telnetprogress == 0)
				return -3;
			if(telnetprogress == 1) 
				return -4;
			if(telnetprogress == 2) 
				return -5;
		}
        //改变默认的telnet行的长度
//        telnet_setlinewidth(hSocket, 0x00, 0xef);
	}
}

int telnet_setlinewidth(SOCKET hSocket, BYTE HighByte, BYTE LowByte)
{
	
    BYTE SB_NAWS[9];
	/*
    SB_NAWS[0] = 0xff;
    SB_NAWS[1] = 0xfa;
    SB_NAWS[2] = 0x1f;
    SB_NAWS[3] = HighByte;
    SB_NAWS[4] = LowByte;
    SB_NAWS[5] = 0x00;
    SB_NAWS[6] = 0xef;
    SB_NAWS[7] = 0xff;
    SB_NAWS[8] = 0xf0;
	*/
	SB_NAWS[0] = 0xff;
    SB_NAWS[1] = 0xfb;
    SB_NAWS[2] = 0x1f;

	send(hSocket, (char*)SB_NAWS, 3, 0);
	Sleep(500);

	SB_NAWS[0] = 0xff;
    SB_NAWS[1] = 0xfa;
    SB_NAWS[2] = 0x1f;
    SB_NAWS[3] = 0x00;
    SB_NAWS[4] = 0xA0;
    SB_NAWS[5] = 0x00;
    SB_NAWS[6] = 0xa0;
    SB_NAWS[7] = 0xff;
    SB_NAWS[8] = 0xf0;
    send(hSocket, (char*)SB_NAWS, 9, 0);
    return 1;
}

extern "C" __declspec(dllexport)
int telnet_connect(SOCKET hSocket, char *uid, char *pwd, 
				   char *logprompt, char *pwdprompt, char arprompt[PR_NUM][256],
                   LPGDATA pgd, struct Tel_Param *pParam)
{
	CString strFileName ="";
	CString strLog="";
	strFileName.Format("telnet_connect_%d",hSocket);

	
	int		n = 0;
	char	buffer[4096] = {0};
	char	*scan = NULL;
	int		telnetprogress = 0;
	int		count = 0;
	memset(pgd->databuffer, 0, BUF_SIZE);
	pgd->datalen = 0;
    pParam->state = state_data;
	pParam->verb= verb_sb;
	pParam->DataProc = terminal[(pgd->term_index==NUM_TERMINALS)?(NUM_TERMINALS-1):pgd->term_index].termproc;
	int nTime=0;
	while(1)
	{
LOOP:
		memset(buffer, 0, 4096);
		if(RESPONSE_WAIT(hSocket, READ_TIMEOUT) <= 0)
		{
			if((telnetprogress==2)&&(nTime<3))
			{
				char buf[10]={0};
				sprintf(buf,"\r\n");
				send(hSocket, buf, strlen(buf),0);
				nTime++;
				goto LOOP;
			}

			return -1;
		} 
		Sleep(500);
		n = recv(hSocket, buffer, 4095, 0);

		cout << "buffer:"<< buffer << endl;
		
		if(n == SOCKET_ERROR)
		{
			int wserror;
			wserror=WSAGetLastError();
			CString stmp;
			stmp.Format("telnet connect failed --error code =%d",wserror);

			return -2;
		}

		char szTemp[128] = {0};
			
		buffer[n] = 0;
		scan = buffer;
		while(n--)
		{
			sprintf( szTemp, "data=%c, state=%d", *scan, pParam->state );

			telnet_protocol(hSocket, *scan++,pgd, pParam);
		}

		switch(telnetprogress)
		{
		case 0:
            if(logprompt)
            {
			    if(strstr(pgd->databuffer, logprompt))
			    {
				    
					char	bufUser[255] = {0};
				    strcpy(bufUser, uid);
					//---wangpeng
					for(int nS=0;nS<strlen(bufUser);nS++)
					{
						Sleep(20);
						send(hSocket, &bufUser[nS], 1,0);	
					}
					 send(hSocket,"\r\n",2,0);

//				    strcat(bufUser, "\r\n");
//				    Sleep(100);			// delay
//				    send(hSocket, bufUser, strlen(bufUser),0);
				    telnetprogress ++;
				    count = 0;
				    memset(pgd->databuffer, 0, BUF_SIZE);
				    pgd->datalen = 0;

					
			    }
            }
            else
            {
               
                return -7;
            }
			break;
		case 1:
            if(pwdprompt)
            {
			    if(strstr(pgd->databuffer, pwdprompt))
			    {
				    char	bufPwd[255] = {0};
				    strcpy(bufPwd, pwd);

					//---wangpeng
					for(int nS=0;nS<strlen(bufPwd);nS++)
					{
						Sleep(20);
						send(hSocket, &bufPwd[nS], 1,0);	
					}
					 send(hSocket,"\r\n",2,0);
					//---------结束------------------
//				    strcat(bufPwd, "\r\n");
//				    Sleep(100);			// delay
//				    send(hSocket, bufPwd, strlen(bufPwd), 0);
				    telnetprogress ++;
				    count = 0;
				    memset(pgd->databuffer, 0, BUF_SIZE);
				    pgd->datalen = 0;

					//WriteLog( strFileName,"密码匹配成功！" );
			    }
            }
            else
            {
               
                return -7;
            }
			break;
		case 2:
			if(strstr(pgd->databuffer, "ogin incorrect"))
			{
				memset(pgd->databuffer, 0, BUF_SIZE);
				pgd->datalen = 0;
				return -6;
			}
			telnet_setlinewidth(hSocket, 0x00, 0xef);
			
			int		i = 0;
			char	prompt[256];
			while(i < PR_NUM && *arprompt[i])
			{
				memset(prompt, 0, sizeof(prompt));
				strcpy(prompt, arprompt[i]);

			
				if(strstr(pgd->databuffer, prompt))
				{
//					puts(pgd->databuffer);
					memset(pgd->databuffer, 0, BUF_SIZE);
					pgd->datalen = 0;
					
					return 0;
				}
				i ++;
			}
			if(strstr(pgd->databuffer, "TERM = (ansi)"))
			{
				puts("TERM = (ansi)");
				char	TERM[255] = {0};
				strcpy(TERM, "\r\n");
				Sleep(100);			// delay
				send(hSocket, TERM, 2, 0);
				puts("TERM = (ansi)");
				memset(pgd->databuffer, 0, BUF_SIZE);
				pgd->datalen = 0;
			
			}
			break;
		}
		if(++count > 80)
		{
			if(telnetprogress == 0)
				return -3;
			if(telnetprogress == 1) 
				return -4;
			if(telnetprogress == 2) 
				return -5;
		}
        //改变默认的telnet行的长度
//        telnet_setlinewidth(hSocket, 0x00, 0xef);
	}
}

int telnet_command(SOCKET hSocket, char *cmd,LPGDATA pgd, 
                     struct Tel_Param *pParam)
{
	int		n = 0;
	char	buffer[102400] = {0};
	char	*scan = NULL;
	char	*ca = NULL, *cb = NULL;

	sprintf(buffer, "echo %s;%s;echo %s\r\n", C_STA, cmd, C_END);

	send(hSocket, buffer, strlen(buffer), 0);

	memset(pgd->databuffer, 0, BUF_SIZE);
	pgd->datalen = 0;
	while(RESPONSE_WAIT(hSocket, READ_TIMEOUT))
	{
        memset(buffer, 0 , 102400);
		n = recv(hSocket, buffer, 102400, 0);
		if(n == SOCKET_ERROR) 
		{
			printf("SOCKET_ERROR");
			return -2;
		}
		if(n == 0) 
		{
			printf("n=0");
			return -2;
		}
		buffer[n] = 0;
		scan = buffer;

		printf("----------------------------");
		printf("buffer:%s",buffer);
		printf("----------------------------");

		while(n--) telnet_protocol(hSocket, *scan++,pgd, pParam);

		printf("----------------------------");
		printf("bufferParser:%s",pgd->databuffer);
		printf("----------------------------");

		char match_1[20] = {0}, match_2[20] = {0};
		sprintf(match_1, "\r\n%s", C_STA);
		sprintf(match_2, "\r\n%s", C_END);
		if(ca = strstr(pgd->databuffer, match_1)) 
		{
			if(cb = strstr(pgd->databuffer, match_2))
			{
				ca += strlen(C_STA) + 4;
				cb += 2;
				strncpy(pgd->databuffer, ca, cb - ca);
				pgd->databuffer[cb - ca] = 0;
				break;
			}
		}
	}
	
	return strlen(pgd->databuffer);
}

void telnet_protocol(SOCKET server,unsigned char code,LPGDATA pgd, 
                     struct Tel_Param *pParam)
{
	//Decide what to do (state based)
	switch(pParam->state)
	{
	case state_data:
		switch(code)
		{
		case IAC: pParam->state = state_code; break;
		default:
            pParam->DataProc(server,code,pgd);          
		}
		break;
	case state_code:
		pParam->state = state_data;
		switch(code)
		{
		// State transition back to data
		case IAC: 
			pParam->DataProc(server,code,pgd);
			break;
		// Code state transitions back to data
		case SE:
			pParam->DataProc = terminal[(pgd->term_index==NUM_TERMINALS)?(NUM_TERMINALS-1):pgd->term_index].termproc;
			break;
		case NOP:
			break;
		case DM:
			break;
		case BRK:
			break;
		case IP:
			break;
		case AO:
			break;
		case AYT:
			break;
		case EC:
			break;
		case EL:
			break;
		case GA:
			break;
		// Transitions to option state
		case SB:
			pParam->verb = verb_sb;
			pParam->state = state_option;
			break;
		case WILL:
			pParam->verb = verb_will;
			pParam->state = state_option;
			break;
		case WONT:
			pParam->verb = verb_wont;
			pParam->state = state_option;
			break;
		case DO:
			pParam->verb = verb_do;
			pParam->state = state_option;
			break;
		case DONT:
			pParam->verb = verb_dont;
			pParam->state = state_option;
		    break;
		}
		break;
	case state_option:
		pParam->state = state_data;

		//Find the option entry
		for(
			int i = 0;
			ol[i].option != TOPT_ERROR && ol[i].option != code;
			i++)

		//Do some verb specific stuff
		if(pParam->verb == verb_sb)
			pParam->DataProc = ol[i].DataProc;
		else
			ol[i].OptionProc(server, pParam->verb, (_option)code);
		break;
	}
}

void telnet_protocol(SOCKET server,unsigned char code,LPGDATA pgd)
{
	//These vars are the finite state
	static int state = state_data;
	static _verb verb = verb_sb;
	static LPDATAPROC DataProc = terminal[(pgd->term_index==NUM_TERMINALS)?(NUM_TERMINALS-1):pgd->term_index].termproc;

    int nType = 0;
	//Decide what to do (state based)
	switch(state)
	{
	case state_data:
		switch(code)
		{
		case IAC: state = state_code; break;
		default:
            DataProc(server,code,pgd);        
		}
		break;
	case state_code:
		state = state_data;
		switch(code)
		{
		// State transition back to data
		case IAC: 
			DataProc(server,code,pgd);
			break;
		// Code state transitions back to data
		case SE:
			DataProc = terminal[(pgd->term_index==NUM_TERMINALS)?(NUM_TERMINALS-1):pgd->term_index].termproc;
			break;
		case NOP:
			break;
		case DM:
			break;
		case BRK:
			break;
		case IP:
			break;
		case AO:
			break;
		case AYT:
			break;
		case EC:
			break;
		case EL:
			break;
		case GA:
			break;
		// Transitions to option state
		case SB:
			verb = verb_sb;
			state = state_option;
			break;
		case WILL:
			verb = verb_will;
			state = state_option;
			break;
		case WONT:
			verb = verb_wont;
			state = state_option;
			break;
		case DO:
			verb = verb_do;
			state = state_option;
			break;
		case DONT:
			verb = verb_dont;
			state = state_option;
		break;
		}
		break;
	case state_option:
		state = state_data;

		//Find the option entry
		for(
			int i = 0;
			ol[i].option != TOPT_ERROR && ol[i].option != code;
			i++)

		//Do some verb specific stuff
		if(verb == verb_sb)
			DataProc = ol[i].DataProc;
		else
			ol[i].OptionProc(server,verb,(_option)code);
		break;
	}
}

int telnet_command(SOCKET hSocket, char *cmd,LPGDATA pgd)
{
	int		n = 0;
	char	buffer[4096] = {0};
	char	*scan = NULL;
	char	*ca = NULL, *cb = NULL;

	sprintf(buffer, "echo %s;%s;echo %s\r\n", C_STA, cmd, C_END);

    if(strlen(buffer) >= 4096)
    {
        char chMsg[256];
        sprintf(chMsg, "Command length is:%d", strlen(buffer));
        printf(chMsg);
    }
	send(hSocket, buffer, strlen(buffer), 0);

	memset(pgd->databuffer, 0, BUF_SIZE);
	pgd->datalen = 0;
	while(RESPONSE_WAIT(hSocket, READ_TIMEOUT))
	{
        memset(buffer, 0 , 4096);
		n = recv(hSocket, buffer, 4095, 0);
		if(n == SOCKET_ERROR) return -2;
		if(n == 0) return -2;
		buffer[n] = 0;
		scan = buffer;

		while(n--) telnet_protocol(hSocket, *scan++,pgd);

		char match_1[20] = {0}, match_2[20] = {0};
		sprintf(match_1, "\r\n%s", C_STA);
		sprintf(match_2, "%s", C_END);
		if(ca = strstr(pgd->databuffer, match_1)) 
		{
			if(cb = strstr(ca, match_2))
			{
				ca += strlen(C_STA) + 4;
//				puts(ca);
				cb += 2;
//				int a=ca-cb;
//				printf("===========%d\n",a);
				strncpy(pgd->databuffer, ca, cb - ca);
				pgd->databuffer[cb - ca] = 0;
				break;
			}
		}
	}
	return strlen(pgd->databuffer);
}

void noreply(SOCKET server, _verb verb, _option option)
{
	unsigned char buf[3];
	buf[0] = IAC;
	buf[1] = (verb==verb_do)?WONT:(verb==verb_dont)?WILL:(verb==verb_will)?DONT:DO;
	buf[2] = (unsigned char)option;
	send(server, (char*)buf, 3, 0);
}

void yesreply(SOCKET server, _verb verb, _option option)
{
	unsigned char buf[3];
	buf[0] = IAC;
	buf[1] = (verb==verb_do)?WILL:(verb==verb_dont)?WONT:(verb==verb_will)?DO:DONT;
	buf[2] = (unsigned char)option;
	send(server, (char*)buf, 3, 0);
}

void ddww_echo(SOCKET server, _verb verb, _option option)
{
	switch(verb)
	{
	case verb_will: // server wants to echo stuff
		break;
	case verb_wont: // server don't want to echo
		break;
	case verb_do:   // server wants me to loopback
		noreply(server,verb,option);
		return;
	case verb_dont: // server doesn't want me to echo
		break;        // don't bother to reply - I don't
	}

	yesreply(server,verb,option);
}

void ddww_supp(SOCKET server, _verb verb, _option option)
{
	switch(verb)
	{
	case verb_will: // server wants to suppress GA's
		break;
	case verb_wont: // server wants to send GA's 
		break;
	case verb_do:   // server wants me to suppress GA's
		break;
	case verb_dont: // server wants me to send GA's
		break;
	}
}

void ddww_term(SOCKET server, _verb verb, _option option)
{
	switch(verb)
	{
	case verb_will:
		noreply(server,verb,option); // I don't want terminal info
		break;
	case verb_wont:
		//dat be cool - its not going to send. no need to confirm
		break;
	case verb_do:
		yesreply(server,verb,option); //I'll send it when asked
		break;
	case verb_dont://Ok - I won't
		break;
	}
}

void sbproc_term(SOCKET server, unsigned char data,LPGDATA pgd)
{
	if(data == SB_TERM_SEND)
	{
		if(pgd->term_index == NUM_TERMINALS)
			pgd->term_index = 0;
		else
			pgd->term_index++;
		char buf[16]; //pls limit 
		buf[0] = (char)IAC;
		buf[1] = (char)SB;
		buf[2] = (char)TOPT_TERM;
		buf[3] = (char)SB_TERM_IS;
		lstrcpy(&buf[4],terminal[(pgd->term_index==NUM_TERMINALS)?(NUM_TERMINALS-1):pgd->term_index].name);
		int nlen = lstrlen(&buf[4]);
		buf[4+nlen] = (char)IAC;
		buf[5+nlen] = (char)SE;
		send(server,buf,4+nlen+2,0);
	}
}

void ddww_error(SOCKET server, _verb verb, _option option)
{
	switch(verb)
	{
	case verb_will: 
		noreply(server,verb,option);
		break;
	case verb_wont:
		return;
	case verb_do:
		noreply(server,verb,option);
		break;
	case verb_dont:
		return;
	}
}

void nvt(SOCKET server, unsigned char data,LPGDATA pgd)
{
	switch(data)
	{
	case 0:  //eat null codes.
		break;
	default: //Send all else to the console.
		//WriteConsole(stdout1,&data,1,&z,NULL);
		pgd->databuffer[pgd->datalen++] = data;
		break;
	}
}

void ansi(SOCKET server,unsigned char data,LPGDATA pgd)
{
/*	
	static _ansi_state state = as_normal;

	char szTemp[100] = {0};
	//sprintf( szTemp, "ansi: data=%c, state=%d", data, state );
	//WriteLog( szTemp );

	switch( state)
	{
	case as_normal:
		switch(data)
		{
		case 0:  //eat null codes.
			break;
		case 27: //ANSI esc.
			state = as_esc;
			break;
		default: //Send all else to the console.
			pgd->databuffer[pgd->datalen++] = data;
		break;
		}
		break;
	case as_esc:
		state = as_esc1;
		pgd->codeptr=0;
		pgd->codebuf[pgd->codeptr] = 0;
		break;
	case as_esc1:
		if( data > 34 )
		{
			pgd->codebuf[pgd->codeptr] = 0;
			for(int i=0; codec[i].cmd && codec[i].cmd != data; i++);
			if(codec[i].proc)
				codec[i].proc(pgd->codebuf);
#ifdef _DEBUG
			else
			{
				char buf[256];
				wsprintf(buf,"Unknown Ansi code:'%c' (%s)\n",data,pgd->codebuf);
				//OutputDebugString(buf);
			}
#endif
			state = as_normal;
		}
		else
			pgd->codebuf[pgd->codeptr++] = data;
		break;
	}
*/
}

void ansi_set_screen_attribute(char* buffer)
{
/*
  while(*buffer)
  {
    switch(*buffer++)
    {
    case '0': //Normal
      sa = FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_BLUE;
      break;
    case '1': //Hign Intensity
      sa |= FOREGROUND_INTENSITY;
      break;
    case '4': //Underscore
      break;
    case '5': //Blink.
      sa |= BACKGROUND_INTENSITY;
      break;
    case '7':
      sa = BACKGROUND_RED | BACKGROUND_GREEN | BACKGROUND_BLUE;
      break;
    case '8':
      sa = 0;
      break;
    case '3':
      sa = sa & (BACKGROUND_BLUE | BACKGROUND_GREEN | BACKGROUND_RED | BACKGROUND_INTENSITY) |
        (*buffer & 1)?FOREGROUND_RED:0 |
        (*buffer & 2)?FOREGROUND_GREEN:0 |
        (*buffer & 4)?FOREGROUND_BLUE:0;
      if(*buffer)
        buffer++;
      break;
    case '6':
      sa = sa & (FOREGROUND_BLUE | FOREGROUND_GREEN | FOREGROUND_RED | FOREGROUND_INTENSITY) |
        (*buffer & 1)?BACKGROUND_RED:0 |
        (*buffer & 2)?BACKGROUND_GREEN:0 |
        (*buffer & 4)?BACKGROUND_BLUE:0;
      if(*buffer)
        buffer++;
      break;
    }
    if(*buffer && *buffer == ';')
      buffer++;
  }
  SetConsoleTextAttribute(stdout1,sa);
*/
}

void ansi_erase_line(char* buffer)
{
/*
  int act = 0;
  while(*buffer)
  {
    act = (*buffer++) - '0';
  }

  CONSOLE_SCREEN_BUFFER_INFO csbi;
  GetConsoleScreenBufferInfo(stdout1,&csbi);

  COORD pos;
  DWORD n;

  switch(act)
  {
  case 0: //erase to end of line
    pos.X = csbi.dwCursorPosition.X;
    pos.Y = csbi.dwCursorPosition.Y;
    n = csbi.dwSize.X - csbi.dwCursorPosition.X;
    break;
  case 1: //erase from beginning
    pos.X = 0;
    pos.Y = csbi.dwCursorPosition.Y;
    n = csbi.dwCursorPosition.X;
    break;
  case 2: // erase whole line
    pos.X = 0;
    pos.Y = csbi.dwCursorPosition.Y;
    n = csbi.dwSize.X;
    break;
  }

  DWORD w;
  FillConsoleOutputCharacter(stdout1,' ',n,pos,&w);
*/
}

void ansi_set_position(char* buffer)
{
/*
  COORD pos = {0,0};

  // Grab line
  while(*buffer && *buffer != ';')
    pos.Y = pos.Y*10 + *buffer++ - '0';

  if(*buffer)
    buffer++;

  // Grab y
  while(*buffer && *buffer != ';')
    pos.X = pos.X*10 + *buffer++ - '0';

  (pos.X)?pos.X--:0;
  (pos.Y)?pos.Y--:0;

  SetConsoleCursorPosition(stdout1,pos);
*/ 
}

void ansi_erase_screen(char* buffer)
{
/*
  int act = 0;
  while(*buffer)
  {
    act = (*buffer++) - '0';
  }

  CONSOLE_SCREEN_BUFFER_INFO csbi;
  GetConsoleScreenBufferInfo(stdout1,&csbi);

  COORD pos;
  DWORD n;

  switch(act)
  {
  case 0:
    pos.X = csbi.dwCursorPosition.X;
    pos.Y = csbi.dwCursorPosition.Y;
    n = csbi.dwSize.X*csbi.dwSize.Y;
    break;
  case 2:
    pos.X = 0;
    pos.Y = 0;
    n = csbi.dwSize.X*csbi.dwSize.Y;
    break;
  }

  DWORD w;
  FillConsoleOutputCharacter(stdout1,' ',n,pos,&w);
  SetConsoleCursorPosition(stdout1,pos);
*/
}

void ansi_move_up(char* buffer)
{
/*
  int cnt = *buffer?0:1;
  while(*buffer)
  {
    cnt = cnt*10 + (*buffer++) - '0';
  }

  COORD pos;

  CONSOLE_SCREEN_BUFFER_INFO csbi;
  GetConsoleScreenBufferInfo(stdout1,&csbi);

  pos.X = csbi.dwCursorPosition.X;
  pos.Y = ((csbi.dwCursorPosition.Y-cnt)>=0)?(csbi.dwCursorPosition.Y-cnt):0;

  SetConsoleCursorPosition(stdout1,pos);
*/
}


int connect_timeo(int sockfd,const struct sockaddr *saptr,int salen,int nsec)
{
	int n;
	int error;
	int len;
	fd_set rset,wset;
	struct timeval tv;

	u_long val = 1;
	ioctlsocket(sockfd,FIONBIO,&val);

	error = 0;

	if((n = connect(sockfd,saptr,salen)) < 0)
	{
		int nErrNum = WSAGetLastError();
		//		if(nErrNum == 10035)
		//			goto done;

		//		if(WSAGetLastError() != WSAEWOULDBLOCK) 
		//		if(nErrNum != WSAEWOULDBLOCK)
		if(nErrNum !=  10035)
		{	
			shutdown(sockfd, 0x02);
			closesocket(sockfd);
			return -1;
		}
	}	


	if(n == 0)
		goto done;

	FD_ZERO(&rset);
	FD_SET(sockfd,&rset);
	wset = rset;
	tv.tv_sec = nsec;
	tv.tv_usec = 0;
	printf("zzzzzzzzzz\r\n");
	if((n = select(sockfd + 1,&rset,&wset,NULL,nsec?&tv:NULL)) == 0)
	{
		shutdown(sockfd, 0x02);
		closesocket(sockfd);
		WSASetLastError(WSAETIMEDOUT);
		return -1;
	}
	printf("xxxxxxxxxxx\r\n");
	if(FD_ISSET(sockfd,&rset) || FD_ISSET(sockfd,&wset))
	{
		len = sizeof(error);
		if(getsockopt(sockfd,SOL_SOCKET,SO_ERROR,(char *)&error,&len) < 0)
		{
			shutdown(sockfd, 0x02);
			closesocket(sockfd);
			return -1;
		}
	}			
	else
	{
		//printf("select error: sockfd not set");
		shutdown(sockfd, 0x02);
		closesocket(sockfd);
		return -1;
	}		

done:
	val = 0;
	ioctlsocket(sockfd,FIONBIO,&val);
	if(error)
	{
		shutdown(sockfd, 0x02);
		closesocket(sockfd);
		WSASetLastError(error);
		return -1;
	}	
	return 0;
}

int RESPONSE_WAIT(SOCKET s, int nsec)
{
	struct timeval Timeout;
	fd_set readfds;

	readfds.fd_count = 1;
	readfds.fd_array[0] = s;
	Timeout.tv_sec = nsec;
	Timeout.tv_usec = 0;
	return(select(1, &readfds, NULL, NULL, &Timeout));
}


int telnet_socket(char *server, char *uid, char *pwd, int port)
{
	char *pserver = NULL; int pport = 0; char *puid = NULL; char *ppwd = NULL;
	char *lprom = "ogin:"; char *pprom = "assword:"; char *prom = "# $ >"; char *inifile = "AIX";
	int ngid = 0; char* strmid = NULL; char *custpath = NULL;

	int		ret = 0;
	int		hSocket = 0;
	bool	bResult = TRUE;
	char	cmdBuffer[256] = {0};

	int		c = 0;
	char	*ca = prom, *cb = NULL;
	char	arprompt[PR_NUM][256];
	memset(arprompt, 0, sizeof(arprompt));

	LPGDATA pgd=new GDATA;
	memset(pgd,0,sizeof(GDATA));
	while(cb = strchr(ca, 0x20))
	{
		if(ca == cb) 
		{
			ca ++;
			continue;
		}
		strncpy(arprompt[c++], ca, cb - ca);
		ca = cb + 1;
	}
	strcpy(arprompt[c++], ca);
	//------------------------------------

	hSocket = telnet_init(server, port, pserver, pport, puid, ppwd);
	if(hSocket <= 0)
	{
		printf("telnet_init error hSocket = %d\n", hSocket);
	}

	Tel_Param param;
	if((ret = telnet_connect(hSocket, uid, pwd, lprom, pprom, arprompt,pgd,&param)) < 0)
	{
		printf("telnet_connect error  = %d\n", ret);
	}

	delete pgd;
	return hSocket;
}

int telnet_command_long(SOCKET hSocket, char *cmd, char *szReturn)
{	
	int ret = 0;
	LPGDATA pgd=new GDATA;
	memset(pgd,0,sizeof(GDATA));

	if((ret = telnet_command(hSocket, cmd, pgd)) < 0)
	{
		printf("telnet_command error");
		return -1;
	}
	//printf("pgd->databuffer = %s\n", pgd->databuffer);

	strcpy(szReturn, pgd->databuffer);

	delete pgd;
	return 0;
}

void close_telnet(int hSocket)
{
	LPGDATA pgd=new GDATA;
	memset(pgd,0,sizeof(GDATA));

	telnet_command(hSocket,"exit",pgd);
	Sleep(500);

	delete pgd;
	shutdown(hSocket, 0x02);
	closesocket(hSocket);

	WSA_Free();
}

