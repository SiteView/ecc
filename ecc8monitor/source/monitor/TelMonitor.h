
#include "stdafx.h"

#define		NUL					0
#define		BEL					7
#define		BS					8
#define		HT					9
#define		LF					10
#define		VT					11
#define		FF					12
#define		CR					13
#define		SE					240
#define		NOP					241
#define		DM					242
#define		BRK					243
#define		IP					244
#define		AO					245
#define		AYT					246
#define		EC					247
#define		EL					248
#define		GA					249
#define		SB					250
#define		WILL				251
#define		WONT				252
#define		DO					253
#define		DONT				254
#define		IAC					255

#define		NUM_TERMINALS		2
#define		NUM_CODEC			6
#define		READ_TIMEOUT		20*3*3
//#define		CONNECT_TIMEOUT		20*3*3
#define		CONNECT_TIMEOUT		20

#define		PR_NUM				10
#define		C_STA				"DF-CMD-STA"
#define		C_END				"DF-CMD-END"




enum
{
  SB_TERM_IS = 0,
  SB_TERM_SEND = 1
};

enum _ansi_state
{
  as_normal,
  as_esc,
  as_esc1
};

enum _verb
{
  verb_sb   = 250,
  verb_will = 251,
  verb_wont = 252,
  verb_do   = 253, 
  verb_dont = 254
};

enum _state
{
  state_data,   //we expect a data byte
  state_code,   //we expect a code
  state_option  //we expect an option
};

enum _option
{
  TOPT_BIN = 0,   // Binary Transmission
  TOPT_ECHO = 1,  // Echo
  TOPT_RECN = 2,  // Reconnection
  TOPT_SUPP = 3,  // Suppress Go Ahead
  TOPT_APRX = 4,  // Approx Message Size Negotiation
  TOPT_STAT = 5,  // Status
  TOPT_TIM = 6,   // Timing Mark
  TOPT_REM = 7,   // Remote Controlled Trans and Echo
  TOPT_OLW = 8,   // Output Line Width
  TOPT_OPS = 9,   // Output Page Size
  TOPT_OCRD = 10, // Output Carriage-Return Disposition
  TOPT_OHT = 11,  // Output Horizontal Tabstops
  TOPT_OHTD = 12, // Output Horizontal Tab Disposition
  TOPT_OFD = 13,  // Output Formfeed Disposition
  TOPT_OVT = 14,  // Output Vertical Tabstops
  TOPT_OVTD = 15, // Output Vertical Tab Disposition
  TOPT_OLD = 16,  // Output Linefeed Disposition
  TOPT_EXT = 17,  // Extended ASCII
  TOPT_LOGO = 18, // Logout
  TOPT_BYTE = 19, // Byte Macro
  TOPT_DATA = 20, // Data Entry Terminal
  TOPT_SUP = 21,  // SUPDUP
  TOPT_SUPO = 22, // SUPDUP Output
  TOPT_SNDL = 23, // Send Location
  TOPT_TERM = 24, // Terminal Type
  TOPT_EOR = 25,  // End of Record
  TOPT_TACACS = 26, // TACACS User Identification
  TOPT_OM = 27,   // Output Marking
  TOPT_TLN = 28,  // Terminal Location Number
  TOPT_3270 = 29, // Telnet 3270 Regime
  TOPT_X3 = 30,  // X.3 PAD
  TOPT_NAWS = 31, // Negotiate About Window Size
  TOPT_TS = 32,   // Terminal Speed
  TOPT_RFC = 33,  // Remote Flow Control
  TOPT_LINE = 34, // Linemode
  TOPT_XDL = 35,  // X Display Location
  TOPT_ENVIR = 36,// Telnet Environment Option
  TOPT_AUTH = 37, // Telnet Authentication Option
  TOPT_NENVIR = 39,// Telnet Environment Option
  TOPT_EXTOP = 255, // Extended-Options-List
  TOPT_ERROR = 256  // Magic number
};

typedef void(*LPOPTIONPROC)(SOCKET, _verb, _option);
typedef void(*LPDATAPROC)(SOCKET, unsigned char data,LPGDATA pgd);
typedef void (*LPCODEPROC)(char*);

struct OL
{
  _option option;
  LPOPTIONPROC OptionProc;
  LPDATAPROC DataProc;
};

struct TERMINAL
{
  char* name;
  LPDATAPROC termproc;
  //pre requsites.
};

struct CODEC
{
  unsigned char cmd;
  LPCODEPROC proc;
};

void ddww_echo(SOCKET server, _verb verb, _option option);
void ddww_supp(SOCKET server, _verb verb, _option option);
void ddww_term(SOCKET server, _verb verb, _option option);
void sbproc_term(SOCKET server, unsigned char data,LPGDATA pgd);
void ddww_error(SOCKET server, _verb verb, _option option);

void nvt(SOCKET server, unsigned char data,LPGDATA pgd);
void ansi(SOCKET server, unsigned char data,LPGDATA pgd);

void ansi_set_screen_attribute(char* buffer);
void ansi_erase_line(char* buffer);
void ansi_set_position(char* buffer);
void ansi_erase_screen(char* buffer);
void ansi_move_up(char* buffer);


struct Tel_Param
{
    int state;
	_verb verb ;
	LPDATAPROC DataProc;
};


SOCKET telnet_init(char *server, int port, char *proxy_server, int proxy_port, char *proxy_uid, char *proxy_pwd);

void telnet_protocol(SOCKET server,unsigned char code,LPGDATA pgd, 
                     struct Tel_Param *pParam);
extern "C" __declspec(dllexport)
int telnet_connect(SOCKET hSocket, char *uid, char *pwd, 
				   char *logprompt, char *pwdprompt, char arprompt[PR_NUM][256],
                   LPGDATA pgd, struct Tel_Param *pParam);

int telnet_command(SOCKET hSocket, char *cmd,LPGDATA pgd, struct Tel_Param *pParam);


int telnet_connect(SOCKET hSocket, char *uid, char *pwd, 
				   char *logprompt, char *pwdprompt, char arprompt[PR_NUM][256],LPGDATA pgd);


int telnet_setlinewidth(SOCKET hSocket, BYTE HighByte, BYTE LowByte);
void telnet_protocol(SOCKET server,unsigned char code,LPGDATA pgd);
int telnet_command(SOCKET hSocket, char *cmd,LPGDATA pgd);

int telnet_socket(char *server, char *uid, char *pwd, int port);
int telnet_command_long(SOCKET hSocket, char *cmd, char *szReturn);
void close_telnet(int hSocket);

