#include <stdio.h>      /*标准输入输出定义*/
#include <stdlib.h>     /*标准函数库定义*/
#include <unistd.h>     /*Unix 标准函数定义*/
#include <sys/types.h>  
#include <sys/stat.h>   
#include <fcntl.h>      /*文件控制定义*/
#include <termios.h>    /*PPSIX 终端控制定义*/
#include <errno.h>      /*错误号定义*/

#include  <string.h>
#include   <iconv.h>
#include  <stdint.h>


#define MaxSMSLen 70 //最大短信长度

#define GSM_7BIT                        0
#define GSM_8BIT                        4
#define GSM_UCS2                      8

typedef unsigned long DWORD;



enum
    {
        OpenPortFailed = -1,//打开端口失败
        NoSetCenter = -2,//没有设置短信中心
        ShortSleepTime = 500
    };

struct MsgList
	{
		char chMsg[200];
		struct MsgList *pNext;
	};

    
struct SM_PARAM
	{
        char SCA[16];       // 短消息服务中心号码(SMSC地址)
        char TPA[16];       // 目标号码或回复号码(TP-DA或TP-RA)
        char TP_PID;        // 用户信息协议标识(TP-PID)
        char TP_DCS;        // 用户信息编码方式(TP-DCS)
        char TP_SCTS[16];   // 服务时间戳字符串(TP_SCTS), 接收时用到
        char TP_UD[161];    // 原始用户信息(编码前或解码后的TP-UD)
        char index;         // 短消息序号，在读取时用到
	};    


    
int  fd;
char* m_strMsgContent;
char* m_strRecvPhone;
char* m_SmsCenterNum;    
//可打印字符串转换为字节数据    
//int gsmString2Bytes(const char*, unsigned char*, int);  

//字节数据转换为可打印字符串    
//int gsmBytes2String(const unsigned char*, char*, int); 

//PDU解码函数，用于接收、阅读短消息      
//int gsmDecodePdu(const char*, SM_PARAM*);

//PDU编码函数，用于编制、发送短消息    
//int gsmEncodePdu(const SM_PARAM*, char*);
    
int open_SerialPort(const char *);

int initPort(const char *,int ,int ,int ,int );

void set_speed(int , int);

int set_Parity(int ,int ,int ,int);

int WritePort(int ,char *, int);

int ReadPort(int ,char *, int);

int gsmDeleteMessage(int ,const int );

int gsmString2Bytes(const char* , unsigned char* , int );

int gsmBytes2String(const unsigned char* , char* , int );

int gsmSerializeNumbers(const char* , char* , int );

int gsmInvertNumbers(const char* , char* , int );

int gsmDecodePdu(const char* , struct SM_PARAM* );

int gsmEncodePdu(const struct SM_PARAM* , char* );

int gsmDecode8bit(const unsigned char *, char *, int );

int gsmEncode8bit(const char *, unsigned char *, int );

int mygsmDecodeUcs2(char* , char* , int);

int mygsmEncodeUcs2(char* , char* , int );

int gsmDecode7bit(const unsigned char* , char* , int );

int gsmEncode7bit(const char* , unsigned char* , int );

int unicode2utf8(uint16_t *, int , uint8_t **);

int ucs2Count(const unsigned char *);

int gsmSendMessage(int ,const struct SM_PARAM* );

int send_msg(int ,char*  , char* , int);

void close_SerialPort(int);  

int test();
