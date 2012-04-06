//linux下串口短信猫操作代码
//使用到libiconv-1.13.1
//动态库入口GsmOperate_driver_output

#include "LinuxGsmOperate.h"

int test()
{
    int a;
    a = 100;
    return a;
}

/********************
*@brief     打开串口
*@param  DEVICE: 类型 int,  打开串口的文件句柄
*@return  fd: 类型 int, 串口句柄
********************/
int open_SerialPort(const char *DEVICE){
    int fd;
    fd = open(DEVICE,O_RDWR);
    if (-1==fd){
        /*不能打开串口*/
        perror("can not open this serial port!");        
    }
    return fd;    
}

/**********************
*@brief     关闭串口
***********************/
void close_SerialPort(int fd)
{
    close(fd);
}

/******************
*@brief  初始化串口
*@param fd 类型 int  打开串口的文件句柄
*@param  speed  类型 int, 串口速度
*@param databits 类型 int,  数据位   取值 为 7 或者8
*@param stopbits 类型 int,  停止位   取值为 1 或者2
*@param parity 类型 int,  效验类型 取值为N,E,O,,S
*@return  int
******************/
int initPort(const char *DEVICE,int speed,int databits,int stopbits,int parity){
    int fd;
    int pt;
    fd = open_SerialPort(DEVICE);
    if (-1 == fd){
        perror("can not open this serial port!");
        exit(-1);        
    }
    set_speed(fd,speed);
    pt = set_Parity(fd, databits,stopbits,parity);
    return pt;    
}

/*******************
*@brief  设置串口通信速率
*@param  fd     类型 int  打开串口的文件句柄
*@param  speed  类型 int  串口速度
*@return  void
********************/
int speed_arr[] = { B38400, B19200, B9600, B4800, B2400, B1200, B300,
					B38400, B19200, B9600, B4800, B2400, B1200, B300,};
int name_arr[] = {38400,  19200,  9600,  4800,  2400,  1200,  300, 38400,  
					19200,  9600, 4800, 2400, 1200,  300,};
void set_speed(int fd, int speed){
	int   i; 
	int   status; 
	struct termios   Opt;
	tcgetattr(fd, &Opt); 
	for ( i= 0;  i < sizeof(speed_arr) / sizeof(int);  i++) { 
		if  (speed == name_arr[i]) {     
			tcflush(fd, TCIOFLUSH);     
			cfsetispeed(&Opt, speed_arr[i]);  
			cfsetospeed(&Opt, speed_arr[i]);   
			status = tcsetattr(fd, TCSANOW, &Opt);  
			if  (status != 0) {        
				perror("tcsetattr fd");  
				return;     
			}    
			tcflush(fd,TCIOFLUSH);   
		}  
	}
}


/**********************
*@brief   设置串口数据位，停止位和效验位
*@param  fd     类型  int  打开的串口文件句柄
*@param  databits 类型  int 数据位   取值 为 7 或者8
*@param  stopbits 类型  int 停止位   取值为 1 或者2
*@param  parity  类型  int  效验类型 取值为N,E,O,,S
***********************/
int set_Parity(int fd,int databits,int stopbits,int parity)
{ 
	struct termios options; 
	if  ( tcgetattr( fd,&options)  !=  0) { 
		perror("SetupSerial 1");     
		return(-1);  
	}
	options.c_cflag &= ~CSIZE; 
	switch (databits) /*设置数据位数*/
	{   
	case 7:		
		options.c_cflag |= CS7; 
		break;
	case 8:     
		options.c_cflag |= CS8;
		break;   
	default:    
		fprintf(stderr,"Unsupported data size\n"); 
        return (-1);  
	}
switch (parity) 
{   
	case 'n':
	case 'N':    
		options.c_cflag &= ~PARENB;   /* Clear parity enable */
		options.c_iflag &= ~INPCK;     /* Enable parity checking */ 
		break;  
	case 'o':   
	case 'O':     
		options.c_cflag |= (PARODD | PARENB); /* 设置为奇效验*/  
		options.c_iflag |= INPCK;             /* Disnable parity checking */ 
		break;  
	case 'e':  
	case 'E':   
		options.c_cflag |= PARENB;     /* Enable parity */    
		options.c_cflag &= ~PARODD;   /* 转换为偶效验*/     
		options.c_iflag |= INPCK;       /* Disnable parity checking */
		break;
	case 'S': 
	case 's':  /*as no parity*/   
	    options.c_cflag &= ~PARENB;
		options.c_cflag &= ~CSTOPB;break;  
	default:   
		fprintf(stderr,"Unsupported parity\n");    
		return (-1);  
	}  
/* 设置停止位*/  
switch (stopbits)
{   
	case 1:    
		options.c_cflag &= ~CSTOPB;  
		break;  
	case 2:    
		options.c_cflag |= CSTOPB;  
	   break;
	default:    
		 fprintf(stderr,"Unsupported stop bits\n");  
		 return (-1); 
} 
/* Set input parity option */ 
if (parity != 'n')   
	options.c_iflag |= INPCK; 
tcflush(fd,TCIFLUSH);
options.c_cc[VTIME] = 150; /* 设置超时15 seconds*/   
options.c_cc[VMIN] = 0; /* Update the options and do it NOW */
if (tcsetattr(fd,TCSANOW,&options) != 0)   
{ 
	perror("SetupSerial 3");   
	return (-1);  
} 
return (0);  
}


/*
*@brief   写串口。
*@param   fd:串口文件句柄。
*@param   pData: 待写的数据缓冲区指针。
*@param  nLength: 待写的数据长度。
*@return  int。 
*/
int WritePort(int fd,char *pData, int nLength)
{   
    return write(fd,pData,nLength);
}

/*
*@brief     读串口。
*@param   fd:串口文件句柄。
*@param   pData: 待读的数据缓冲区指针。
*@param   nLength: 待读的最大数据长度。
*@return   实际读入的数据长度。 
*/
int ReadPort(int fd,char *pData, int nLength)
{  
    char buff[nLength];
    int nread = read(fd,buff,nLength);    
    return nread;
}

/* 
*@brief     可打印字符串转换为字节数据,如："C8329BFD0E01" --> {0xC8, 0x32, 0x9B, 0xFD, 0x0E, 0x01}                        

                                            
*@param   pSrc: 源字符串指针                                                    
*@param  pDst: 目标数据指针
*@param  nSrcLength: 源字符串长度   
*@return  目标数据长度    
*/
int gsmString2Bytes(const char* pSrc, unsigned char* pDst, int nSrcLength)
{
    for(int i=0; i<nSrcLength; i+=2)
    {
        // 输出高4位
        if(*pSrc>='0' && *pSrc<='9')
        {
            *pDst = (*pSrc - '0') << 4;
		}
        else
        {
            *pDst = (*pSrc - 'A' + 10) << 4;
        }
    
        pSrc++;
        // 输出低4位
        if(*pSrc>='0' && *pSrc<='9')
        {
            *pDst |= *pSrc - '0';
        }
        else
		{
            *pDst |= *pSrc - 'A' + 10;
        }
        pSrc++;
        pDst++;
    }   
    // 返回目标数据长度
    return nSrcLength / 2;
}

/* 
*@brief     字节数据转换为可打印字符串,如：如：{0xC8, 0x32, 0x9B, 0xFD, 0x0E, 0x01} --> "C8329BFD0E01"                    

                                                
*@param   pSrc: 源数据指针                                                    
*@param   pDst: 目标字符串指针
*@param  nSrcLength: 源数据长度   
*@return  目标字符串长度     
*/
int gsmBytes2String(const unsigned char* pSrc, char* pDst, int nSrcLength)
{
    const char tab[]="0123456789ABCDEF";    // 0x0-0xf的字符查找表    
    for(int i=0; i<nSrcLength; i++)
    {
		// 输出低4位
        *pDst++ = tab[*pSrc >> 4];                                        
        // 输出高4位
        *pDst++ = tab[*pSrc & 0x0f];    
        pSrc++;
	}    
    // 输出字符串加个结束符
    *pDst = '\0';    
    // 返回目标字符串长度
    return nSrcLength * 2;
}


/*
* 函数：gsmSerializeNumbers                                                   
*说明：两两颠倒的字符串转换为正常顺序的字符串                               
*       如："8613910199192" --> "683119109991F2"                              
* 参数：                                                                      
*      pSrc: 源字符串指针                                                     
*      pDst: 目标字符串指针                                                   
*      nSrcLength: 源字符串长度                                              
* 返回: 目标字符串长度                                                       
*/
int gsmSerializeNumbers(const char* pSrc, char* pDst, int nSrcLength)
{
    int nDstLength;   // 目标字符串长度
    char ch;          // 用于保存一个字符
    
    // 复制串长度
    nDstLength = nSrcLength;
	  // 两两颠倒
    for(int i=0; i<nSrcLength;i+=2)
    {
        ch = *pSrc++;        // 保存先出现的字符
        *pDst++ = *pSrc++;   // 复制后出现的字符
        *pDst++ = ch;        // 复制先出现的字符
    }
    
    // 最后的字符是'F'吗？
    if(*(pDst-1) == 'F')
    {
        pDst--;
        nDstLength--;        // 目标字符串长度减1
    }
    
    // 输出字符串加个结束符
    *pDst = '\0';
    
    // 返回目标字符串长度
    return nDstLength;
}

/*
* 函数：gsmInvertNumbers                                                     
* 说明：PDU串中的号码和时间，都是两两颠倒的字符串。利用下面两个函数可进行正反 
*      变换：正常顺序的字符串转换为两两颠倒的字符串，若长度为奇数，补'F'凑成偶
*      数，如："8613910199192" --> "683119109991F2"                           
* 参数：                                                                      
*      pSrc: 源字符串指针                                                     
*      pDst: 目标字符串指针                                                   
*      nSrcLength: 源字符串长度                                               
* 返回: 目标字符串长度                                                       
*/
int gsmInvertNumbers(const char* pSrc, char* pDst, int nSrcLength)
{
    int nDstLength;   // 目标字符串长度
    char ch;          // 用于保存一个字符
    
    // 复制串长度
    nDstLength = nSrcLength;
    
    // 两两颠倒
    for(int i=0; i<nSrcLength;i+=2)
    {
        ch = *pSrc++;        // 保存先出现的字符
        *pDst++ = *pSrc++;   // 复制后出现的字符
        *pDst++ = ch;        // 复制先出现的字符
    }
    
    // 源串长度是奇数吗？
    if(nSrcLength & 1)
   {
        *(pDst-2) = 'F';     // 补'F'
        nDstLength++;        // 目标串长度加1
    }
    
    // 输出字符串加个结束符
    *pDst = '\0';
    
    // 返回目标字符串长度
    return nDstLength;
}

/* 
*@brief     8-bit解码                                                                   
*@param  pSrc: 源编码串指针                                                
*@param  pDst: 目标字符串指针
*@param  nSrcLength: 源编码串长度 
*@return  目标字符串长度
*/
int gsmDecode8bit(const unsigned char *pSrc, char *pDst, int nSrcLength)
{
    // 简单复制
	memcpy(pDst, pSrc, nSrcLength);
	// 输出字符串加个结束符
	*pDst = '\0';
	return nSrcLength;
}

/* 
*@brief     8-bit编码                                                                   
*@param  pSrc: 源编码串指针                                                
*@param  pDst: 目标字符串指针
*@param  nSrcLength: 源编码串长度  
*@return  目标字符串长度   
*/
int gsmEncode8bit(const char *pSrc, unsigned char *pDst, int nSrcLength)
{
    // 简单复制
	memcpy(pDst, pSrc, nSrcLength);
	return nSrcLength;
}

/* 
*@brief     7-bit解码                                                                   
*@param  pSrc: 源编码串指针                                                
*@param  pDst: 目标字符串指针
*@param  nSrcLength: 源编码串长度  
*@return  目标字符串长度   
*/
int gsmDecode7bit(const unsigned char* pSrc, char* pDst, int nSrcLength)
{
    int nSrc;        // 源字符串的计数值
    int nDst;        // 目标解码串的计数值
    int nByte;       // 当前正在处理的组内字节的序号，范围是0-6
    unsigned char nLeft;    // 上一字节残余的数据    
    // 计数值初始化
    nSrc = 0;
    nDst = 0;    
    // 组内字节序号和残余数据初始化
    nByte = 0;
    nLeft = 0;    
    // 将源数据每7个字节分为一组，解压缩成8个字节
    // 循环该处理过程，直至源数据被处理完
    // 如果分组不到7字节，也能正确处理
    while(nSrc<nSrcLength)
    {
        // 将源字节右边部分与残余数据相加，去掉最高位，得到一个目标解码字节
        *pDst = ((*pSrc << nByte) | nLeft) & 0x7f;
        // 将该字节剩下的左边部分，作为残余数据保存起来
        nLeft = *pSrc >> (7-nByte);    
        // 修改目标串的指针和计数值
        pDst++;
        nDst++;
        // 修改字节计数值
        nByte++;    
        // 到了一组的最后一个字节
        if(nByte == 7)
        {
            // 额外得到一个目标解码字节
            *pDst = nLeft;    
            // 修改目标串的指针和计数值
            pDst++;
            nDst++;    
            // 组内字节序号和残余数据初始化
            nByte = 0;
            nLeft = 0;
        }    
        // 修改源串的指针和计数值
        pSrc++;
        nSrc++;
	}
    
    *pDst = 0;    
    // 返回目标串长度
    return nDst;
}

/* 
*@brief     7bit编码                                                                   
*@param  pSrc: 源字符串指针                                                
*@param  pDst: 目标编码串指针
*@param  nSrcLength: 源字符串长度  
*@return  目标编码串长度
*/
int gsmEncode7bit(const char* pSrc, unsigned char* pDst, int nSrcLength)
{
    int nSrc;        // 源字符串的计数值
    int nDst;        // 目标编码串的计数值
    int nChar;       // 当前正在处理的组内字符字节的序号，范围是0-7
    unsigned char nLeft;    // 上一字节残余的数据
    
    // 计数值初始化
    nSrc = 0;
    nDst = 0;
    
    // 将源串每8个字节分为一组，压缩成7个字节
    // 循环该处理过程，直至源串被处理完
    // 如果分组不到8字节，也能正确处理
    while(nSrc<nSrcLength)
    {
        // 取源字符串的计数值的最低3位
        nChar = nSrc & 7;
		  // 处理源串的每个字节
        if(nChar == 0)
        {
            // 组内第一个字节，只是保存起来，待处理下一个字节时使用
            nLeft = *pSrc;
        }
        else
        {
            // 组内其它字节，将其右边部分与残余数据相加，得到一个目标编码字节
            *pDst = (*pSrc << (8-nChar)) | nLeft;
      // 将该字节剩下的左边部分，作为残余数据保存起来
            nLeft = *pSrc >> nChar;
            // 修改目标串的指针和计数值 pDst++;
            nDst++; 
        } 
        
        // 修改源串的指针和计数值
        pSrc++; nSrc++;
    }
    
    // 返回目标串长度
    return nDst; 
}

/*
*@brief Unicode转UTF-8
*@param  in: 源unicode字符指针                                                
*@param  out: 目标字符串指针的指针
*@param  insize: 源数据长度                                                   
*@return  0
*/
int unicode2utf8(uint16_t *in, int insize, uint8_t **out)
{
    int i = 0;
    int outsize = 0;
    int charscount = 0;
    uint8_t *result = NULL;
    uint8_t *tmp = NULL;

    charscount = insize / sizeof(uint16_t);
    result = (uint8_t *)malloc(charscount * 3 + 1);
    memset(result, 0, charscount * 3 + 1);
    tmp = result;
    for (i = 0; i < charscount; i++)
    {
        uint16_t unicode = in[i];        
        if (unicode >= 0x0000 && unicode <= 0x007f)
        {
            *tmp = (uint8_t)unicode;
            tmp += 1;
            outsize += 1;
        }
        else if (unicode >= 0x0080 && unicode <= 0x07ff)
        {
            *tmp = 0xc0 | (unicode >> 6);
            tmp += 1;
            *tmp = 0x80 | (unicode & (0xff >> 2));
            tmp += 1;
            outsize += 2;
        }
        else if (unicode >= 0x0800 && unicode <= 0xffff)
        {
            *tmp = 0xe0 | (unicode >> 12);
            tmp += 1;
            *tmp = 0x80 | (unicode >> 6 & 0x00ff);
            tmp += 1;
            *tmp = 0x80 | (unicode & (0xff >> 2));
            tmp += 1;
            outsize += 3;
        }

    }

    *tmp = '\0';
    *out = result;
    return 0;
}

// 计算出UCS2编码的长度并返回长度
int ucs2Count(const unsigned char *pSrc)
{
    int i;
    for (i = 0; pSrc[i]+pSrc[i+1] != 0; i += 2);
    //注意上面有个";"
    return i;
}

/* 
*@brief PDU解码，用于接收、阅读短消息                                                                   
*@param  pSrc: 源PDU串指针                                                
*@param  pDst: 目标PDU参数指针
*@return  用户信息串长度   
*/

int gsmDecodePdu(const char* pSrc, struct SM_PARAM* pDst)
{
    int nDstLength;          // 目标PDU串长度
    unsigned char tmp;       // 内部用的临时字节变量
    unsigned char buf[256];  // 内部用的缓冲区

    // SMSC地址信息段
    gsmString2Bytes(pSrc, &tmp, 2);    // 取长度
    tmp = (tmp - 1) * 2;    // SMSC号码串长度
    pSrc += 4;              // 指针后移
    gsmSerializeNumbers(pSrc, (*pDst).SCA, tmp);    // 转换SMSC号码到目标PDU串
    pSrc += tmp;        // 指针后移
    
    // TPDU段基本参数、回复地址等
    gsmString2Bytes(pSrc, &tmp, 2);    // 取基本参数
    pSrc += 2;        // 指针后移
    //if(tmp & 0x80)
    {
        // 包含回复地址，取回复地址信息
        gsmString2Bytes(pSrc, &tmp, 2);    // 取长度
        if(tmp & 1) tmp += 1;    // 调整奇偶性
        pSrc += 4;          // 指针后移
        gsmSerializeNumbers(pSrc, (*pDst).TPA, tmp);    // 取TP-RA号码
		m_strRecvPhone = (*pDst).TPA;
        pSrc += tmp;        // 指针后移
    }
    
    // TPDU段协议标识、编码方式、用户信息等
    gsmString2Bytes(pSrc, (unsigned char*)(*pDst).TP_PID, 2);    // 取协议标识(TP-PID)
    pSrc += 2;        // 指针后移
    gsmString2Bytes(pSrc, (unsigned char*)(*pDst).TP_DCS, 2);    // 取编码方式(TP-DCS)
    pSrc += 2;        // 指针后移
    gsmSerializeNumbers(pSrc, (*pDst).TP_SCTS, 14);        // 服务时间戳字符串(TP_SCTS) 
    pSrc += 14;       // 指针后移
    gsmString2Bytes(pSrc, &tmp, 2);    // 用户信息长度(TP-UDL)
    pSrc += 2;        // 指针后移
    (*pDst).TP_DCS=8;
    if((*pDst).TP_DCS == GSM_7BIT)    
    {
        // 7-bit解码
        nDstLength = gsmString2Bytes(pSrc, buf, tmp & 7 ? (int)tmp * 7 / 4 + 2 : (int)tmp * 7 / 4);  // 格式转换
        gsmDecode7bit(buf, (*pDst).TP_UD, nDstLength);    // 转换到TP-DU
        nDstLength = tmp;
    }
    //else if((*pDst).TP_DCS == GSM_UCS2)
    //{
        // UCS2解码
        //nDstLength = gsmString2Bytes(pSrc, buf, tmp * 2);        // 格式转换
        //nDstLength = mygsmDecodeUcs2(buf, (*pDst).TP_UD, nDstLength);    // 转换到TP-DU
    //}
    else
    {
        // 8-bit解码
        nDstLength = gsmString2Bytes(pSrc, buf, tmp * 2);        // 格式转换
        nDstLength = gsmDecode8bit(buf, (*pDst).TP_UD, nDstLength);    // 转换到TP-DU
		// nDstLength = gsmDecodeUcs2(buf, pDst->TP_UD, nDstLength);    // 转换到TP-DU		
    }
    
    // 返回目标字符串长度
    return nDstLength;
}


/* 
*@brief     PDU编码，用于编制、发送短消息                                                                   
*@param  pSrc:   源PDU参数指针                                                
*@param  pDst:  目标PDU串指针 
*@return  目标PDU串长度  
*/
int gsmEncodePdu(const struct SM_PARAM* pSrc, char* pDst)
{
    int nLength;             // 内部用的串长度
    int nDstLength;          // 目标PDU串长度
    unsigned char buf[256];  // 内部用的缓冲区
    
    // SMSC地址信息段
    nLength = (int)strlen((*pSrc).SCA);    // SMSC地址字符串的长度    
    buf[0] = (char)((nLength & 1) == 0 ? nLength : nLength + 1) / 2 + 1;    // SMSC地址信息长度
    buf[1] = 0x91;        // 固定: 用国际格式号码
    nDstLength = gsmBytes2String(buf, pDst, 2);        // 转换2个字节到目标PDU串
    nDstLength += gsmInvertNumbers((*pSrc).SCA, &pDst[nDstLength], nLength);    // 转换SMSC到目标PDU串
     // TPDU段基本参数、目标地址等
    nLength = (int)strlen((*pSrc).TPA);    // TP-DA地址字符串的长度
	m_strRecvPhone = (*pSrc).TPA;
    buf[0] = 0x11;            // 是发送短信(TP-MTI=01)，TP-VP用相对格式(TP-VPF=10)
    buf[1] = 0;               // TP-MR=0
    buf[2] = (char)nLength;   // 目标地址数字个数(TP-DA地址字符串真实长度)
    buf[3] = 0x91;            // 固定: 用国际格式号码
    nDstLength += gsmBytes2String(buf, &pDst[nDstLength], 4);  // 转换4个字节到目标PDU串
    nDstLength += gsmInvertNumbers((*pSrc).TPA, &pDst[nDstLength], nLength); // 转换TP-DA到目标PDU串
    
    // TPDU段协议标识、编码方式、用户信息等
    nLength = (int)strlen((*pSrc).TP_UD);    // 用户信息字符串的长度
    buf[0] = (*pSrc).TP_PID;        // 协议标识(TP-PID)
    buf[1] = (*pSrc).TP_DCS;        // 用户信息编码方式(TP-DCS)
    buf[2] = 0;            // 有效期(TP-VP)为5分钟
    if((*pSrc).TP_DCS == GSM_7BIT)    
    {
        // 7-bit编码方式
//        buf[3] = nLength;            // 编码前长度
//        nLength = gsmEncode7bit(pSrc->TP_UD, &buf[4], nLength+1) + 4; 
		// 转换		TP-DA到目标PDU串
        buf[3] = gsmEncode8bit((*pSrc).TP_UD, &buf[4], nLength);    // 转换TP-DA到目标PDU串
        nLength = buf[3] + 4;        // nLength等于该段数据长度
    }
    //else if((*pSrc).TP_DCS == GSM_UCS2)
    //{
        // UCS2编码方式
        //buf[3] = mygsmEncodeUcs2((*pSrc).TP_UD, &buf[4], nLength);    // 转换TP-DA到目标PDU串
        //nLength = buf[3] + 4;        // nLength等于该段数据长度
    //}
    else
    {
        // 8-bit编码方式
        buf[3] = gsmEncode8bit((*pSrc).TP_UD, &buf[4], nLength);    // 转换TP-DA到目标PDU串
        nLength = buf[3] + 4;        // nLength等于该段数据长度
    }
    nDstLength += gsmBytes2String(buf, &pDst[nDstLength], nLength);        // 转换该段数据到目标PDU串
    
    // 返回目标字符串长度
    return nDstLength;
}



/*
* 函数：gsmSendMessage                                                        
* 说明：发送短消息                                                            
* 参数：                                                                      
*      pSrc: 源PDU参数指针                                                    
*/
int gsmSendMessage(int fd,const struct SM_PARAM* pSrc)
{
    int nPduLength;        // PDU串长度
    unsigned char nSmscLength;    // SMSC串长度
    int nLength;           // 串口收到的数据长度
	char cmd[16] = {0};          // 命令串
	char pdu[512] = {0};         // PDU串
	char ans[128] = {0};         // 应答串
	nPduLength = gsmEncodePdu(pSrc, pdu);    // 根据PDU参数，编码PDU串
    strcat(pdu, "\x01a");        // 以Ctrl-Z结束
    
    gsmString2Bytes(pdu, &nSmscLength, 2);    // 取PDU串中的SMSC信息长度
    nSmscLength++;        // 加上长度字节本身
    // 命令中的长度，不包括SMSC信息长度，以数据字节计

    sprintf(cmd, "AT+CMGS=%d\r", nPduLength / 2 - nSmscLength);    // 生成命令
    nPduLength = gsmEncodePdu(pSrc, pdu);    // 根据PDU参数，编码PDU串
        
    gsmString2Bytes(pdu, &nSmscLength, 2);    // 取PDU串中的SMSC信息长度
    nSmscLength++;        // 加上长度字节本身

	strcat(pdu, "\x01A\0");        // 以Ctrl-Z结束

    // 命令中的长度，不包括SMSC信息长度，以数据字节计
    sprintf(cmd, "AT+CMGS=%d\r", nPduLength / 2 - nSmscLength);    // 生成命令

	//strcpy( pdu, "0891683108200105F011000D91683181076159F6000800064F60597D0021\0" );
	//strcpy( cmd, "AT+CMGS=21\r" );

	char szTemp[1024] = {0};
	sprintf( szTemp, "发送信息命令：%s, 信息内容：%s\n\0", cmd, pdu );
	//WriteLog( szTemp );

	//for( int i = 0; i != 6; i++ )
	{
		// 先输出命令串
        WritePort(fd,cmd, (int)(strlen(cmd)));
		sleep(100);

		// 输出信息
		WritePort(fd,pdu, (int)(strlen(pdu)));
		//WritePort("\x1A", 1);
		sleep(2000);

		// 读发送应答数据
		nLength = ReadPort(fd,ans, 128);
		ans[nLength] = '\0';
		sprintf( szTemp, "发送的应答：%s\n\0", ans );
		//WriteLog( szTemp );
	}

	 return 0;    
}



/*
*@brief 通过指定端口，发送短信
*@param  strRecvPhone，接收手机号码
*@param  strMsgContent，短信内容
*@param  strPortName，串行端口的名称
*/
int send_msg(int fd,char* strRecvPhone , char* strMsgContent, int nSMSMaxLength)
{
    int i=0;
    size_t t;
    char ans[128];        // 应答串
    char TPA[16];       // 目标号码或回复号码(TP-DA或TP-RA)
    char  cmd[20];
    //struct MsgList *msglist;
	//int iPage = page( strMsgContent.GetBuffer(strMsgContent.GetLength()), msglist, nSMSMaxLength ); //返回短信分页数
    int iPage = 1;
    //t= m_strRecvPhone.GetLength()+2;
    t = strlen(strRecvPhone) + 2;
	for(i=2;i<t;i++)
    {         
	   //TPA[i]=m_strRecvPhone.GetAt(i-2);
        TPA[i] = *strRecvPhone++; 
    }
    TPA[0]='8';
    TPA[1]='6';
	TPA[t]='\0';
	for(int iIndex=0 ;iIndex < iPage;iIndex++)
	{
		//printf( "Content:%s,Msg:%s\n", strMsgContent.GetBuffer(), msglist->chMsg );
		//WriteErr( "短信：" );
		//WriteErr( msglist->chMsg );
        sprintf(cmd , "AT\r");
        WritePort(fd,cmd, (int)strlen(cmd));
	    sleep(ShortSleepTime);
        int nLength = ReadPort(fd,ans, 128);
        ans[nLength] = '\0';

        sprintf(cmd, "ATE0\r"); 
        WritePort(fd,cmd, (int)strlen(cmd));
	    sleep(ShortSleepTime);
        nLength = ReadPort(fd,ans, 128);
        ans[nLength] = '\0';

	    sprintf(cmd, "AT+CSMS=0\r");      
        WritePort(fd,cmd, (int)strlen(cmd));  
 	    sleep(ShortSleepTime);
        nLength = ReadPort(fd,ans, 128);
        ans[nLength] = '\0';
 
	    sprintf(cmd, "AT+CMGF=0\r");      
        WritePort(fd,cmd, (int)strlen(cmd));  
	    sleep(ShortSleepTime);
        nLength = ReadPort(fd,ans, 128);
        ans[nLength] = '\0';
        //sm_param_temp= new SM_PARAM;
        struct SM_PARAM *sm_param_temp;
        strcpy((*sm_param_temp).SCA,m_SmsCenterNum);

        (*sm_param_temp).TP_DCS=0x8;
        (*sm_param_temp).TP_PID=0x0;
        strcpy((*sm_param_temp).TPA,TPA);
        strcpy((*sm_param_temp).TP_UD,strMsgContent);
        if(iPage > 1)
            sprintf((*sm_param_temp).TP_UD,"%d/%d %s" ,iIndex+1,iPage,strMsgContent);
        //printf("Msg :%s\n" ,sm_param_temp->TP_UD);
        //msglist = msglist.pNext;      
        if(!gsmSendMessage(fd,sm_param_temp))//发送短信
        {
            printf("Send SMS Failed\n");
			//WriteLog("Send SMS Failed");
            return -1;
        }
        sleep(5000);
    }
	return 0;   
}

