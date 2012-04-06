
/* Copyright (C) 2010  dragonflow Inc.
 * All right reserved.
 */

#include <stdio.h>
#include <string.h>
#include <erl_driver.h>
#include <ei.h>

#define _LARGEFILE_SOURCE 


#ifdef WIN32
int wmi_win(ei_x_buff *x,char *server,char *username,char *password,char *psql[],int count);
#else	
int wmi_linux(ei_x_buff *x,char *server,char *username_password,char *psql[],int count);
#endif

int wmi(ei_x_buff *x,char *server,char *username,char *password,char *psql[],int count)
{
#ifdef WIN32
    int result = wmi_win(x,server,username,password,psql,count);
#else	
	char *hostserver = NULL;
	char *username_password = NULL;
	int result;
	username_password = (char*)driver_alloc(256);
	strcpy(username_password,username);
	strcat(username_password,"%");
	strcat(username_password,password);
	hostserver = (char*)driver_alloc(256);
	strcpy(hostserver,"//");
	strcat(hostserver,server);	
	result = wmi_linux(x,hostserver,username_password,psql,count);
	driver_free(username_password);	
	driver_free(hostserver);	
#endif
	return result;
}

typedef struct {
      ErlDrvPort port;
} wmic_data;

typedef struct {
  ErlDrvPort port;
  char *ei_buff;
  int ei_index;
  char msg[64];
  char szmsg[256];
}async_data;

static ErlDrvData start(ErlDrvPort port, char *buff)
{
  
   wmic_data* data = (wmic_data*)driver_alloc(sizeof(wmic_data));
   data->port = port;
 
   set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);

   return (ErlDrvData)data;
}

static void stop(ErlDrvData handle)
{
   driver_free((char*)handle);
}

static ErlDrvBinary* ei_x_to_new_binary(const ei_x_buff *x_buff)
{ 
	ErlDrvBinary *bin = driver_alloc_binary(x_buff->index); 
	if(bin != NULL) 
	{  
		memcpy(bin->orig_bytes, x_buff->buff, x_buff->index); 
	} 
	return bin;
}


   

static int wmic(char *buf, ei_x_buff *x, char **str)
{
    char *server = NULL;
	char *username = NULL;
	char *password = NULL;
	char *psql[100];
	int version;
	int arity;
	int size,listsize;
	int index = 0;
	int type;
	int i;
	
        
	
	ei_decode_version(buf, &index, &version);

	ei_decode_tuple_header(buf, &index, &arity);

	ei_get_type(buf, &index, &type, &size);
	if(type == ERL_STRING_EXT)
	{	
		server = (char*)driver_alloc(size + 1);
		ei_decode_string(buf, &index, server);
		//printf("server:%s\n\r",server);
		ei_get_type(buf, &index, &type, &size);
		if(type == ERL_STRING_EXT)
		{	
			username = (char*)driver_alloc(size + 1);
			ei_decode_string(buf, &index, username);
			//printf("username:%s\n\r",username);
			ei_get_type(buf, &index, &type, &size);
			if(type == ERL_STRING_EXT)
			{	
				password = (char*)driver_alloc(size + 1);
				ei_decode_string(buf, &index, password);
				//printf("password:%s\n\r",password);
				ei_get_type(buf, &index, &type, &size);
				if(type == ERL_LIST_EXT)
				{						
					ei_decode_list_header(buf, &index,  &size);					
					for(i = 0; i < size; i++) 
					{						
						 ei_get_type(buf, &index, &type, &listsize);	
						 psql[i] = (char*)driver_alloc(listsize + 1);
						 if(type == ERL_STRING_EXT)
						 {							 
							  ei_decode_string(buf, &index,  psql[i]);
							 // printf("psql:%s\n\r", psql[i]);							 
						 }
						 else
						 {
							strcpy(psql[i],"");
						 }

					}
					wmi(x,server,username,password,psql,size);

					for(i = 0; i < size; i++) 
					{
						 driver_free(psql[i]);
					}
					
					return -1;
				}
				else if(type == ERL_STRING_EXT)
				{
				    psql[0] = (char*)driver_alloc(size + 1);
					ei_decode_string(buf, &index, psql[0]);
					//printf("psql:%s\n\r",psql[0]);	
					
					wmi(x,server,username,password,psql,1);
					driver_free(server);	
					driver_free(username);	
					driver_free(password);	
					driver_free(psql[0]);
					return -1;
				}
				else
				{	
					driver_free(password);	
					driver_free(username);	
					driver_free(server);	
					
				}	
			}
			else
			{	
				driver_free(username);	
				driver_free(server);
			}	
		}
		else
		{	
			driver_free(server);	
		}		
	}
	*str = (char*)driver_alloc(10241);
	strcpy(*str,"Params error!");
	return 0;

}

static void error_drv(void * data)
{
    async_data* d = (async_data*)data;
    ei_x_buff x;
	//printf("GetCurrentThreadId:%d \n\r",GetCurrentThreadId());
    ei_x_new_with_version(&x);
    ei_x_encode_tuple_header(&x, 2);
    ei_x_encode_atom(&x, d->msg);
	ei_x_encode_string(&x, d->szmsg);
	driver_output(d->port,x.buff, x.index);
	 ei_x_free(&x);
}



static void wmic_drv(void * data)
{
	async_data* d = (async_data*)data;
	char *buf = d->ei_buff;
	int index = d->ei_index;
        char *server = NULL;
	char *username = NULL;
	char *password = NULL;
	char *psql[100];

	int size,listsize;	
	int type;
	int i;
    ei_x_buff x; 

	/*printf("GetCurrentThreadId:%d \n\r",GetCurrentThreadId());*/
	ei_x_new_with_version(&x);
    ei_x_encode_tuple_header(&x, 2);

	ei_get_type(buf, &index, &type, &size);

	if(type == ERL_STRING_EXT)
	{	
		server = (char*)driver_alloc(size + 1);
		ei_decode_string(buf, &index, server);
	/*	printf("server:%s\n\r",server);*/
		ei_get_type(buf, &index, &type, &size);
		if(type == ERL_STRING_EXT)
		{	
			username = (char*)driver_alloc(size + 1);
			ei_decode_string(buf, &index, username);
			//printf("username:%s\n\r",username);
			ei_get_type(buf, &index, &type, &size);
			if(type == ERL_STRING_EXT)
			{	
				password = (char*)driver_alloc(size + 1);
				ei_decode_string(buf, &index, password);
				//printf("password:%s\n\r",password);
				ei_get_type(buf, &index, &type, &size);
				if(type == ERL_LIST_EXT)
				{						
					ei_decode_list_header(buf, &index,  &size);					
					for(i = 0; i < size; i++) 
					{						
						 ei_get_type(buf, &index, &type, &listsize);	
						 psql[i] = (char*)driver_alloc(listsize + 1);
						 if(type == ERL_STRING_EXT)
						 {							 
							  ei_decode_string(buf, &index,  psql[i]);
							 // printf("psql:%s\n\r", psql[i]);							 
						 }
						 else
						 {
							strcpy(psql[i],"");
						 }

					}
					wmi(&x,server,username,password,psql,size);

					for(i = 0; i < size; i++) 
					{
						 driver_free(psql[i]);
					}					
					
				}
				else if(type == ERL_STRING_EXT)
				{
				    psql[0] = (char*)driver_alloc(size + 1);
					ei_decode_string(buf, &index, psql[0]);
					//printf("psql:%s\n\r",psql[0]);	
					
					wmi(&x,server,username,password,psql,1);
					driver_free(server);	
					driver_free(username);	
					driver_free(password);	
					driver_free(psql[0]);					
				}
				else
				{	
					driver_free(password);	
					driver_free(username);	
					driver_free(server);	
					ei_x_encode_atom(&x, "error");
				    ei_x_encode_atom(&x, "Params error!");
				}	
			}
			else
			{	
				driver_free(username);	
				driver_free(server);
				ei_x_encode_atom(&x, "error");
				ei_x_encode_atom(&x, "Params error!");
			}	
		}
		else
		{	
			driver_free(server);
			ei_x_encode_atom(&x, "error");
			ei_x_encode_atom(&x, "Params error!");
		}		
	}
	else
	{
	   ei_x_encode_atom(&x, "error");
	   ei_x_encode_atom(&x, "Params error!");
	}
	free(d->ei_buff);
	driver_output(d->port,x.buff, x.index);
	ei_x_free(&x);
}

static void  output(ErlDrvData drv_data, char* buff, int bufflen)
{
    wmic_data* data = (wmic_data*)drv_data;
	async_data* a = (async_data*)malloc(sizeof(async_data));	
    int version;
	int arity;
	int size;
	int index = 0;
	int type;
    /* printf("__________________________\n\r");	*/
    a->ei_buff = malloc(bufflen+1);
	memset(a->ei_buff,0,bufflen+1);
	memcpy(a->ei_buff,buff,bufflen);

	/*printf("GetCurrentThreadId:%d \n\r",GetCurrentThreadId());*/
	a->port = data->port;
	
	ei_decode_version(buff, &index, &version);
	ei_decode_tuple_header(buff, &index, &arity);
	ei_get_type(buff, &index, &type, &size);


	if(type == ERL_SMALL_INTEGER_EXT)
	{	
		unsigned long command;
        ei_decode_ulong(buff, &index, &command);		
		a->ei_index = index;
        switch(command)
		{
		    case 0:
				driver_async(data->port, NULL, wmic_drv, a, free);
				break;
			default:
				strcpy(a->msg,"error");
				strcpy(a->szmsg,"not_unkown_function");
				driver_async(data->port, NULL, error_drv, a, free);
		}
	}
	else
	{
	        strcpy(a->msg,"error");
			strcpy(a->szmsg,"not_unkown_function");
			driver_async(data->port, NULL, error_drv, a, free);
	}
}

static int control(ErlDrvData drv_data, unsigned int command,char *buf, int len,char **rbuf, int rlen)
{

	wmic_data* data = (wmic_data*)drv_data;
    async_data* a = (async_data*)malloc(sizeof(async_data));

	/*printf("GetCurrentThreadId:%d \n\r",GetCurrentThreadId());*/
	a->port = data->port;
	switch (command) {
		case 0:	
			a->ei_buff = buf;
			driver_async(data->port, NULL, wmic_drv, a, free);
			break;		
        default:
			strcpy(a->msg,"error");
			strcpy(a->szmsg,"not_unkown_function");
			driver_async(data->port, NULL, error_drv, a, free);
			break;
    }
	return 0;

  //  char *pattern=NULL, *str=NULL;
  //  char *retv=NULL;    
  //  int r_code = 0;
  //  int sub_size = 0;
  //  int result;
  //  int ok = 0;
	
  //  ei_x_buff x;
  //  wmic_data* data = (wmic_data*)drv_data;
  //  ei_x_new_with_version(&x);
  //  ei_x_encode_tuple_header(&x, 2);
  //  printf("_______________________________\n\r");
  //  switch (command) {
		//case 0:
		//	//~ if(mac_addr_sys(addrs) == 1)				
		//	//~ {
		//		//~ ei_x_encode_atom(&x, "ok");		
		//		//~ ei_x_encode_string(&x, addrs);                     
		//	//~ }
		//	//~ else
		//	//~ {
		//		//~ ei_x_encode_atom(&x, "error");
		//		//~ ei_x_encode_atom(&x, "nomatch");
		//	//~ }
		//	//~ break;	
		//	result = wmic(buf, &x, &str);			
		//	if(result != -1)			
		//	{
		//		ei_x_encode_atom(&x, "error");
		//		ei_x_encode_string(&x, str);
		//		driver_free(str);
		//	}
		//	
		//	break;		
  //      default:
		//	ei_x_encode_atom(&x, "error");
		//	ei_x_encode_atom(&x, "not_unkown_function");
		//	break;
  //  }
  //  *rbuf = (char*)ei_x_to_new_binary(&x);
  //  rlen = x.buffsz;
  //  ei_x_free(&x);

  //  return rlen;
}

static void ready_async(ErlDrvData handle, ErlDrvThreadData async_data)
{
  
}


ErlDrvEntry wmic_driver_entry = {
   NULL,                       /* F_PTR init, N/A */
   start,          /* L_PTR start, called when port is opened */
   stop,           /* F_PTR stop, called when port is closed */
   output,         /* F_PTR output, called when erlang has sent */
   NULL,                       /* F_PTR ready_input, called when input descriptor ready */
   NULL,                       /* F_PTR ready_output, called when output descriptor ready */
   "wmic_drv",              /* char *driver_name, the argument to open_port */
   NULL,                       /* F_PTR finish, called when unloaded */
   NULL,                       /* handle */
   NULL,          /* F_PTR control, port_command callback */
   NULL,                       /* F_PTR timeout, reserved */
   NULL,                        /* F_PTR outputv, reserved */
   ready_async,
   NULL,
   NULL,
   NULL
};

DRIVER_INIT(wmic_erl) /* must match name in driver_entry */
{
    return &wmic_driver_entry;
}


