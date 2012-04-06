/*
 * ejabberd, Copyright (C) 2002-2008   Process-one
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *                         
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
 * 02111-1307 USA
 *
 */

#include <stdio.h>
#include <string.h>
#include <erl_driver.h>
#include <ei.h>
#include "os_regex.h"

#define _LARGEFILE_SOURCE 



typedef struct {
      ErlDrvPort port;
} regex_data;


static ErlDrvData start(ErlDrvPort port, char *buff)
{
   regex_data* data = (regex_data*)driver_alloc(sizeof(regex_data));
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



static int match_param(char *buf, ei_x_buff *x, char **pattern, char **str)
{
	unsigned char smallWidth, smallHeight;

	int version;
	int arity;
	int size;
	int index = 0;
	int type;

	ei_decode_version(buf, &index, &version);

	ei_decode_tuple_header(buf, &index, &arity);

	ei_get_type(buf, &index, &type, &size);
	if(type == ERL_STRING_EXT)
	{
		*pattern = (char*)driver_alloc(size + 1);
		ei_decode_string(buf, &index, *pattern);

		ei_get_type(buf, &index, &type, &size);
		if(type == ERL_STRING_EXT)
		{
			*str = (char*)driver_alloc(size + 1);
			ei_decode_string(buf, &index, *str);
		}
		else
		{
			ei_x_encode_atom(x, "error");
			ei_x_encode_atom(x, "format_parameter_type_error");
			return 0;
		}
	}
	else
	{
		ei_x_encode_atom(x, "error");
		ei_x_encode_atom(x, "image_parameter_type_error");
		return 0;
	}
	return index;

}





static int control(ErlDrvData drv_data, unsigned int command,char *buf, int len,char **rbuf, int rlen)
{
    char *pattern=NULL, *str=NULL;
    char *retv=NULL;    
    OSRegex reg;
    int r_code = 0;
    int sub_size = 0;
	int result;
    char **ret;

    ei_x_buff x;
    regex_data* data = (regex_data*)drv_data;
    ei_x_new_with_version(&x);
	ei_x_encode_tuple_header(&x, 2);

    switch (command) {
		case 0:
			result = match_param(buf, &x, &pattern, &str);
            //printf("_________%s____%s_______\n",pattern,str);
			if(result)
			{
				if(OS_Match2(pattern,str))				
				{
					ei_x_encode_atom(&x, "ok");		
                    ei_x_encode_atom(&x, "match");                    
				}
				else
				{
					ei_x_encode_atom(&x, "error");
					ei_x_encode_atom(&x, "nomatch");
				}
			}
			break;
		case 1:
			result = match_param(buf, &x, &pattern, &str);
			if(result)
			{
				if(OS_Regex(pattern,str))				
				{
					ei_x_encode_atom(&x, "ok");		
                    ei_x_encode_atom(&x, "match");                    
				}
				else
				{
					ei_x_encode_atom(&x, "error");
					ei_x_encode_atom(&x, "nomatch");
				}
			}
			break;
        case 2:
			result = match_param(buf, &x, &pattern, &str);
			if(result)
			{
                 if(OSRegex_Compile(pattern, &reg, OS_RETURN_SUBSTRING))
                 {                   
                    if((retv = OSRegex_Execute(str, &reg)))
                    {                              
                        r_code = 1;                            
                        ei_x_encode_atom(&x, "ok");
                       // printf(" : '%s'\r\n", retv);   
                        ei_x_encode_list_header(&x, 1);
                        ei_x_encode_string(&x, retv+1);                        
                        ret = reg.sub_strings;                         
                        while(*ret)
                        {
                           // printf("%d: !%s!\n\r", sub_size, *ret);    
                            ei_x_encode_list_header(&x, 1);
                            ei_x_encode_string(&x, *ret);                            
                            sub_size++; ret++;
                        }                        
                        OSRegex_FreeSubStrings(&reg);   
                        ei_x_encode_empty_list(&x);	
                          
                    }
                    else
                    {
                        ei_x_encode_atom(&x, "error");
                        ei_x_encode_atom(&x, "nomatch");
                    }
                    OSRegex_FreePattern(&reg);
                 }
                 else                     
                 {
                    ei_x_encode_atom(&x, "error");
                    ei_x_encode_atom(&x, "nomatch");
                 }               
			}
			break;  
         case 3:
			result = match_param(buf, &x, &pattern, &str);
			if(result)
			{
                 if(OSRegex_Compile(pattern, &reg, OS_RETURN_SUBSTRING))
                 {                   
                    if((retv = OSRegex_Execute(str, &reg)))
                    {                              
                        r_code = 1;                            
                        ei_x_encode_atom(&x, "ok");
                       // printf(" : '%s'\r\n", retv);   
                       // ei_x_encode_list_header(&x, 1);
                        ei_x_encode_string(&x, retv+1); 
                        OSRegex_FreeSubStrings(&reg);   
                       // ei_x_encode_empty_list(&x);
                    }
                    else
                    {
                        ei_x_encode_atom(&x, "error");
                        ei_x_encode_atom(&x, "nomatch");
                    }
                    OSRegex_FreePattern(&reg);
                 }
                 else                     
                 {
                    ei_x_encode_atom(&x, "error");
                    ei_x_encode_atom(&x, "nomatch");
                 }               
			}
			break;    
        default:
			ei_x_encode_atom(&x, "error");
			ei_x_encode_atom(&x, "not_unkown_function");
			break;
    }
    *rbuf = (char*)ei_x_to_new_binary(&x);
	rlen = x.buffsz;
    ei_x_free(&x);

    return rlen;
}





ErlDrvEntry regex_driver_entry = {
   NULL,                       /* F_PTR init, N/A */
   start,          /* L_PTR start, called when port is opened */
   stop,           /* F_PTR stop, called when port is closed */
   NULL,         /* F_PTR output, called when erlang has sent */
   NULL,                       /* F_PTR ready_input, called when input descriptor ready */
   NULL,                       /* F_PTR ready_output, called when output descriptor ready */
   "regex_drv",              /* char *driver_name, the argument to open_port */
   NULL,                       /* F_PTR finish, called when unloaded */
   NULL,                       /* handle */
   control,          /* F_PTR control, port_command callback */
   NULL,                       /* F_PTR timeout, reserved */
   NULL                        /* F_PTR outputv, reserved */
};

DRIVER_INIT(regex_erl) /* must match name in driver_entry */
{
    return &regex_driver_entry;
}


