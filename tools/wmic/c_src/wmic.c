/*
   WMI Sample client
   Copyright (C) 2006 Andrzej Hajda <andrzej.hajda@wp.pl>

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/


#include "includes.h"
#include "lib/cmdline/popt_common.h"
#include "librpc/rpc/dcerpc.h"
#include "librpc/gen_ndr/ndr_oxidresolver.h"
#include "librpc/gen_ndr/ndr_oxidresolver_c.h"
#include "librpc/gen_ndr/ndr_dcom.h"
#include "librpc/gen_ndr/ndr_dcom_c.h"
#include "librpc/gen_ndr/ndr_remact_c.h"
#include "librpc/gen_ndr/ndr_epmapper_c.h"
#include "librpc/gen_ndr/com_dcom.h"
#include "librpc/rpc/dcerpc_table.h"

#include "lib/com/dcom/dcom.h"
#include "lib/com/proto.h"
#include "lib/com/dcom/proto.h"

#include "wmi/wmi.h"

#include <stdlib.h> 
#include <stdio.h>
#include <string.h>
#include <erl_driver.h>
#include <ei.h>




struct WBEMCLASS;
struct WBEMOBJECT;

#include "wmi/proto.h"

struct program_args {
    char *hostname;
    char *query;
    char *ns;
    char *delim;
};

static bool parse_args(int argc, char *argv[], struct program_args *pmyargs)
{
    poptContext pc;
    int opt, i;

    int argc_new;
    char **argv_new;

    struct poptOption long_options[] = {
	POPT_AUTOHELP
	POPT_COMMON_SAMBA
	POPT_COMMON_CONNECTION
	POPT_COMMON_CREDENTIALS
	POPT_COMMON_VERSION
        {"namespace", 0, POPT_ARG_STRING, &pmyargs->ns, 0,
         "WMI namespace, default to root\\cimv2", 0},
	{"delimiter", 0, POPT_ARG_STRING, &pmyargs->delim, 0,
	 "delimiter to use when querying multiple values, default to '|'", 0},
	POPT_TABLEEND
    };

    pc = poptGetContext("wmi", argc, (const char **) argv,
	        long_options, POPT_CONTEXT_KEEP_FIRST);

    poptSetOtherOptionHelp(pc, "//host query\n\nExample: wmic -U [domain/]adminuser%password //host \"select * from Win32_ComputerSystem\"");

    while ((opt = poptGetNextOpt(pc)) != -1) {
	poptPrintUsage(pc, stdout, 0);
	poptFreeContext(pc);
	return false;
    }

    argv_new = discard_const_p(char *, poptGetArgs(pc));

    argc_new = argc;
    for (i = 0; i < argc; i++) {
	if (argv_new[i] == NULL) {
	    argc_new = i;
	    break;
	}
    }

    if (argc_new != 3
	|| strncmp(argv_new[1], "//", 2) != 0) {
	poptPrintUsage(pc, stdout, 0);
	poptFreeContext(pc);	
	return false;
    }

    /* skip over leading "//" in host name */
    pmyargs->hostname = argv_new[1] + 2;
    pmyargs->query = argv_new[2];
    poptFreeContext(pc);
    return true;
}

#define WERR_CHECK(msg) if (!W_ERROR_IS_OK(result)) { \
			    DEBUG(0, ("ERROR: %s\n", msg)); \
			    goto error; \
			} else { \
			    DEBUG(1, ("OK   : %s\n", msg)); \
			}

#define RETURN_CVAR_ARRAY_STR(fmt, arr) {\
        uint32_t i;\
	char *r;\
\
        if (!arr) {\
                return talloc_strdup(mem_ctx, "NULL");\
        }\
	r = talloc_strdup(mem_ctx, "[");\
        for (i = 0; i < arr->count; ++i) {\
		r = talloc_asprintf_append(r, fmt "%s", arr->item[i], (i+1 == arr->count)?"":",");\
        }\
        return talloc_asprintf_append(r, "]");\
}

char *string_CIMVAR(TALLOC_CTX *mem_ctx, union CIMVAR *v, enum CIMTYPE_ENUMERATION cimtype)
{
	switch (cimtype) {
        case CIM_SINT8: return talloc_asprintf(mem_ctx, "%d", v->v_sint8);
        case CIM_UINT8: return talloc_asprintf(mem_ctx, "%u", v->v_uint8);
        case CIM_SINT16: return talloc_asprintf(mem_ctx, "%d", v->v_sint16);
        case CIM_UINT16: return talloc_asprintf(mem_ctx, "%u", v->v_uint16);
        case CIM_SINT32: return talloc_asprintf(mem_ctx, "%d", v->v_sint32);
        case CIM_UINT32: return talloc_asprintf(mem_ctx, "%u", v->v_uint32);
        case CIM_SINT64: return talloc_asprintf(mem_ctx, "%lld", v->v_sint64);
        case CIM_UINT64: return talloc_asprintf(mem_ctx, "%llu", v->v_sint64);
        case CIM_REAL32: return talloc_asprintf(mem_ctx, "%f", (double)v->v_uint32);
        case CIM_REAL64: return talloc_asprintf(mem_ctx, "%f", (double)v->v_uint64);
        case CIM_BOOLEAN: return talloc_asprintf(mem_ctx, "%s", v->v_boolean?"True":"False");
        case CIM_STRING:
        case CIM_DATETIME:
        case CIM_REFERENCE: return talloc_asprintf(mem_ctx, "%s", v->v_string);
        case CIM_CHAR16: return talloc_asprintf(mem_ctx, "undefined");
        case CIM_OBJECT: return talloc_asprintf(mem_ctx, "undefined");
        case CIM_ARR_SINT8: RETURN_CVAR_ARRAY_STR("%d", v->a_sint8);
        case CIM_ARR_UINT8: RETURN_CVAR_ARRAY_STR("%u", v->a_uint8);
        case CIM_ARR_SINT16: RETURN_CVAR_ARRAY_STR("%d", v->a_sint16);
        case CIM_ARR_UINT16: RETURN_CVAR_ARRAY_STR("%u", v->a_uint16);
        case CIM_ARR_SINT32: RETURN_CVAR_ARRAY_STR("%d", v->a_sint32);
        case CIM_ARR_UINT32: RETURN_CVAR_ARRAY_STR("%u", v->a_uint32);
        case CIM_ARR_SINT64: RETURN_CVAR_ARRAY_STR("%lld", v->a_sint64);
        case CIM_ARR_UINT64: RETURN_CVAR_ARRAY_STR("%llu", v->a_uint64);
        case CIM_ARR_REAL32: RETURN_CVAR_ARRAY_STR("%f", v->a_real32);
        case CIM_ARR_REAL64: RETURN_CVAR_ARRAY_STR("%f", v->a_real64);
        case CIM_ARR_BOOLEAN: RETURN_CVAR_ARRAY_STR("%d", v->a_boolean);
        case CIM_ARR_STRING: RETURN_CVAR_ARRAY_STR("%s", v->a_string);
        case CIM_ARR_DATETIME: RETURN_CVAR_ARRAY_STR("%s", v->a_datetime);
        case CIM_ARR_REFERENCE: RETURN_CVAR_ARRAY_STR("%s", v->a_reference);
	default: return talloc_asprintf(mem_ctx, "undefined");
	}
}

#undef RETURN_CVAR_ARRAY_STR
int wmi_linux(ei_x_buff *x,char *server,char *username_password,char *psql[],int count)
{
	struct program_args args = {};
	uint32_t cnt = 5, ret;
	//char *class_name = NULL;
	WERROR result;
	NTSTATUS status;
	struct IWbemServices *pWS = NULL;
	int i;

	//char *params[] = {"./wmic","-U","administrator%888888","//192.168.0.185","select * from CIM_DataFile where Drive= 'C:' and Path = '\\\\'"};   
	char *params[] = {"./wmic","-U","administrator%888888","//192.168.0.185","select * from CIM_DataFile where Drive= 'C:' and Path = '\\\\' and ( FileName = 'ntdetect' or FileName = 'id')"};   
	//char *params[] = {"./wmic","-U","administrator%888888","//192.168.0.185","select * from CIM_DataFile where Drive= 'C:' and Path = '\\\\' and FileName = 'ntdetect' "};   

	params[2] = username_password;
	params[3] = server;
	params[4] = "";
	

	//~ printf("username_password:%s\n\r",params[2]);
	//~ printf("hostname:%s\n\r",params[3]);
	//~ printf("query:%s\n\r",params[4] );
	
        if (!parse_args(5, params, &args))
	{
		ei_x_encode_atom(x, "error");		
		ei_x_encode_string(x,"Error Params!"); 
		return 1;
	}
	
	//~ /* apply default values if not given by user*/
	if (!args.ns) args.ns = "root\\cimv2";
	if (!args.delim) args.delim = "|";
	//~ args.ns = "root\\cimv2";
	//~ args.delim = "|";
	//~ args.hostname = "//192.168.0.185";
        //~ args.query = "select * from CIM_DataFile where Drive= 'C:' and Path = '\\\\' and FileName = 'ntdetect'";
		
	
	//~ printf("ns:%s\n\r",args.ns);
	//~ printf("delim:%s\n\r",args.delim);
	//~ printf("hostname:%s\n\r",args.hostname);
	//~ printf("query:%s\n\r",args.query);

				
			


			
	dcerpc_init();
	dcerpc_table_init();

	dcom_proxy_IUnknown_init();
	dcom_proxy_IWbemLevel1Login_init();
	dcom_proxy_IWbemServices_init();
	dcom_proxy_IEnumWbemClassObject_init();
	dcom_proxy_IRemUnknown_init();
	dcom_proxy_IWbemFetchSmartEnum_init();
	dcom_proxy_IWbemWCOSmartEnum_init();

	struct com_context *ctx = NULL;
	com_init_ctx(&ctx, NULL);
	dcom_client_init(ctx, cmdline_credentials);

	result = WBEM_ConnectServer(ctx, args.hostname, args.ns, 0, 0, 0, 0, 0, 0, &pWS);
	
	if (!W_ERROR_IS_OK(result)) 
	{ 
		//printf("_________________WBEM_ConnectServer again!\r\n");
		usleep(200);
		//printf("WBEM_ConnectServer again!\r\n");
		result = WBEM_ConnectServer(ctx, args.hostname, args.ns, 0, 0, 0, 0, 0, 0, &pWS);
	
		if (!W_ERROR_IS_OK(result)) 
		{ 
			status = werror_to_ntstatus(result);
			fprintf(stderr, "WBEM_ConnectServer NTSTATUS: %s - %s\n", nt_errstr(status), get_friendly_nt_error_msg(status));
			talloc_free(ctx);
			ei_x_encode_atom(x, "error");		
			ei_x_encode_string(x,get_friendly_nt_error_msg(status)); 
			return 0;
		} 
	} 
			
	//WERR_CHECK("Login to remote object.");


	ei_x_encode_atom(x, "ok");
	for(i = 0; i< count;i++)
	{
		ei_x_encode_list_header(x,1);
		ei_x_encode_tuple_header(x, 2);	
		ei_x_encode_string(x,psql[i]);
		
		struct IEnumWbemClassObject *pEnum = NULL;
		result = IWbemServices_ExecQuery(pWS, ctx, "WQL", psql[i], WBEM_FLAG_RETURN_IMMEDIATELY | WBEM_FLAG_ENSURE_LOCATABLE, NULL, &pEnum);
		if (!W_ERROR_IS_OK(result)) 
		{ 
			status = werror_to_ntstatus(result);
			fprintf(stderr, "IWbemServices_ExecQuery NTSTATUS: %s - %s\n", nt_errstr(status), get_friendly_nt_error_msg(status));
			ei_x_encode_tuple_header(x, 2);
			ei_x_encode_atom(x, "error");		
			ei_x_encode_string(x,get_friendly_nt_error_msg(status)); 
			return 0;
		} 
		//WERR_CHECK("WMI query execute.");

		IEnumWbemClassObject_Reset(pEnum, ctx);
		if (!W_ERROR_IS_OK(result)) 
		{ 
			status = werror_to_ntstatus(result);
			fprintf(stderr, "NTSTATUS: %s - %s\n", nt_errstr(status), get_friendly_nt_error_msg(status));			
			ei_x_encode_tuple_header(x, 2);
			ei_x_encode_atom(x, "error");		
			ei_x_encode_string(x,get_friendly_nt_error_msg(status)); 
			return 0;
		} 
		//WERR_CHECK("Reset result of WMI query.");
		
		ei_x_encode_tuple_header(x, 2);	
		ei_x_encode_atom(x, "ok");
		int recordcount = 0;
		do {
			uint32_t i, j;
			struct WbemClassObject *co[cnt];

			result = IEnumWbemClassObject_SmartNext(pEnum, ctx, 0xFFFFFFFF, cnt, co, &ret);
			/* WERR_BADFUNC is OK, it means only that there is less returned objects than requested */
			if (!W_ERROR_EQUAL(result, WERR_BADFUNC))
			{
				if (!W_ERROR_IS_OK(result)) continue;
			} 
			
			if (!ret) break;		
					
			for (i = 0; i < ret; ++i) {
				
				ei_x_encode_list_header(x,1);
				ei_x_encode_tuple_header(x, co[i]->obj_class->__PROPERTY_COUNT);
				recordcount ++;
				for (j = 0; j < co[i]->obj_class->__PROPERTY_COUNT; ++j)				
				{
					char *s;	
					TALLOC_CTX *mem_ctx;
					union CIMVAR *v;
					enum CIMTYPE_ENUMERATION cimtype;
					
					ei_x_encode_tuple_header(x, 2);	
					ei_x_encode_atom(x, co[i]->obj_class->properties[j].name);
					//s = string_CIMVAR(ctx, &co[i]->instance->data[j], co[i]->obj_class->properties[j].desc->cimtype & CIM_TYPEMASK);
					//ei_x_encode_binary(x, s,strlen(s));  
					mem_ctx =  ctx;
					v = &co[i]->instance->data[j];
					cimtype = co[i]->obj_class->properties[j].desc->cimtype & CIM_TYPEMASK;
					//printf("---->%s=%d\r\n",  (const char*)co[i]->obj_class->properties[j].name, cimtype);
					switch (cimtype) {
						case CIM_SINT8: 
							ei_x_encode_long(x,v->v_sint8);
							break;
							//s = talloc_asprintf(mem_ctx, "%d", v->v_sint8);
						case CIM_UINT8: 
							ei_x_encode_ulong(x,v->v_uint8);
							break;
							//return talloc_asprintf(mem_ctx, "%u", v->v_uint8);
						case CIM_SINT16: 
							ei_x_encode_long(x,v->v_sint16);
							break;
							//return talloc_asprintf(mem_ctx, "%d", v->v_sint16);
						case CIM_UINT16: 
							ei_x_encode_ulong(x,v->v_uint16);
							break;
							//return talloc_asprintf(mem_ctx, "%u", v->v_uint16);
						case CIM_SINT32: 
							ei_x_encode_long(x,v->v_sint32);
							break;
							//return talloc_asprintf(mem_ctx, "%d", v->v_sint32);
						case CIM_UINT32: 
							ei_x_encode_ulong(x,v->v_uint32);
							break;
							//return talloc_asprintf(mem_ctx, "%u", v->v_uint32);
						//~ case CIM_SINT64: 
							//~ ei_x_encode_long(x,v->v_sint64);
							//~ break;
							//~ //return talloc_asprintf(mem_ctx, "%lld", v->v_sint64);
						//~ case CIM_UINT64: 
							//~ ei_x_encode_ulong(x,v->v_sint64);
							//~ break;
							//~ //return talloc_asprintf(mem_ctx, "%llu", v->v_sint64);
						//~ case CIM_REAL32: 
							//~ ei_x_encode_double(x,v->v_uint32);
							//~ break;
							//~ //return talloc_asprintf(mem_ctx, "%f", (double)v->v_uint32);
						//~ case CIM_REAL64: 
							//~ ei_x_encode_double(x,v->v_uint64);
							//~ break;
							//~ //return talloc_asprintf(mem_ctx, "%f", (double)v->v_uint64);
						case CIM_BOOLEAN: 
							s = talloc_asprintf(mem_ctx, "%s", v->v_boolean?"True":"False");
							ei_x_encode_atom(x, s);
							break;
						case CIM_STRING:
						case CIM_DATETIME:
						case CIM_REFERENCE: 
							s = talloc_asprintf(mem_ctx, "%s", v->v_string);
							ei_x_encode_binary(x, s,strlen(s)); 
							break;
						case CIM_CHAR16: 
							ei_x_encode_atom(x, "undefined");
							break;
							//return talloc_asprintf(mem_ctx, "Unsupported");
						case CIM_OBJECT: 
							ei_x_encode_atom(x, "undefined");
							break;
							//return talloc_asprintf(mem_ctx, "Unsupported");
						case CIM_ARR_SINT8: 
						case CIM_ARR_UINT8:
						case CIM_ARR_SINT16: 
						case CIM_ARR_UINT16: 
						case CIM_ARR_SINT32:
						case CIM_ARR_UINT32: 
						case CIM_ARR_SINT64: 
						case CIM_ARR_UINT64:
						case CIM_ARR_REAL32: 
						case CIM_ARR_REAL64:
						case CIM_ARR_BOOLEAN: 
						case CIM_ARR_STRING: 
						case CIM_ARR_DATETIME:
						case CIM_ARR_REFERENCE: 
						case CIM_SINT64: 							
						case CIM_UINT64: 							
						case CIM_REAL32: 							
						case CIM_REAL64:												
							s = string_CIMVAR(ctx, &co[i]->instance->data[j], co[i]->obj_class->properties[j].desc->cimtype & CIM_TYPEMASK);
							ei_x_encode_binary(x, s,strlen(s));  
							break;
						default: 
							//~ s = string_CIMVAR(ctx, &co[i]->instance->data[j], co[i]->obj_class->properties[j].desc->cimtype & CIM_TYPEMASK);
							//~ ei_x_encode_binary(x, s,strlen(s));  
							ei_x_encode_atom(x, "undefined");
							break;						
					}
		
				}	
			}			
			
		} while (ret == cnt);
		//printf( "         %d \r\n",recordcount);			
		if (recordcount == 0)
		{
			ei_x_encode_atom(x, "empty");
		}
		else
		{
			ei_x_encode_empty_list(x);
		}
	}
	ei_x_encode_empty_list(x);
	talloc_free(ctx);
	return 1;
}


