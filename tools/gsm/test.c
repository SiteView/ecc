#include "erl_driver.h"

typedef struct {
    ErlDrvPort port;
 }GsmOperate_data;

static ErlDrvData GsmOperate_driver_start(ErlDrvPort port, char *buff)
{                                   
    GsmOperate_data* d = (GsmOperate_data*)driver_alloc(sizeof(GsmOperate_data));
    d->port = port;
    return (ErlDrvData)d;
}

static void GsmOperate_driver_stop(ErlDrvData handle)
{
    driver_free((char*)handle);
}


static void GsmOperate_driver_output(ErlDrvData handle, char *buff, int bufflen)
{
    GsmOperate_data* d = (GsmOperate_data*)handle;
    char fn = buff[0];
    char *res;
    //const char* pPort = buff[1]; //?????????
    res =  "test";    
    driver_output(d->port, &res, 1);
}

ErlDrvEntry GsmOperate_driver_entry = {
	NULL, /* F_PTR init, N/A */
	GsmOperate_driver_start, /* L_PTR start, called when port is opened */
	GsmOperate_driver_stop, /* F_PTR stop, called when port is closed */
	GsmOperate_driver_output, /* F_PTR output, called when erlang has sent
	data to the port */
	NULL, /* F_PTR ready_input,
	called when input descriptor ready to read*/
	NULL, /* F_PTR ready_output,
	called when output descriptor ready to write */
	"test" , /* char *driver_name, the argument to open_port */
	NULL, /* F_PTR finish, called when unloaded */
	NULL, /* F_PTR control, port_command callback */
	NULL, /* F_PTR timeout, reserved */
	NULL /* F_PTR outputv, reserved */
};


DRIVER_INIT(GsmOperate) /* must match name in driver_entry */
{
    return &GsmOperate_driver_entry;
}
