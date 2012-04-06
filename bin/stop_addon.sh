#!/bin/sh
PATH=/usr/bin
SHOULD_B_UP_FILE_NAME=/opt/topaz/hp_ovo_addon/.topaz_addon_should_be_up

rm ${SHOULD_B_UP_FILE_NAME} >/dev/null 2>&1
PID_LIST=`ps -ef|grep tp_hpovo_addon |grep -v grep|awk '{print $2}'`
RETSTATUS=0
for PID in $PID_LIST; do
	if kill -USR1 ${PID} >/dev/null 2>&1 ; then 
		:
	else 
		RETSTATUS=1
	fi
done

exit ${RETSTATUS}

