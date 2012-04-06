#!/bin/sh
ERL=erl
HOSTNAME='localhost'
export HEART_COMMAND="$PWD/svecc.sh start"

# licnese lib search
LICENSE=/usr/lib/license.so
if [ ! -f "$LICENSE" ]
then
cp $PWD/priv/lib/license.so $LICENSE
fi


case $1 in
  start)
    $ERL +S 8 -heart -detached +P 500000 +K true -sname eccng -setcookie 3ren -boot start_sasl \
        -env ERL_MAX_ETS_TABLES 20000 -env GETTEXT_DIR $PWD/additionmod/erlang-gettext/priv \
        -pa $PWD/additionmod/erlang-gettext/ebin $PWD/core/utils/GsmOperateUtils $PWD/additionmod/erlang-gettext/include $PWD/additionmod/erlsoap/ebin \
        -pa $PWD/additionmod/erlsom-1.2.1/ebin $PWD/iconv $PWD/core/ebin $PWD/plugin/ebin $PWD/modules/df_snmp/ebin $PWD/modules/nmap_scan/ebin $PWD/modules/df_snmp/ebin \
        -pa $PWD/modules/snmp/ebin $PWD/modules/erlcmdb/ebin $PWD/ssh/ebin $PWD/nitrogen/ebin $PWD/nitrogen/include  $PWD/rest/ebin $PWD/rest/deps \
	-pa $PWD/sec/ebin \
        -eval "application:start(quickstart_mochiweb),application:start(svecc),application:start(crypto),application:start(gettext),gettext:recreate_db(),extension_sup:start(),application:start(test)"
    echo "Starting Eccng..."
    ;;

  live)
    $ERL +S 8  +P 500000 +K true -sname eccng -setcookie 3ren -boot start_sasl \
        -env ERL_MAX_ETS_TABLES 20000 -env GETTEXT_DIR $PWD/additionmod/erlang-gettext/priv \
        -pa $PWD/additionmod/erlang-gettext/ebin $PWD/core/utils/GsmOperateUtils $PWD/additionmod/erlang-gettext/include $PWD/additionmod/erlsoap/ebin \
        -pa $PWD/additionmod/erlsom-1.2.1/ebin $PWD/iconv $PWD/core/ebin $PWD/plugin/ebin $PWD/modules/df_snmp/ebin $PWD/modules/nmap_scan/ebin $PWD/modules/df_snmp/ebin \
        -pa $PWD/modules/snmp/ebin $PWD/modules/erlcmdb/ebin $PWD/ssh/ebin $PWD/nitrogen/ebin $PWD/nitrogen/include  $PWD/rest/ebin $PWD/rest/deps \
        -pa $PWD/sec/ebin \
        -eval "application:start(quickstart_mochiweb),application:start(svecc),application:start(crypto),application:start(gettext),gettext:recreate_db(),extension_sup:start(),application:start(test)"
    echo "Starting Eccng..."
    ;;
 
  stop)
    $ERL -noshell -sname appserver_stopper  \
	     -setcookie 3ren  -pa $PWD/rest/ebin -s test stop eccng@$HOSTNAME -kernel inet_dist_listen_min 8100 -kernel inet_dist_listen_max 8200 	 		   
    echo "Stopping Eccng...    "
    ;;
 
  *)
    echo "Usage: $0 {start|stop}"
    exit 1
esac
 
exit 0
