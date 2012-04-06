#!/bin/sh
ERL=/usr/lib/erlang/bin/erl
HOSTNAME='localhost'
export HEART_COMMAND="$PA/erlsec.sh start"

case $1 in
  start)
    $ERL -smp +S 8 -heart -detached +P 102400 +K true -sname erlsec \
	     -setcookie 3ren -mnesia dir '"db"' -pa $PWD/ebin $PWD/include $PWD/deps/*/ebin $PWD/deps/*/include -kernel inet_dist_listen_min 8100 -kernel inet_dist_listen_max 8200 -s esyslog
    echo "Starting ErlSyslog ..."
    ;;
 
  debug)
    echo "Starting ErlSyslog ..."
    $ERL -smp +S 8 +P 102400 +K true +W i -sname erlsec \
	     -setcookie 3ren -mnesia dir '"db"' -pa $PWD/ebin $PWD/include $PWD/deps/*/ebin $PWD/deps/*/include -kernel inet_dist_listen_min 8100 -kernel inet_dist_listen_max 8200 -s esyslog
    ;;
    
  live)
    echo "Starting ErlSyslog ..."
    $ERL -smp +S 8 +P 102400 +K true +W i -sname erlsec \
	     -setcookie 3ren -mnesia dir '"db"' -pa $PWD/ebin $PWD/include $PWD/deps/*/ebin $PWD/deps/*/include  -boot start_sasl -kernel inet_dist_listen_min 8100 -kernel inet_dist_listen_max 8200 -s esyslog
    ;;  
 
  stop)
    $ERL -noshell -sname appserver_stopper  \
	     -setcookie 3ren  -pa $PWD/ebin -s erlnode stop erlsec@$HOSTNAME -kernel inet_dist_listen_min 8100 -kernel inet_dist_listen_max 8200 	 		   
    echo "Stopping ErlSyslog...    "
    ;;
 
  *)
    echo "Usage: $0 {start|stop|debug}"
    exit 1
esac
 
exit 0