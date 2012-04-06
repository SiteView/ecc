
PA=/raid5/3ren/appserver
ERL=/usr/lib/erlang/bin/erl
HOSTNAME='rest'
export HEART_COMMAND="$PA/contentstore.sh start"

case $1 in
  start)
    $ERL -smp +S 1 -heart -detached +P 102400 +K true -sname sv3ren \
	     -setcookie 3ren -mnesia dir '"/raid5/3ren/contentstore/db"' -pa /raid5/3ren/contentstore/ebin/   -boot start_sasl -s content_store 
    echo  "Starting sv3ren"
    ;;
 
  debug)
    $ERL -smp +S 1 +P 102400 +K true -sname sv3ren \
	     -setcookie 3ren -mnesia dir '"/raid5/3ren/contentstore/db"' -pa /raid5/3ren/contentstore/ebin/   -boot start_sasl -s content_store 
   ;;
 
  stop)
    $ERL -noshell -sname appserver_stopper  \
	     -setcookie 3ren  -pa $PA/src/appserver/ebin -s test stop sv3ren@$HOSTNAME		 		   
    echo "Stopping sv3ren"
    ;;
 
  *)
    echo "Usage: $0 {start|stop|debug}"
    exit 1
esac
 
exit 0
                    
