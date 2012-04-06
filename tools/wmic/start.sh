ERL=/usr/lib/erlang/bin/erl
HOSTNAME='localhost'
export HEART_COMMAND="$PWD/start.sh start"

case $1 in
  start)
    $ERL +P 500000 +A 200 -smp -heart -detached -sname wmi -setcookie 3ren -pa $PWD/ebin -boot start_sasl -s test -kernel inet_dist_listen_min 8100 -kernel inet_dist_listen_max 8200
    echo  "Starting ..."
    ;;
 
  debug)
    $ERL +P 500000 +A 200 -smp -sname wmi -setcookie 3ren -pa $PWD/ebin -boot start_sasl -s test -kernel inet_dist_listen_min 8100 -kernel inet_dist_listen_max 8200
    ;;
    
  live)
    $ERL +P 500000 +A 200 -smp -sname wmi -setcookie 3ren -pa $PWD/ebin -s test -kernel inet_dist_listen_min 8100 -kernel inet_dist_listen_max 8200
    ;;  
 
  stop)
    echo "Stopping ..."
    $ERL -noshell -sname erlnode  \
	     -setcookie 3ren  -pa $PWD/ebin -s erlnode stop wmi@$HOSTNAME -kernel inet_dist_listen_min 8100 -kernel inet_dist_listen_max 8200	
    ;;
 
  *)
    echo "Usage: $0 {start|stop|debug|live}"
    exit 1
esac
 
exit 0
