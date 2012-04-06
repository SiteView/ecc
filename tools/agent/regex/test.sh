ERL=/usr/lib/erlang/bin/erl
HOSTNAME='localhost'
export HEART_COMMAND="$PWD/test.sh start"

case $1 in
  start)
    $ERL -smp -heart -detached -sname test -setcookie 3ren -pa $PWD/ebin -boot start_sasl -s test
    echo  "Starting ..."
    ;;
 
  debug)
    $ERL -smp -sname test -setcookie 3ren -pa $PWD/ebin -boot start_sasl -s test
    ;;
    
  live)
    $ERL -smp -sname test -setcookie 3ren -pa $PWD/ebin -s test
    ;;  
 
  stop)
    echo "Stopping ..."
    $ERL -noshell -sname erlnode  \
	     -setcookie 3ren  -pa $PWD/ebin -s erlnode stop test@$HOSTNAME	
    ;;
 
  *)
    echo "Usage: $0 {start|stop|debug|live}"
    exit 1
esac
 
exit 0