PA=/raid5/3ren/source
ERL=/usr/lib/erlang/bin/erl
HOSTNAME='rest'
export HEART_COMMAND="$PA/source.sh start"

case $1 in
  start)
    $ERL -smp -heart -detached +P 102400 +K true -sname source -setcookie 3ren -pa $PA/ebin -pa $PA/deps -boot start_sasl -s test 
    echo  "Starting sourceserver"
    ;;
 
  debug)
    $ERL -sname source -setcookie 3ren -pa $PA/ebin -pa $PA/deps -boot start_sasl -s test 
    ;;
 
  stop)
    echo "Stopping sourceserver"
    $ERL -noshell -sname source_stopper -setcookie 3ren  -pa $PA/ebin -s test stop source@$HOSTNAME	
    ;;
 
  *)
    echo "Usage: $0 {start|stop|debug}"
    exit 1
esac
 
exit 0
                    
