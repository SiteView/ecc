echo "Starting ErlSyslog ..."
erl -smp +S 8 +P 102400 +W i -sname erlsec -setcookie 3ren -mnesia dir '"db"' -pa ebin -pa ..\store\ebin include deps/*/ebin deps/*/include   -boot start_sasl -kernel inet_dist_listen_min 8100 -kernel inet_dist_listen_max 8200 -s esyslog
  