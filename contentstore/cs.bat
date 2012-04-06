
erl +P 102400 -sname db -setcookie 3ren -mnesia dir '"./db"' -pa %cd%\ebin\ -boot start_sasl -s content_store