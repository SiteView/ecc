echo "Starting test ..."
erl  -heart -env HEART_COMMAND "start.bat" +P 500000 +A 10 -sname wmi -setcookie 3ren  -pa ebin  -s test
  