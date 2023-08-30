date=`date +%s`';'
test=`curl -w "%{response_code};%{time_total}\n" -o /dev/null -s --max-time 10 http://$1:8080`
echo $date$test
