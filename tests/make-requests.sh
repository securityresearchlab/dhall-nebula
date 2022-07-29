echo client $1
BINS=`echo "obase=2;$1" | bc`
PADDED=`printf %16s $BINS | tr ' ' 0`
N_IP3=$((2#${PADDED:0:8}))
N_IP4=$((2#${PADDED:8:16}))
echo nebula ip _._.$N_IP3.$N_IP4

NAME=${N_IP3}${N_IP4}

sleep 300 # wait until server is reachable

echo "starting requests"
start=`date +%s`
while [[ $((`date +%s` - $start)) -le $(( $2 * 60)) ]]; do
    curl -w "%{response_code};%{time_total}\n" -o /dev/null -s --max-time 10 http://192.168.0.2:8080 >> $NAME.txt &
    sleep 0.25s
done

echo "upload results"
curl -F "file=@$NAME.txt" -X POST http://192.168.0.2:8090/$NAME
