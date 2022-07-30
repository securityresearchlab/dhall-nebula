echo client $1
BINS=`echo "obase=2;$1" | bc`
PADDED=`printf %16s $BINS | tr ' ' 0`
N_IP3=$((2#${PADDED:0:8}))
N_IP4=$((2#${PADDED:8:16}))
echo nebula ip _._.$N_IP3.$N_IP4

NAME=${N_IP3}${N_IP4}

echo "now waiting for nebula to connect"
curl --max-time 10 http://$3:8080
while [[ "$?" == 28 ]]; do echo "waiting for nebula to connect"; sleep 20; curl --max-time 10 http://$3:8080; done

echo "starting requests"
start=`date +%s`
while [[ $((`date +%s` - $start)) -le $(( $2 * 60)) ]]; do
    echo -n `date +%s`';' >> $NAME.txt && curl -w "%{response_code};%{time_total}\n" -o /dev/null -s --max-time 10 http://$3:8080 >> $NAME.txt &
    sleep 0.25s
done

wait $!
sleep 30
echo "upload results"
curl -F "file=@$NAME.txt" -X POST http://$3:8090/$NAME
