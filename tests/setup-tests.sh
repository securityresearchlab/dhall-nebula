# echo setup cluster
# az group create --name aksResources --location eastus
# az aks create -g aksResources -n aksCluster --enable-managed-identity --node-count 7 --node-vm-size "Standard_B4ms"
# az aks get-credentials --resource-group aksResources --name aksCluster

# get list of node names
NODES=`kubectl get nodes -o jsonpath='{range .items[*]}{.metadata.name}'`
# echo get vm (1) name
VM1=`echo ${NODES} | head -n 2 | tail -n 1`

# echo execute lighthouse on vm (1) outside of cluster
# echo execute nebula "server" on vm (2) outside of cluster
# echo execute requests server on other vm (2) outside of cluster
# echo execute results server on other vm (2) outside of cluster

# echo create 50 client deployment (initial deployment)
kubectl apply -f ./client-deployment.yaml
# echo wait test time (20 minutes)
sleep 1320
# echo recover test results from vm (2)
# echo increase deployment replicas to 100
kubectl scale statefulste client-deployment --replicas=100
# echo wait test time (20 minutes)
sleep 1320
# echo recover test results from vm (2)
# echo increase deployment replicas to 150
kubectl scale statefulste client-deployment --replicas=150
# echo wait test time (20 minutes)
sleep 1320
# echo recover test results from vm (2)
# echo increase deployment replicas to 200
kubectl scale statefulste client-deployment --replicas=200
# echo wait test time (20 minutes)
sleep 1320
# echo recover test results from vm (2)
# echo increase deployment replicas to 250
kubectl scale statefulste client-deployment --replicas=250
# echo wait test time (20 minutes)
sleep 1320
# echo recover test results from vm (2)
# echo increase deployment replicas to 300
kubectl scale statefulste client-deployment --replicas=300
# echo wait test time (20 minutes)
sleep 1320
# echo recover test results from vm (2)
# echo increase deployment replicas to 350
kubectl scale statefulste client-deployment --replicas=350
# echo wait test time (20 minutes)
sleep 1320
# echo recover test results from vm (2)
# echo increase deployment replicas to 400
kubectl scale statefulste client-deployment --replicas=400
# echo wait test time (20 minutes)
sleep 1320
# echo recover test results from vm (2)
# echo increase deployment replicas to 450
kubectl scale statefulste client-deployment --replicas=450
# echo wait test time (20 minutes)
sleep 1320
# echo recover test results from vm (2)
# echo increase deployment replicas to 500
kubectl scale statefulste client-deployment --replicas=500
# echo wait test time (20 minutes)
sleep 1320
# echo recover test results from vm (2)
# echo increase deployment replicas to 550
kubectl scale statefulste client-deployment --replicas=550
# echo wait test time (20 minutes)
sleep 1320
# echo recover test results from vm (2)
# echo increase deployment replicas to 600
kubectl scale statefulste client-deployment --replicas=600
# echo wait test time (20 minutes)
sleep 1320
# echo recover test results from vm (2)
# echo increase deployment replicas to 650
kubectl scale statefulste client-deployment --replicas=650
# echo wait test time (20 minutes)
sleep 1320
# echo recover test results from vm (2)
# echo increase deployment replicas to 700
kubectl scale statefulste client-deployment --replicas=700
# echo wait test time (20 minutes)
sleep 1320
# echo recover test results from vm (2)
# echo increase deployment replicas to 750
kubectl scale statefulste client-deployment --replicas=750
# echo wait test time (20 minutes)
sleep 1320
# echo recover test results from vm (2)
# echo check presence of all test results
ssh -i ~/.ssh/lighthouse_key.pem gio@20.63.142.142 echo $(( `ls -1 /home/gio/results/50 | wc -l` == 50)) >> checks
ssh -i ~/.ssh/lighthouse_key.pem gio@20.63.142.142 echo $(( `ls -1 /home/gio/results/100 | wc -l` == 100)) >> checks
ssh -i ~/.ssh/lighthouse_key.pem gio@20.63.142.142 echo $(( `ls -1 /home/gio/results/150 | wc -l` == 150)) >> checks
ssh -i ~/.ssh/lighthouse_key.pem gio@20.63.142.142 echo $(( `ls -1 /home/gio/results/200 | wc -l` == 200)) >> checks
ssh -i ~/.ssh/lighthouse_key.pem gio@20.63.142.142 echo $(( `ls -1 /home/gio/results/250 | wc -l` == 250)) >> checks
ssh -i ~/.ssh/lighthouse_key.pem gio@20.63.142.142 echo $(( `ls -1 /home/gio/results/300 | wc -l` == 300)) >> checks
ssh -i ~/.ssh/lighthouse_key.pem gio@20.63.142.142 echo $(( `ls -1 /home/gio/results/350 | wc -l` == 350)) >> checks
ssh -i ~/.ssh/lighthouse_key.pem gio@20.63.142.142 echo $(( `ls -1 /home/gio/results/400 | wc -l` == 400)) >> checks
ssh -i ~/.ssh/lighthouse_key.pem gio@20.63.142.142 echo $(( `ls -1 /home/gio/results/450 | wc -l` == 450)) >> checks
ssh -i ~/.ssh/lighthouse_key.pem gio@20.63.142.142 echo $(( `ls -1 /home/gio/results/500 | wc -l` == 500)) >> checks
ssh -i ~/.ssh/lighthouse_key.pem gio@20.63.142.142 echo $(( `ls -1 /home/gio/results/550 | wc -l` == 550)) >> checks
ssh -i ~/.ssh/lighthouse_key.pem gio@20.63.142.142 echo $(( `ls -1 /home/gio/results/600 | wc -l` == 600)) >> checks
ssh -i ~/.ssh/lighthouse_key.pem gio@20.63.142.142 echo $(( `ls -1 /home/gio/results/650 | wc -l` == 650)) >> checks
ssh -i ~/.ssh/lighthouse_key.pem gio@20.63.142.142 echo $(( `ls -1 /home/gio/results/700 | wc -l` == 700)) >> checks
ssh -i ~/.ssh/lighthouse_key.pem gio@20.63.142.142 echo $(( `ls -1 /home/gio/results/750 | wc -l` == 750)) >> checks
if [[ $((`cat checks | grep -c 1` == `cat checks | wc -l`)) ]]; then
    echo missing results, please act
else
    echo ok, destroying cluster
    # echo destroy cluster
    az aks delete --name aksCluster -g aksResources
fi
