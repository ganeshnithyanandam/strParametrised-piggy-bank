#!/bin/bash
export W1=$(curl -s -H "Content-Type: application/json" -X POST -d '{"caID": "StrParameterisedPiggyBankContract", "caWallet":{"getWallet": 1}}' http://localhost:9080/api/contract/activate | jq .unContractInstanceId | tr -d '"')
export W2=$(curl -s -H "Content-Type: application/json" -X POST -d '{"caID": "StrParameterisedPiggyBankContract", "caWallet":{"getWallet": 2}}' http://localhost:9080/api/contract/activate | jq .unContractInstanceId | tr -d '"')
export W3=$(curl -s -H "Content-Type: application/json" -X POST -d '{"caID": "StrParameterisedPiggyBankContract", "caWallet":{"getWallet": 3}}' http://localhost:9080/api/contract/activate | jq .unContractInstanceId | tr -d '"')
export W4=$(curl -s -H "Content-Type: application/json" -X POST -d '{"caID": "StrParameterisedPiggyBankContract", "caWallet":{"getWallet": 4}}' http://localhost:9080/api/contract/activate | jq .unContractInstanceId | tr -d '"')

export DAD=${W1}
export MOM=${W2}
export Jack=${W3}
export Jill=${W4}
#Inspect Jack's and Jill's contract instances to find the pkh
#Use the pkh in run-2.sh

curl -H "Content-Type: application/json" -X POST -d "true" http://localhost:9080/api/contract/instance/${Jack}/endpoint/logPkh && sleep 4
