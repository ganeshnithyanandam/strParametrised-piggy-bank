#!/bin/bash
export Dad=$(curl -s -H "Content-Type: application/json" -X POST -d '{"caID": "StrParameterisedPiggyBankContract", "caWallet":{"getWallet": 1}}' http://localhost:9080/api/contract/activate | jq .unContractInstanceId | tr -d '"')
export Mom=$(curl -s -H "Content-Type: application/json" -X POST -d '{"caID": "StrParameterisedPiggyBankContract", "caWallet":{"getWallet": 2}}' http://localhost:9080/api/contract/activate | jq .unContractInstanceId | tr -d '"')
export Jack=$(curl -s -H "Content-Type: application/json" -X POST -d '{"caID": "StrParameterisedPiggyBankContract", "caWallet":{"getWallet": 3}}' http://localhost:9080/api/contract/activate | jq .unContractInstanceId | tr -d '"')
export Jill=$(curl -s -H "Content-Type: application/json" -X POST -d '{"caID": "StrParameterisedPiggyBankContract", "caWallet":{"getWallet": 4}}' http://localhost:9080/api/contract/activate | jq .unContractInstanceId | tr -d '"')

sleep 4
curl -H "Content-Type: application/json" -X POST \
-d '{"pbaBeneficiaryName":"Jack", "pbaAmount":2000000}' \
http://localhost:9080/api/contract/instance/$Jack/endpoint/logPkh && sleep 1

curl -H "Content-Type: application/json" -X POST \
-d '{"pbaBeneficiaryName":"Jack", "pbaAmount":2000000}' \
http://localhost:9080/api/contract/instance/$Jill/endpoint/logPkh && sleep 4

#Put
curl -H "Content-Type: application/json" -X POST \
-d '{ "pbaBeneficiaryName":"Jack", "pbaAmount":2000000}' \
http://localhost:9080/api/contract/instance/$Dad/endpoint/put && sleep 4

curl -H "Content-Type: application/json" -X POST \
-d '{ "pbaBeneficiaryName":"Jill", "pbaAmount":3000000}' \
http://localhost:9080/api/contract/instance/$Mom/endpoint/put && sleep 4

curl -H "Content-Type: application/json" -X POST \
-d '{ "pbaBeneficiaryName":"Jack", "pbaAmount":2000000}' \
http://localhost:9080/api/contract/instance/$Dad/endpoint/inspect && sleep 4

curl -H "Content-Type: application/json" -X POST \
-d '{ "pbaBeneficiaryName":"Jack", "pbaAmount":2000000}' \
http://localhost:9080/api/contract/instance/$Jack/endpoint/empty && sleep 4

curl -H "Content-Type: application/json" -X POST \
-d '{ "pbaBeneficiaryName":"Jack", "pbaAmount":2000000}' \
http://localhost:9080/api/contract/instance/$Jack/endpoint/inspect && sleep 4

curl -H "Content-Type: application/json" -X POST \
-d '{ "pbaBeneficiaryName":"Jill", "pbaAmount":2000000}' \
http://localhost:9080/api/contract/instance/$Jill/endpoint/empty && sleep 4