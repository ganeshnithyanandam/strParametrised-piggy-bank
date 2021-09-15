
### Introduction
This smart contract demonstrates use of paramterisation in Plutus contracts. There are piggy banks are parametrised by the name of the beneficiary. The use case is built upon two kids Jack and Jill who have their own piggy banks. Dad and Mom can put money to either of their piggy banks. Jack and Jill can withdraw. Here we use the names as the parameters whereas in practice it will be the public key hash. Additionally, the piggy bank allows emptying only when lovelace > 1M is accumulated.

### On chain code
Here we intend to have different script addresses for Jack and Jill, so that they have their own piggy banks. To achieve that, we use the concept of parametrised validator. 

```haskell

data InParam = InParam
    { beneficiaryName :: !BuiltinString
    } deriving (Show,Prelude.Eq)

PlutusTx.makeLift ''InParam

{-# INLINABLE mkValidator #-}
mkValidator :: InParam -> () -> () -> ScriptContext -> Bool
...
````

The `makeLift` in the above code is needed lift Haskell values into corresponding Plutus script values at runtime. Here the parameter `InParam` has to be lifted using `makeLift` as it is not an instance of `PlutusTx.Lift.Class.`


The TypedValidator generated in this case will be different for different values of the parameter. The UTxOs for Jack and Jill will be at held at separate script addresses.

```haskell

data Typed
instance Scripts.ValidatorTypes Typed where
    type instance DatumType Typed    = ()
    type instance RedeemerType Typed = ()

typedValidator :: InParam -> Scripts.TypedValidator Typed
typedValidator p = Scripts.mkTypedValidator @Typed
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode p)
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @() @()

validator :: InParam -> Validator
validator = Scripts.validatorScript . typedValidator

valHash :: InParam ->  Ledger.ValidatorHash
valHash = Scripts.validatorHash . typedValidator

scrAddress :: InParam ->  Ledger.Address
scrAddress = scriptAddress . validator
````

The validator logic is that it checks if there is sufficient lovelace (1M) accumulated in order to empty it.
```haskell
{-# INLINABLE mkValidator #-}
mkValidator :: InParam -> () -> () -> ScriptContext -> Bool
mkValidator p () () ctx =
    hasSufficientAmount

    where
      contextTxInfo :: TxInfo
      contextTxInfo = scriptContextTxInfo ctx

      hasSufficientAmount :: Bool
      hasSufficientAmount =
          traceIfFalse "Sorry. Not enough lovelace" $ checkAmount $ inValue contextTxInfo

{-# INLINABLE inValue #-}
inValue :: TxInfo -> Value
inValue txInfo = valueSpent txInfo

{-# INLINABLE checkAmount #-}
checkAmount :: Value -> Bool
checkAmount val = (assetClassValueOf val $ assetClass Ada.adaSymbol Ada.adaToken) > 1000000

````

### Off chain code

First we define the input to the contract endpoints and declare the endpoints. Both endpoints will take input in the form of `PBArg`. We define a schema `StrParameterisedPiggyBankSchema` to declare the endpoints.
```haskell
data PBArg = PBArg
    { pbaBeneficiaryName :: !String
    , pbaAmount          :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type StrParameterisedPiggyBankSchema =
            Endpoint "put" PBArg
        .\/ Endpoint "empty" PBArg
        .\/ Endpoint "inspect" PBArg
        .\/ Endpoint "logPkh" PBArg

````

Now, we come to the endpoints.
#### 1. put endpoint 
The put endpoint is used to add lovelace to a piggy bank. The trnsaction constraint used here is `mustPayToOtherScript` which takes the valHash of the recipient script and a Datum, which in this use case is just `()`. st.
The amount to be added to the piggy bank is provided using the helper function `Ada.lovelaceValueOf`.

```haskell
put :: AsContractError e => PBArg -> Contract w s e ()
put pba =
  do
    let p  = InParam { beneficiaryName = BC.stringToBuiltinString $ pbaBeneficiaryName pba
                       }
    utxos <- utxoAt $ scrAddress p
    let totalVal = Plutus.foldMap (txOutValue . txOutTxOut) $ snd <$> Map.toList utxos
        numInputs = Map.size utxos
    logInfo @String $ "Putting to piggy bank currently holding "
            ++ show numInputs
            ++ " outputs with a total value of "
            ++ show totalVal
    let tx = mustPayToTheScript () $ Ada.lovelaceValueOf $ pbaAmount pba
    
```
After calling ``submitTxConstraints`` in the next line, the Plutus app runtime examines the transaction constraints ``tx`` and builds a transaction that fulfills them. The runtime then sends the transaction to the wallet, which adds enough to cover the required funds (in this case, the ada amount specified in `pbaAmount`).

```haskell
  
    ledgerTx <- submitTxConstraints (typedValidator p) tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "Put %d lovelaces in the piggy bank" <> show $ pbaAmount pba
````
#### 2. empty endpoint
This endpoint is used to empty the amount at the script address and receive it into the wallet. The piggy bank to eb emptied is determined by using the `InParam`. Here in `InParam` we have used the beneficiary name as the identifier. In practice the public key hash of the beneficiary wallet will be used.    

The constraints used here are `Constraints.unspentOutputs` to identify the utxos collected from the script address and `Constraints.otherScript` to identify the script itself through the parametrised validator.

```haskell
empty :: forall w s e. AsContractError e => PBArg -> Contract w s e ()
empty pba =
  do
    let p  = InParam{ beneficiaryName = BC.stringToBuiltinString $ pbaBeneficiaryName pba
                      }
    utxos <- utxoAt $ scrAddress p
    let totalVal = Plutus.foldMap (txOutValue . txOutTxOut) $ snd <$> Map.toList utxos
        numInputs = Map.size utxos
    logInfo @String
        $ "Attempting to empty piggy bank currently holding "
        <> show numInputs
        <> " outputs with a total value of "
        <> show totalVal
    let orefs   = fst <$> Map.toList utxos
        lookups = Constraints.unspentOutputs utxos <>
                  Constraints.otherScript (validator p)
        tx :: TxConstraints Void Void
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ toBuiltinData () | oref <- orefs]
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    handleError (\err -> Contract.logInfo $ "caught error: " ++ unpack err) $ void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ "Emptied piggy bank."
````

#### 3. inspect endpoint
This is a helper endpoint added to inspect the value held at Jack's and Jill's respective wallets.  It only computes in the wallet and does not invoke the on chain part. This approach can also helps in debugging, by mocking some of the code in the validator and printing the result. Here for example, `inspect` endpoint calculates the total lovelace in the piggy bank and similar logic is used in the validator.

```haskell
inspect :: forall w s e. AsContractError e => PBArg -> Contract w s e ()
inspect _ =
  do
    let p1  = InParam{ beneficiaryName = "Jack"
                      }
        p2  = InParam{ beneficiaryName = "Jill"
                      }
    utxos1 <- utxoAt $ scrAddress p1
    utxos2 <- utxoAt $ scrAddress p2
    let totalVal1 = Plutus.foldMap (txOutValue . txOutTxOut) $ snd <$> Map.toList utxos1
        numInputs1 = Map.size utxos1
        totalVal2 = Plutus.foldMap (txOutValue . txOutTxOut) $ snd <$> Map.toList utxos2
        numInputs2 = Map.size utxos2
    
    logInfo @String
        $ "UTXOs at script address for : " <> show (beneficiaryName p1) <> " :"
            <> show numInputs1
            <> " outputs with a total 'Value' of "
            <> show totalVal1
            <> " with total Lovelace of "
            <> show (assetClassValueOf totalVal1 $ assetClass Ada.adaSymbol Ada.adaToken)
    logInfo @String
        $ "UTXOs at script address for : " <> show (beneficiaryName p2) <> " :"
            <> show numInputs2
            <> " outputs with a total 'Value' of "
            <> show totalVal2
            <> " with total Lovelace of "
            <> show (assetClassValueOf totalVal2 $ assetClass Ada.adaSymbol Ada.adaToken)
    logInfo @String $ "Inspect complete"

````

#### 4. logPkh endpoint
This is a another helper endpoint to just print the public key hashes of Jack's and Jill's wallets. It shows that indeed different wallets are generated for them while running the PAB executable. The pkh can come to use if you want to use it as input for test cases etc.

```haskell
logPkh :: forall w s e. AsContractError e => PBArg -> Contract w s e ()
logPkh _ =
  do
     pkh <- pubKeyHash <$> ownPubKey
     logInfo @String
         $ "Public key hash is : "
             <> show pkh
     logInfo @String $ "logPkh complete"


````

```haskell
Finally we define `endpoints` exposed using the PAB as below. The `selectList` is used to monitor multiple endpoints. It will cause code to block until one of the endpoints is picked, and then execute that.

put' :: Promise () StrParameterisedPiggyBankSchema Text ()
put' = endpoint @"put" put

empty' :: Promise () StrParameterisedPiggyBankSchema Text ()
empty' = endpoint @"empty" empty

inspect' :: Promise () StrParameterisedPiggyBankSchema Text ()
inspect' = endpoint @"inspect" inspect

logPkh' :: Promise () StrParameterisedPiggyBankSchema Text ()
logPkh' = endpoint @"logPkh" logPkh

endpoints :: AsContractError e => Contract () StrParameterisedPiggyBankSchema Text e
endpoints =
  do
    logInfo @String "Waiting for put or empty."
    selectList [put', empty', inspect', logPkh'] >>  endpoints

-- these functions are used in the simulator
mkSchemaDefinitions ''StrParameterisedPiggyBankSchema
mkKnownCurrencies []

```