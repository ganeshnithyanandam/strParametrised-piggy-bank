{-# LANGUAGE DataKinds                     #-}
{-# LANGUAGE DeriveAnyClass                #-}
{-# LANGUAGE DeriveGeneric                 #-}
{-# LANGUAGE FlexibleContexts              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving    #-}
{-# LANGUAGE MultiParamTypeClasses         #-}
{-# LANGUAGE NoImplicitPrelude             #-}
{-# LANGUAGE OverloadedStrings             #-}
{-# LANGUAGE ScopedTypeVariables           #-}
{-# LANGUAGE TemplateHaskell               #-}
{-# LANGUAGE TypeApplications              #-}
{-# LANGUAGE TypeFamilies                  #-}
{-# LANGUAGE TypeOperators                 #-}
{-# OPTIONS_GHC -fno-warn-unused-imports   #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Plutus.Contracts.StrParameterisedPiggyBank
(inValue,checkAmount,endpoints, StrParameterisedPiggyBankSchema, {-MyRedeemer (..)-}) where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.Map             as Map hiding (empty)
import           Data.Text            (Text, unpack)
import           Data.Monoid          (Last (..))
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import           Plutus.Contract
import           PlutusTx             (toBuiltinData)
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import qualified PlutusTx.Prelude     as Plutus
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO, Semigroup (..), String)
import qualified Prelude
import           Text.Printf          (printf)
import           Data.Text.Prettyprint.Doc.Extras (PrettyShow (..))
import           Prelude              (Semigroup (..), Show (..), Eq)
import           Plutus.Contract       as Contract
import           Plutus.V1.Ledger.Value (Value (..), assetClass, assetClassValueOf)
import qualified PlutusTx.Builtins.Class    as BC

data InParam = InParam
    { beneficiaryName :: !BuiltinString
    } deriving (Show,Prelude.Eq)

{-PlutusTx.unstableMakeIsData ''InParam-}
PlutusTx.makeLift ''InParam


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

data PBArg = PBArg
    { pbaBeneficiaryName :: !String
    , pbaAmount          :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type StrParameterisedPiggyBankSchema =
            Endpoint "put" PBArg
        .\/ Endpoint "empty" PBArg
        .\/ Endpoint "inspect" PBArg
        .\/ Endpoint "logPkh" PBArg

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
    ledgerTx <- submitTxConstraints (typedValidator p) tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "Put %d lovelaces in the piggy bank" <> show $ pbaAmount pba

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

logPkh :: forall w s e. AsContractError e => PBArg -> Contract w s e ()
logPkh _ =
  do
     pkh <- pubKeyHash <$> ownPubKey
     logInfo @String
         $ "Public key hash is : "
             <> show pkh
     logInfo @String $ "logPkh complete"

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
