{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Homework1 where

import           Plutus.V1.Ledger.Interval (contains)
import           Plutus.V2.Ledger.Api (BuiltinData, POSIXTime, PubKeyHash,
                                            ScriptContext (scriptContextTxInfo),
                                            TxInfo (txInfoValidRange),
                                            Validator, from,to, mkValidatorScript)  

import           Plutus.V2.Ledger.Contexts (txSignedBy,txInfoSignatories)                                                                              
import           PlutusTx             (compile, unstableMakeIsData)
import           PlutusTx.Prelude     (Bool (..),BuiltinData,traceIfFalse,($),Eq ((==)),(&&),(||),(+),otherwise,elem)
import           Utilities            (wrapValidator)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data VestingDatum = VestingDatum
    { beneficiary1 :: PubKeyHash
    , beneficiary2 :: PubKeyHash
    , deadline     :: POSIXTime
    }

unstableMakeIsData ''VestingDatum

{-# INLINABLE mkVestingValidator #-}
-- This should validate if either beneficiary1 has signed the transaction and the current slot is before or at the deadline
-- or if beneficiary2 has signed the transaction and the deadline has passed.
mkVestingValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkVestingValidator dat () ctx = (elem (beneficiary1 dat) signedBy && deadlineReached1)  
                              ||  (elem (beneficiary2 dat) signedBy && deadlineReached2)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedBy :: [PubKeyHash]
    signedBy = txInfoSignatories info 

    deadlineReached1 :: Bool
    deadlineReached1 = (contains (to ( deadline dat) ) $ txInfoValidRange info)

    deadlineReached2 :: Bool
    deadlineReached2 = (contains (from ( 1 + deadline dat )) $ txInfoValidRange info)
    

    

{-# INLINABLE  mkWrappedVestingValidator #-}
mkWrappedVestingValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedVestingValidator = wrapValidator mkVestingValidator

validator :: Validator
validator = mkValidatorScript $$(compile [|| mkWrappedVestingValidator ||])
