{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Homework2 where

import           Plutus.V1.Ledger.Interval (contains)
import           Plutus.V2.Ledger.Api (BuiltinData, POSIXTime, PubKeyHash,TxInfo (txInfoValidRange),scriptContextTxInfo,
                                       ScriptContext, Validator,
                                       mkValidatorScript,from)
import           PlutusTx             (applyCode, compile, liftCode)

import           Plutus.V2.Ledger.Contexts (txSignedBy,txInfoSignatories)   
import           PlutusTx.Prelude     (Bool (False), (.),($),(&&))
import           Utilities            (wrapValidator)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

{-# INLINABLE mkParameterizedVestingValidator #-}
-- This should validate if the transaction has a signature from the parameterized beneficiary and the deadline has passed.
mkParameterizedVestingValidator :: PubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkParameterizedVestingValidator beneficiary deadline () ctx = signedBybeneficiary && deadlineReached
      where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedBybeneficiary :: Bool
    signedBybeneficiary = txSignedBy info beneficiary

    deadlineReached :: Bool
    deadlineReached = (contains (from ( deadline ) ) $ txInfoValidRange info)


    

{-# INLINABLE  mkWrappedParameterizedVestingValidator #-}
mkWrappedParameterizedVestingValidator :: PubKeyHash -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedParameterizedVestingValidator = wrapValidator . mkParameterizedVestingValidator

validator :: PubKeyHash -> Validator
validator beneficiary = mkValidatorScript ($$(compile [|| mkWrappedParameterizedVestingValidator ||]) `applyCode` liftCode beneficiary)



     