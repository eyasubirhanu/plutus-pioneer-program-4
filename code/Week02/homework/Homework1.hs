{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}

module Homework1 where

import qualified Plutus.V2.Ledger.Api as PlutusV2
import           PlutusTx             (compile)
import           PlutusTx.Prelude     (Bool (..), BuiltinData,traceIfFalse,($),Eq ((==)),(&&))
import           Utilities            (wrapValidator)
import           Prelude              (IO)
---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

{-# INLINABLE mkValidator #-}
-- This should validate if and only if the two Booleans in the redeemer are True!
mkValidator :: () -> (Bool, Bool) -> PlutusV2.ScriptContext -> Bool
mkValidator _ (a,b) _ = (a && b) ==True

wrappedVal :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedVal = wrapValidator mkValidator


validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| wrappedVal ||])
