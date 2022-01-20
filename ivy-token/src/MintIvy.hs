{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE NumericUnderscores #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}


module MintIvy(
    test
) where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.List.NonEmpty   (NonEmpty (..))
import           Data.Map             as Map
import           Data.Text            (pack, Text)
import           GHC.Generics         (Generic)
import           Ledger               hiding (singleton)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Value         as Value
import           Ledger.Ada           as Ada
import           Playground.Contract  (IO, ensureKnownCurrencies, printSchemas, stage, printJson)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import qualified PlutusTx
import           Schema               (ToSchema)
import           Text.Printf          (printf)
import           Data.Monoid                  (Last (..))
import           Plutus.Contract              as Contract
import           PlutusTx.Prelude             hiding (Semigroup(..), check, unless)
import           Ledger                       hiding (singleton)
import           Ledger.Value
import           Wallet.Emulator.Wallet 
import           Prelude                      (Semigroup (..), Show (..), String)
import           Plutus.Trace.Emulator  as Emulator
import           Plutus.Contract.Request
import           Wallet.Emulator.Types
import           PlutusTx.Enum
import           Data.Map             as Map
import           Ledger.Constraints
import           Data.Void           (Void)


ivyToken = "IVY_T1"

-- On-chain
{-# INLINABLE mkPolicy #-}
mkPolicy :: () -> ScriptContext -> Bool
mkPolicy () ctx = True

policy :: Scripts.MintingPolicy
policy = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy mkPolicy ||])

curSymbol ::  CurrencySymbol
curSymbol = scriptCurrencySymbol policy


-- Off-chain
data MintParams = MintParams {
    mpAmount :: !Integer
} deriving (Generic, ToJSON, FromJSON, ToSchema)

type MintIvySchema = Endpoint "mint" ()

mintIvy :: () -> Contract w MintIvySchema Text ()
mintIvy _ = do
    let val     = Value.singleton curSymbol ivyToken 1_000_000
        lookups = Constraints.mintingPolicy $ policy
        tx      = Constraints.mustMintValue val
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () MintIvySchema Text ()
endpoints = forever
            $ awaitPromise
            $ mint'
    where
        mint' = endpoint @"mint" mintIvy
    

-- Testing
test :: IO ()
test = runEmulatorTraceIO $ do
    h1 <- activateContractWallet (fromWalletNumber $ WalletNumber 1) endpoints
    callEndpoint @"mint" h1 ()
    void $ Emulator.waitNSlots 1
