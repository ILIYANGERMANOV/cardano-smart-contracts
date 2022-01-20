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


module IvyToken(
    defGovernance,
    Proposal (..),
    Governance (..),
    VoteType (..),
    test,
    curSymbol
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


-- Data
data Governance = Governance {
    pVotesThreshold     :: Integer,
    gVotesThreshold     :: Integer,
    maxProposalPrice    :: Integer,
    votersRewardPercent :: Integer
} deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

PlutusTx.unstableMakeIsData ''Governance
PlutusTx.makeLift ''Governance

data Proposal = 
    Proposal {
        id        :: !Integer,
        proposers :: ![(PubKeyHash, Integer)],
        tag       :: !BuiltinByteString,
        url       :: !BuiltinByteString,
        price     :: !Integer
    } 
    | 
    GovernanceUpdate {
        id              :: !Integer,
        newGovernance   :: !Governance
    } 
    deriving (Show, Generic, FromJSON, ToJSON)

PlutusTx.unstableMakeIsData ''Proposal
PlutusTx.makeLift ''Proposal

data VoteType =  Approve | Reject 
    deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Enum)

PlutusTx.unstableMakeIsData ''VoteType
PlutusTx.makeLift ''VoteType

data Vote = Vote {
    voter   :: !PubKeyHash,
    types   :: !VoteType
} deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

PlutusTx.unstableMakeIsData ''Vote
PlutusTx.makeLift ''Vote

data IvyDatum = Genesis | IvyDatum {
    proposals :: ![Proposal],
    votes     :: ![(Integer, Vote)]
} deriving (Show, Generic, FromJSON, ToJSON)

PlutusTx.unstableMakeIsData ''IvyDatum
PlutusTx.makeLift ''IvyDatum

data IvyReedemer = RedeemWIP
    deriving (Show, Generic, FromJSON, ToJSON)

PlutusTx.unstableMakeIsData ''IvyReedemer
PlutusTx.makeLift ''IvyReedemer

ivyToken = "IVY_T1"

-- Initial -------------------------
defGovernance :: Governance
defGovernance = Governance {
    pVotesThreshold     = 100,
    gVotesThreshold     = 1000,
    maxProposalPrice    = 500,
    votersRewardPercent = 1
}

-- On-chain
{-# INLINABLE mkValidator #-}
mkValidator :: IvyDatum -> () -> ScriptContext -> Bool
mkValidator  _ _ _ = True


data IvyTokenContract
instance Scripts.ValidatorTypes IvyTokenContract where
    type instance DatumType IvyTokenContract = IvyDatum
    type instance RedeemerType IvyTokenContract = ()

ivyTypedValidator :: Scripts.TypedValidator IvyTokenContract
ivyTypedValidator = Scripts.mkTypedValidator @IvyTokenContract
        $$(PlutusTx.compile [|| mkValidator ||])
        $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator @IvyDatum @()

validator :: Validator
validator = Scripts.validatorScript ivyTypedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash ivyTypedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

ivyMintingPolicy :: Scripts.MintingPolicy
ivyMintingPolicy = Scripts.forwardingMintingPolicy ivyTypedValidator

curSymbol :: CurrencySymbol
curSymbol = scriptCurrencySymbol $ ivyMintingPolicy

-- Off-chain
type IvyTokenSchema = Endpoint "mint" ()

mintIvy :: () -> Contract w IvyTokenSchema Text ()
mintIvy _ = do
    let val = Value.singleton curSymbol ivyToken 1
        lookups = Constraints.mintingPolicy ivyMintingPolicy
        tx      = Constraints.mustMintValue val
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ printf "forged %s" (show val)


endpoints :: Contract () IvyTokenSchema Text ()
endpoints = forever
                $ awaitPromise
                $ mint'
  where
    mint' = endpoint @"mint" mintIvy
    

-- Playground
mkSchemaDefinitions ''IvyTokenSchema

myToken :: KnownCurrency
myToken = KnownCurrency (ValidatorHash "f") "Token" (TokenName "T" :| [])

mkKnownCurrencies ['myToken]

-- Testing
test :: IO ()
test = runEmulatorTraceIO $ do
    h1 <- activateContractWallet (fromWalletNumber $ WalletNumber 1) endpoints
    callEndpoint @"mint" h1 ()
    void $ Emulator.waitNSlots 1
    -- callEndpoint @"mint" h1 ()
    -- void $ Emulator.waitNSlots 1