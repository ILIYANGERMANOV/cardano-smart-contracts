{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module IvyToken(
    defGovernance
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
import           Control.Monad                hiding (fmap)
import           Data.Monoid                  (Last (..))
import           Plutus.Contract              as Contract
import           Plutus.Contract.StateMachine
import           PlutusTx.Prelude             hiding (Semigroup(..), check, unless)
import           Ledger                       hiding (singleton)
import           Ledger.Ada                   as Ada
import           Ledger.Constraints           as Constraints
import qualified Ledger.Typed.Scripts         as Scripts
import           Ledger.Value
import           Wallet.Emulator.Wallet 
import           Prelude                      (Semigroup (..), Show (..), String)
import           Plutus.Trace.Emulator  as Emulator
import           Plutus.Contract.Request
import           Wallet.Emulator.Types
import           PlutusTx.Enum
import           Data.Map             as Map


-- Data
data Proposal = 
    Proposal {
        id        :: Integer,
        proposers :: [(PubKeyHash, Integer)],
        tag       :: String,
        url       :: String,
        price     :: Integer
    } | 
    GovernanceUpdate {
        id              :: Integer,
        newGovernance   :: Governance
    } 
    deriving (Show)

data VoteType =  Approve | Reject 
    deriving (Show, Enum)

data Vote = Vote {
    voter :: PubKeyHash,
    types  :: VoteType
} deriving (Show)

data Governance = Governance {
    pVotesThreshold     :: Integer,
    gVotesThreshold     :: Integer,
    maxProposalPrice    :: Integer,
    votersRewardPercent :: Integer
} deriving (Show)

data IvyDatum = Genesis | IvyDatum {
    proposals :: [Proposal],
    votes     :: Map Integer Vote
} deriving (Show)


ivyToken = "IVY"

-- Initial
defGovernance :: Governance
defGovernance = Governance {
    pVotesThreshold     = 100,
    gVotesThreshold     = 1000,
    maxProposalPrice    = 500,
    votersRewardPercent = 1
}

-- On-chain
{-# INLINABLE mkPolicy #-}
mkPolicy :: () -> ScriptContext -> Bool
mkPolicy  _ _ = True

policy :: Scripts.MintingPolicy
policy = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy $ mkPolicy||])

curSymbol :: CurrencySymbol
curSymbol = scriptCurrencySymbol $ policy


-- Off-chain
type IvyTokenSchema = Endpoint "mint" ()

mintIvy :: () -> Contract w IvyTokenSchema Text ()
mintIvy _ = do
    pkh <- Plutus.Contract.Request.ownPaymentPubKeyHash
    utxos <- utxosAt (pubKeyHashAddress pkh Nothing)
    case Map.keys utxos of
        []       -> Contract.logError @String "no utxo found"
        oref : _ -> do
            let val     = Value.singleton curSymbol ivyToken 1
                lookups = Constraints.mintingPolicy policy <> Constraints.unspentOutputs utxos
                tx      = Constraints.mustMintValue val <> Constraints.mustSpendPubKeyOutput oref
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () IvyTokenSchema Text ()
endpoints = forever
                $ awaitPromise
                $ mint'
  where
    mint' = endpoint @"mint" mintIvy
    

mkSchemaDefinitions ''IvyTokenSchema

mkKnownCurrencies []

test :: IO ()
test = runEmulatorTraceIO $ do
    h1 <- activateContractWallet (fromWalletNumber $ WalletNumber 1) endpoints
    callEndpoint @"mint" h1 ()
    void $ Emulator.waitNSlots 1
    callEndpoint @"mint" h1 ()
    void $ Emulator.waitNSlots 1