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

module NFTContest (
    hello,
    GameDatum
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
import           PlutusTx.Prelude     hiding (unless)
import qualified Prelude              as P
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
import           Prelude                      (Semigroup (..), Show (..), uncurry)

hello:: P.String
hello = "Hello from NFTContest.hs"

data ContractParams = ContractParams {
    cpGameMaker         :: !PubKeyHash,
    cpEnterFee          :: !Integer,
    cpMaxParticipants   :: !Integer,
    cpTT                :: !(Maybe ThreadToken)
} deriving (Show, Generic, FromJSON, ToJSON, P.Eq)

data NFTState = NFTState {
    owner   :: !PubKeyHash,
    votes   :: ![PubKeyHash]
} deriving (Show)

data GameDatum = 
    GameDatum {
        gNfts           :: !(Map AssetClass NFTState),
        gEnterDeadline  :: !POSIXTime,
        gVoteDeadline   :: !POSIXTime
    } | Finished 
    deriving (Show)

data GameRedeemer = 
      Start {
          enterDeadline :: POSIXTime,
          voiteDeadline :: POSIXTime
      }
    | Enter PubKeyHash
    | Vote PubKeyHash AssetClass
    | Exit PubKeyHash AssetClass
    | Finish
    deriving (Show, P.Eq)

-- TODO Define validator

-- Define Contracts