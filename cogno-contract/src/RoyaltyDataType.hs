{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
-- Options
{-# OPTIONS_GHC -fno-strictness               #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas   #-}
{-# OPTIONS_GHC -fobject-code                 #-}
{-# OPTIONS_GHC -fno-specialise               #-}
{-# OPTIONS_GHC -fexpose-all-unfoldings       #-}
module RoyaltyDataType
  ( updateRoyaltyData
  , RoyaltyData
  , rPkhs
  , rScs
  , rInPid
  , rInTkn
  , rRate
  , rOutPid
  , rOutTkn
  , rThres
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Plutus.V2.Ledger.Api   as PlutusV2
-------------------------------------------------------------------------------
-- | Create the RoyaltyData object.
-------------------------------------------------------------------------------
data RoyaltyData = RoyaltyData
  { rPkhs   :: [PlutusV2.PubKeyHash]
  -- ^ The list of all royalty receiving public key hashes.
  , rScs    :: [PlutusV2.PubKeyHash]
  -- ^ The list of all royalty stake key hashes.
  , rInPid  :: PlutusV2.CurrencySymbol
  -- ^ The policy id for applying the royalty.
  , rInTkn  :: PlutusV2.TokenName
  -- ^ The token name for applying the royalty.
  , rRate   :: Integer
  -- ^ The royalty rate for the token.
  , rOutPid :: PlutusV2.CurrencySymbol
  -- ^ The policy id of the royalty payout.
  , rOutTkn :: PlutusV2.TokenName
  -- ^ The token name of the royalty payout.
  , rThres  :: Integer
  -- ^ The multisig threshold.
  }
PlutusTx.unstableMakeIsData ''RoyaltyData

-- The group, token, and threshold are constant but the rate and payout token can change.
updateRoyaltyData :: RoyaltyData -> RoyaltyData -> Bool
updateRoyaltyData a b = ( rPkhs  a == rPkhs  b ) &&
                        ( rScs   a == rScs   b ) &&
                        ( rInPid a == rInPid b ) &&
                        ( rInTkn a == rInTkn b ) &&
                        ( rThres a == rThres b )