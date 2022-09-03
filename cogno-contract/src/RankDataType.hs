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
module RankDataType
  ( RankData
  , rPkh
  , rSc
  , rUpVote
  , rDownVote
  , rAge
  , rCognoTxId
  , rCognoIndex
  , rType
  , checkForUpVote
  , checkForDownVote
  , checkForNewCogno
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Plutus.V2.Ledger.Api as PlutusV2
{- |
  Author   : The Ancient Kraken
  Copyright: 2022
-}
-------------------------------------------------------------------------------
-- | Create the RankData object.
-------------------------------------------------------------------------------
data RankData = RankData
  { rPkh        :: PlutusV2.PubKeyHash
  -- ^ The public key hash of the wallet.
  , rSc         :: PlutusV2.PubKeyHash
  -- ^ The stake hash of the wallet.
  , rUpVote     :: Integer
  -- ^ The up rank of the wallet.
  , rDownVote   :: Integer
  -- ^ The down rank of the wallet.
  , rAge        :: Integer
  -- ^ The age of the wallet rank.
  , rCognoTxId  :: PlutusV2.BuiltinByteString
  -- ^ The TxId of the cogno connected to this rank.
  , rCognoIndex :: Integer
  -- ^ The Index of the TxId of the congo connected to this rank.
  , rType       :: Integer
  -- ^ The type of rank the data is representing.
  }
PlutusTx.unstableMakeIsData ''RankData

-- a is old; b is new
checkForUpVote :: RankData -> RankData -> Bool
checkForUpVote a b = ( rPkh        a == rPkh        b ) &&
                     ( rSc         a == rSc         b ) &&
                     ( rUpVote a + 1 == rUpVote     b ) &&
                     ( rDownVote   a == rDownVote   b ) &&
                     ( rAge        a == rAge        b ) &&
                     ( rCognoTxId  a == rCognoTxId  b ) &&
                     ( rCognoIndex a == rCognoIndex b ) &&
                     ( rType       a == rType       b )

-- a is old; b is new
checkForDownVote :: RankData -> RankData -> Bool
checkForDownVote a b =  ( rPkh          a == rPkh        b ) &&
                        ( rSc           a == rSc         b ) &&
                        ( rUpVote       a == rUpVote     b ) &&
                        ( rDownVote a + 1 == rDownVote   b ) &&
                        ( rAge          a == rAge        b ) &&
                        ( rCognoTxId    a == rCognoTxId  b ) &&
                        ( rCognoIndex   a == rCognoIndex b ) &&
                        ( rType         a == rType       b )

-- a is old; b is new
checkForNewCogno :: RankData -> RankData -> Bool
checkForNewCogno a b =  ( rPkh       a == rPkh        b ) &&
                        ( rSc        a == rSc         b ) &&
                        ( rUpVote    a == rUpVote     b ) &&
                        ( rDownVote  a == rDownVote   b ) &&
                        ( rAge       a == rAge        b ) &&
                        ( rCognoTxId a /= rCognoTxId  b ) &&
                        ( rType      a == rType       b )

