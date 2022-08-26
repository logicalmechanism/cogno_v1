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
module TagDataType
  ( TagData
  , tPkh
  , tSc
  , tTag
  , tDetail
  , updateTagData
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Plutus.V2.Ledger.Api as PlutusV2
{- |
  Author   : The Ancient Kraken
  Copyright: 2022
-}
-------------------------------------------------------------------------------
-- | Create the vesting data object.
-------------------------------------------------------------------------------
data TagData = TagData
  { tPkh    :: PlutusV2.PubKeyHash
  -- ^ The public key hash of the wallet.
  , tSc     :: PlutusV2.PubKeyHash
  -- ^ The stake hash of the wallet.
  , tTag    :: PlutusV2.BuiltinByteString
  -- ^ The tag of the message.
  , tDetail :: [PlutusV2.BuiltinByteString]
  -- ^ The details of the message.
  }
PlutusTx.unstableMakeIsData ''TagData

-- Owner must not change
updateTagData :: TagData -> TagData -> Bool
updateTagData a b = ( tPkh   a == tPkh   b ) &&
                    ( tSc    a == tSc    b )