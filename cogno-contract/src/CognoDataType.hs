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
module CognoDataType
  ( CognoData
  , cdPkh
  , cdSc
  , cdKudos
  , cdCogno
  , cdImage
  , cdDetail
  , cdLocale
  , updateCognoData
  , giveAKudo
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Plutus.V2.Ledger.Api as PlutusV2
{- |
  Author   : The Ancient Kraken
  Copyright: 2022
-}
-------------------------------------------------------------------------------
-- | Create the cogno data object.
-------------------------------------------------------------------------------
data CognoData = CognoData
  { cdPkh    :: PlutusV2.PubKeyHash
  -- ^ The public key hash of the wallet.
  , cdSc     :: PlutusV2.PubKeyHash
  -- ^ The stake hash of the wallet.
  , cdKudos  :: Integer
  -- ^ The wallet's global kudos points.
  , cdCogno  :: PlutusV2.BuiltinByteString
  -- ^ The cognomen of the wallet.
  , cdImage  :: [PlutusV2.BuiltinByteString]
  -- ^ The image of the wallet.
  , cdDetail :: [PlutusV2.BuiltinByteString]
  -- ^ The details of the wallet.
  , cdLocale  :: PlutusV2.BuiltinByteString
  -- ^ The wallets locale.
  }
PlutusTx.unstableMakeIsData ''CognoData

-- Owner must not change and the kudos is constant
updateCognoData :: CognoData -> CognoData -> Bool
updateCognoData a b = ( cdPkh   a == cdPkh   b ) &&
                      ( cdSc    a == cdSc    b ) &&
                      ( cdKudos a == cdKudos b )

-- nothing can change but the kudos by one
giveAKudo :: CognoData -> CognoData -> Bool
giveAKudo a b = ( cdPkh       a == cdPkh    b ) &&
                ( cdSc        a == cdSc     b ) &&
                ( cdKudos a + 1 == cdKudos  b ) &&
                ( cdCogno     a == cdCogno  b ) &&
                ( cdImage     a == cdImage  b ) &&
                ( cdDetail    a == cdDetail b ) &&
                ( cdLocale    a == cdLocale b )