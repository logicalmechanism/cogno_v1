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
module OracleDataType
  ( OracleData
  , oPkh
  , oSc
  , oInPid
  , oInTkn
  , oInAmt
  , oOutPid
  , oOutTkn
  , oOutAmt
  , oAge
  , oCognoTxId
  , oCognoIndex
  , testData
  , getInPriceConversion
  , getOutPriceConversion
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import qualified Plutus.V2.Ledger.Api   as PlutusV2

-------------------------------------------------------------------------------
-- | Create the OracleData object.
-------------------------------------------------------------------------------
data OracleData = OracleData
  { oPkh        :: PlutusV2.PubKeyHash
  -- ^ The public key hash of the oracle.
  , oSc         :: PlutusV2.PubKeyHash
  -- ^ The stake hash of the oracle.
  , oInPid      :: PlutusV2.CurrencySymbol
  -- ^ The incoming pid
  , oInTkn      :: PlutusV2.TokenName
  -- ^ The incoming token name
  , oInAmt      :: Integer
  -- ^ The amount
  , oOutPid     :: PlutusV2.CurrencySymbol
  -- ^ The outgoing pid
  , oOutTkn     :: PlutusV2.TokenName
  -- ^ The outgoing token name
  , oOutAmt     :: Integer
  -- ^ The outgoing amount
  , oAge        :: Integer
  -- ^ The age of the data.
  , oCognoTxId  :: PlutusV2.BuiltinByteString
  -- ^ The TxId of the cogno connected to this oracle.
  , oCognoIndex :: Integer
  -- ^ The Index of the TxId of the congo connected to this oracle.
  }
PlutusTx.unstableMakeIsData ''OracleData


testData :: OracleData
testData = OracleData
  { oPkh        = ""
  , oSc         = ""
  , oInPid      = ""
  , oInTkn      = ""
  , oInAmt      = 741636485
  , oOutPid     = ""
  , oOutTkn     = ""
  , oOutAmt     = 1000000
  , oAge        = 0
  , oCognoTxId  = ""
  , oCognoIndex = 0
  }

getInPriceConversion :: Integer -> OracleData -> Integer
getInPriceConversion amt datum = (divide amt (oInAmt datum)) * (oOutAmt datum)

getOutPriceConversion :: Integer -> OracleData -> Integer
getOutPriceConversion amt datum = (divide amt (oOutAmt datum)) * (oInAmt datum)

