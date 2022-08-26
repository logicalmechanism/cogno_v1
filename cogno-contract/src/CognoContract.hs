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
module CognoContract
  ( cognoContractScript
  , cognoContractScriptShortBs
  ) where
import qualified PlutusTx
import           PlutusTx.Prelude
import           Codec.Serialise
import           Cardano.Api.Shelley            ( PlutusScript (..), PlutusScriptV2 )
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.ByteString.Short          as SBS
import qualified Plutus.V1.Ledger.Scripts       as Scripts
import qualified Plutus.V1.Ledger.Value       as Value
import qualified Plutus.V2.Ledger.Api           as PlutusV2
import qualified Plutus.V2.Ledger.Contexts      as ContextsV2
import           Plutus.Script.Utils.V2.Scripts as Utils
import           CognoDataType
import           TagDataType
import           HelperFunctions
{- |
  Author   : The Ancient Kraken
  Copyright: 2022
  
  cardano-cli 1.35.3 - linux-x86_64 - ghc-8.10
  git rev 950c4e222086fed5ca53564e642434ce9307b0b9

  cabal-install version 3.6.2.0
  compiled using version 3.6.2.0 of the Cabal library

  The Glorious Glasgow Haskell Compilation System, version 8.10.7
-}
thresholdLovelace :: Integer
thresholdLovelace = 10000000
-------------------------------------------------------------------------------
-- | Create the datum type.
-------------------------------------------------------------------------------
data CustomDatumType = Cogno CognoData |
                       Tag TagData
PlutusTx.makeIsDataIndexed ''CustomDatumType [ ( 'Cogno, 0 )
                                             , ( 'Tag,   1 )
                                             ]

-------------------------------------------------------------------------------
-- | Create the redeemer type.
-------------------------------------------------------------------------------
data CustomRedeemerType = Remove | 
                          Update |
                          Kudos
PlutusTx.makeIsDataIndexed ''CustomRedeemerType [ ( 'Remove, 0 )
                                                , ( 'Update, 1 )
                                                , ( 'Kudos,  2 )
                                                ]
-------------------------------------------------------------------------------
-- | mkValidator :: Datum -> Redeemer -> ScriptContext -> Bool
-------------------------------------------------------------------------------
{-# INLINABLE mkValidator #-}
mkValidator :: CustomDatumType -> CustomRedeemerType -> PlutusV2.ScriptContext -> Bool
mkValidator datum redeemer context =
  case datum of
    -- the tag state
    (Tag td) ->
      let userPkh  = tPkh td
          userAddr = createAddress userPkh (tSc td)
      in case redeemer of
        -- remove utxo from the contract
        Remove -> do
          { let a = traceIfFalse "Incorrect In/Out" $ isNInputs txInputs 1 && isNOutputs contTxOutputs 0   -- single input no outputs
          ; let b = traceIfFalse "Wrong Tx Signer"  $ ContextsV2.txSignedBy info userPkh                   -- wallet must sign it
          ; let c = traceIfFalse "Value Not Paid"   $ isAddrGettingPaid txOutputs userAddr validatingValue -- send back the leftover
          ;         traceIfFalse "Tag Remove Error" $ all (==(True :: Bool)) [a,b,c]
          }
        
        -- update the utxo datum
        Update ->
          case getOutboundDatum contTxOutputs validatingValue of
            Nothing            -> False
            Just outboundDatum ->
              case outboundDatum of
                (Tag td') -> do
                  { let a = traceIfFalse "Incorrect In/Out" $ isNInputs txInputs 1 && isNOutputs contTxOutputs 1 -- single input single output
                  ; let b = traceIfFalse "Wrong Tx Signer"  $ ContextsV2.txSignedBy info userPkh                 -- wallet must sign it
                  ; let c = traceIfFalse "Incorrect Datum"  $ updateTagData td td'                               -- the datum changes correctly
                  ;         traceIfFalse "Tag Update Error" $ all (==(True :: Bool)) [a,b,c]
                  }

                -- only tag
                _ -> False
        -- only remove or update
        _ -> False

    -- the cogno state
    (Cogno cd) ->
      let userPkh  = cdPkh cd
          userAddr = createAddress userPkh (cdSc cd)
      in case redeemer of
        -- remove utxo from the contract
        Remove -> do
          { let a = traceIfFalse "Incorrect In/Out"   $ isNInputs txInputs 1 && isNOutputs contTxOutputs 0   -- single input no outputs
          ; let b = traceIfFalse "Wrong Tx Signer"    $ ContextsV2.txSignedBy info userPkh                   -- wallet must sign it
          ; let c = traceIfFalse "Value Not Paid"     $ isAddrGettingPaid txOutputs userAddr validatingValue -- send back the leftover
          ;         traceIfFalse "Cogno Remove Error" $ all (==(True :: Bool)) [a,b,c]
          }

        -- update the utxo datum
        Update ->
          case getOutboundDatum contTxOutputs validatingValue of
            Nothing            -> False
            Just outboundDatum ->
              case outboundDatum of
                (Cogno cd') -> do
                  { let a = traceIfFalse "Incorrect In/Out"   $ isNInputs txInputs 1 && isNOutputs contTxOutputs 1 -- single input single output
                  ; let b = traceIfFalse "Wrong Tx Signer"    $ ContextsV2.txSignedBy info userPkh                 -- wallet must sign it
                  ; let c = traceIfFalse "Incorrect Datum"    $ updateCognoData cd cd'                             -- the datum changes correctly
                  ; let d = traceIfFalse "Minimum Value"      $ Value.geq validatingValue minimumValue             -- Must have minimum value
                  ;         traceIfFalse "Cogno Update Error" $ all (==(True :: Bool)) [a,b,c,d]
                  }
                
                -- only cogno
                _ -> False

        -- anyone can give a kudos
        Kudos ->
          case getOutboundDatum contTxOutputs validatingValue of
            Nothing            -> False
            Just outboundDatum ->
              case outboundDatum of
                (Cogno cd') -> do
                  { let a = traceIfFalse "Incorrect In/Out" $ isNInputs txInputs 1 && isNOutputs contTxOutputs 1 -- single input single output
                  ; let b = traceIfFalse "Incorrect Datum"  $ giveAKudo cd cd'                                   -- the datum changes correctly
                  ; let c = traceIfFalse "Minimum Value"    $ Value.geq validatingValue minimumValue             -- Must have minimum value
                  ;         traceIfFalse "Cog Update Error" $ all (==(True :: Bool)) [a,b,c]
                  }
                
                -- only cogno
                _ -> False
  where
    info :: PlutusV2.TxInfo
    info = ContextsV2.scriptContextTxInfo  context

    -- | in / outs
    txInputs :: [PlutusV2.TxInInfo]
    txInputs = PlutusV2.txInfoInputs info

    contTxOutputs :: [PlutusV2.TxOut]
    contTxOutputs = ContextsV2.getContinuingOutputs context

    txOutputs :: [PlutusV2.TxOut]
    txOutputs = ContextsV2.txInfoOutputs info

    -- | the value being spent by this tx
    validatingValue :: PlutusV2.Value
    validatingValue =
      case ContextsV2.findOwnInput context of
        Nothing    -> traceError "" -- This error should never be hit.
        Just input -> PlutusV2.txOutValue $ PlutusV2.txInInfoResolved input
    
    -- threshold ada amount to do things, 10 ada
    minimumValue :: PlutusV2.Value
    minimumValue = Value.singleton Value.adaSymbol Value.adaToken thresholdLovelace
    
    -- | Get the inline datum that holds a value from a list of tx outs.
    getOutboundDatum :: [PlutusV2.TxOut] -> PlutusV2.Value -> Maybe CustomDatumType
    getOutboundDatum []     _ = Nothing
    getOutboundDatum (x:xs) val =
      if PlutusV2.txOutValue x == val -- strict value continue
        then
          case PlutusV2.txOutDatum x of
            PlutusV2.NoOutputDatum       -> Nothing -- forbid null datums
            (PlutusV2.OutputDatumHash _) -> Nothing -- forbid embedded datums
            
            -- inline datum only
            (PlutusV2.OutputDatum (PlutusV2.Datum d)) -> 
              case PlutusTx.fromBuiltinData d of
                Nothing     -> getOutboundDatum xs val
                Just inline -> Just $ PlutusTx.unsafeFromBuiltinData @CustomDatumType inline
        
        -- just loop if bad value
        else getOutboundDatum xs val
-- end of mkValidator
-------------------------------------------------------------------------------
-- | Now we need to compile the Validator.
-------------------------------------------------------------------------------
validator' :: PlutusV2.Validator
validator' = PlutusV2.mkValidatorScript
    $$(PlutusTx.compile [|| wrap ||])
 where
    wrap = Utils.mkUntypedValidator mkValidator
-------------------------------------------------------------------------------
-- | The code below is required for the plutus script compile.
-------------------------------------------------------------------------------
script :: Scripts.Script
script = Scripts.unValidatorScript validator'

cognoContractScriptShortBs :: SBS.ShortByteString
cognoContractScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

cognoContractScript :: PlutusScript PlutusScriptV2
cognoContractScript = PlutusScriptSerialised cognoContractScriptShortBs
