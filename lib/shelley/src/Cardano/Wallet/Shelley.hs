{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- This module provides the main wallet server function for Shelley.
--
-- The "Cardano.Wallet.Shelley.Network" uses the mini-protocols (ChainSync and
-- TxSubmission) to talk with a core node and synchronize with the network.
--
-- Functionality specific to this backend for creating transactions is in
-- "Cardano.Wallet.Shelley.Transaction"

module Cardano.Wallet.Shelley
    ( SomeNetworkDiscriminant (..)
    , serveWallet
    , module Logging
    , module Tracers
    ) where

import Prelude

import Cardano.Pool.DB
    ( DBLayer (..) )
import Cardano.Wallet.Api
    ( ApiLayer, ApiV2 )
import Cardano.Wallet.Api.Server
    ( HostPreference, Listen (..), ListenError (..), TlsConfiguration )
import Cardano.Wallet.Api.Types
    ( ApiStakePool
    , DecodeAddress
    , DecodeStakeAddress
    , EncodeAddress
    , EncodeStakeAddress
    )
import Cardano.Wallet.DB.Sqlite.CheckpointsOld
    ( PersistAddressBook )
import Cardano.Wallet.DB.Sqlite.Migration
    ( DefaultFieldValues (..) )
import Cardano.Wallet.Network
    ( NetworkLayer (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( DelegationAddress (..)
    , Depth (..)
    , NetworkDiscriminant (..)
    , NetworkDiscriminantVal
    , PaymentAddress
    , PersistPrivateKey
    , WalletKey
    , networkDiscriminantVal
    )
import Cardano.Wallet.Primitive.AddressDerivation.Byron
    ( ByronKey )
import Cardano.Wallet.Primitive.AddressDerivation.Icarus
    ( IcarusKey )
import Cardano.Wallet.Primitive.AddressDerivation.SharedKey
    ( SharedKey )
import Cardano.Wallet.Primitive.AddressDerivation.Shelley
    ( ShelleyKey )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( IsOurs )
import Cardano.Wallet.Primitive.AddressDiscovery.Random
    ( RndState )
import Cardano.Wallet.Primitive.AddressDiscovery.Sequential
    ( SeqState )
import Cardano.Wallet.Primitive.AddressDiscovery.Shared
    ( SharedState )
import Cardano.Wallet.Primitive.Slotting
    ( neverFails )
import Cardano.Wallet.Primitive.SyncProgress
    ( SyncTolerance )
import Cardano.Wallet.Primitive.Types
    ( Block
    , NetworkParameters (..)
    , NetworkParameters
    , PoolMetadataGCStatus (..)
    , ProtocolParameters (..)
    , Settings (..)
    , SlottingParameters (..)
    , TokenMetadataServer (..)
    , WalletId
    )
import Cardano.Wallet.Primitive.Types.Address
    ( Address )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..) )
import Cardano.Wallet.Primitive.Types.RewardAccount
    ( RewardAccount )
import Cardano.Wallet.Primitive.Types.Tx
    ( SealedTx )
import Cardano.Wallet.Registry
    ( HasWorkerCtx (..), traceAfterThread )
import Cardano.Wallet.Shelley.Api.Server
    ( server )
import Cardano.Wallet.Shelley.BlockchainSource
    ( BlockchainSource (..) )
import Cardano.Wallet.Shelley.Compatibility
    ( CardanoBlock, HasNetworkId (..), StandardCrypto, fromCardanoBlock )
import Cardano.Wallet.Shelley.Logging as Logging
    ( ApplicationLog (..) )
import Cardano.Wallet.Shelley.Network
    ( NetworkLayerLog, withNetworkLayer )
import Cardano.Wallet.Shelley.Pools
    ( StakePoolLayer (..)
    , StakePoolLog (..)
    , monitorMetadata
    , monitorStakePools
    , newStakePoolLayer
    )
import Cardano.Wallet.Shelley.Tracers as Tracers
    ( TracerSeverities
    , Tracers
    , Tracers' (..)
    , nullTracers
    , setupTracers
    , tracerDescriptions
    , tracerLabels
    , tracerSeverities
    )
import Cardano.Wallet.Shelley.Transaction
    ( newTransactionLayer )
import Cardano.Wallet.TokenMetadata
    ( newMetadataClient )
import Cardano.Wallet.Transaction
    ( TransactionLayer )
import Control.Monad
    ( forM_, void, (>=>) )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Cont
    ( ContT (ContT), evalContT )
import Control.Tracer
    ( Tracer, contramap, traceWith )
import Data.Function
    ( (&) )
import Data.Maybe
    ( fromJust, fromMaybe )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import GHC.Stack
    ( HasCallStack )
import Network.Ntp
    ( NtpClient (..), withWalletNtpClient )
import Network.Socket
    ( Socket, getSocketName )
import Network.URI
    ( URI (..), parseURI )
import Network.Wai.Handler.Warp
    ( setBeforeMainLoop )
import System.Exit
    ( ExitCode (..) )
import System.IOManager
    ( withIOManager )
import Type.Reflection
    ( Typeable )
import UnliftIO.Concurrent
    ( forkFinally, forkIOWithUnmask, killThread )
import UnliftIO.MVar
    ( modifyMVar_, newMVar )
import UnliftIO.STM
    ( newTVarIO )

import qualified Cardano.Api.Shelley as Cardano
import qualified Cardano.Pool.DB.Sqlite as Pool
import qualified Cardano.Wallet.Api.Server as Server
import qualified Cardano.Wallet.DB.Sqlite as Sqlite
import qualified Network.Wai.Handler.Warp as Warp

-- | Encapsulate a network discriminant and the necessary constraints it should
-- satisfy.
data SomeNetworkDiscriminant where
    SomeNetworkDiscriminant
        :: forall (n :: NetworkDiscriminant).
            ( NetworkDiscriminantVal n
            , PaymentAddress n IcarusKey
            , PaymentAddress n ByronKey
            , PaymentAddress n ShelleyKey
            , DelegationAddress n ShelleyKey
            , HasNetworkId n
            , DecodeAddress n
            , EncodeAddress n
            , DecodeStakeAddress n
            , EncodeStakeAddress n
            , Typeable n
            )
        => Proxy n
        -> SomeNetworkDiscriminant

deriving instance Show SomeNetworkDiscriminant

-- | The @cardano-wallet@ main function. It takes the configuration
-- which was passed from the CLI and environment and starts all components of
-- the wallet.
serveWallet
    :: BlockchainSource
    -- ^ Source of the blockchain data
    -> NetworkParameters
    -- ^ Records the complete set of parameters
    -- currently in use by the network that are relevant to the wallet.
    -> SomeNetworkDiscriminant
    -- ^ Proxy for the network discriminant
    -> Tracers IO
    -- ^ Logging config.
    -> SyncTolerance
    -- ^ A time tolerance within we consider being synced
    -> Maybe FilePath
    -- ^ Database folder filepath
    -> Maybe (Pool.DBDecorator IO)
    -- ^ An optional decorator that can be used to monitor pool DB operations.
    -> HostPreference
    -- ^ Which host to bind.
    -> Listen
    -- ^ HTTP API Server port.
    -> Maybe TlsConfiguration
    -- ^ An optional TLS configuration
    -> Maybe Settings
    -- ^ Settings to be set at application start, will be written into DB.
    -> Maybe TokenMetadataServer
    -> Block
    -- ^ The genesis block, or some starting point.
    -- See also: 'Cardano.Wallet.Shelley.Compatibility#KnownNetwork'.
    -> (URI -> IO ())
    -- ^ Callback to run before the main loop
    -> IO ExitCode
serveWallet
  blockchainSrc
  netParams@NetworkParameters
    { protocolParameters
    , genesisParameters
    , slottingParameters
    }
  (SomeNetworkDiscriminant proxyNetwork)
  Tracers{..}
  sTolerance
  databaseDir
  mPoolDatabaseDecorator
  hostPref
  listen
  tlsConfig
  settings
  tokenMetaUri
  block0
  beforeMainLoop = evalContT $ do
    lift $ case blockchainSrc of
        NodeSource nodeConn _ -> trace $ MsgStartingNode nodeConn
        BlockfrostSource project -> trace $ MsgStartingLite project
    lift . trace $ MsgNetworkName $ networkName proxyNetwork
    netLayer <- withNetLayer blockchainSrc networkTracer net sTolerance
    dbLayer <- withDbLayer netLayer
    stakePoolLayer <- withStakePoolLayer dbLayer netLayer
    randomApi <- withRandomApi netLayer
    icarusApi  <- withIcarusApi netLayer
    shelleyApi <- withShelleyApi netLayer
    multisigApi <- withMultisigApi netLayer
    ntpClient <- withNtpClient
    bindSocket >>= lift . \case
        Left err -> do
            trace $ MsgServerStartupError err
            pure $ ExitFailure $ exitCodeApiServer err
        Right (_port, socket) -> do
            startServer
                proxyNetwork
                socket
                randomApi
                icarusApi
                shelleyApi
                multisigApi
                stakePoolLayer
                ntpClient
            pure ExitSuccess

  where
    trace :: ApplicationLog -> IO ()
    trace = traceWith applicationTracer

    net :: Cardano.NetworkId
    net = networkIdVal proxyNetwork

    withNetLayer
        :: HasCallStack
        => BlockchainSource
        -> Tracer IO NetworkLayerLog
        -> Cardano.NetworkId
        -> SyncTolerance
        -> ContT r IO (NetworkLayer IO (CardanoBlock StandardCrypto) )
    withNetLayer blockchainSrc tr net tol =
        ContT $ case blockchainSrc of
            NodeSource nodeConn ver ->
                withNetworkLayer tr net netParams nodeConn ver tol
            BlockfrostSource pr ->
                withBlockfrostNetworkLayer

    bindSocket :: ContT r IO (Either ListenError (Warp.Port, Socket))
    bindSocket = ContT $ Server.withListeningSocket hostPref listen

    withRandomApi netLayer =
        lift $ apiLayer (newTransactionLayer net) netLayer Server.idleWorker

    withIcarusApi netLayer =
        lift $ apiLayer (newTransactionLayer net) netLayer Server.idleWorker

    withShelleyApi netLayer =
        lift $ apiLayer (newTransactionLayer net) netLayer
            (Server.manageRewardBalance proxyNetwork)

    withMultisigApi netLayer =
        let txLayerUdefined = error "TO-DO in ADP-686"
        in lift $ apiLayer txLayerUdefined netLayer Server.idleWorker

    withBlockfrostNetworkLayer ::
        (NetworkLayer IO (CardanoBlock StandardCrypto) -> IO a) -> IO a
    withBlockfrostNetworkLayer = error "not implemented"

    withNtpClient :: ContT r IO NtpClient
    withNtpClient = do
        iom <- ContT withIOManager
        ContT $ withWalletNtpClient iom ntpClientTracer

    startServer
        :: forall n.
            ( PaymentAddress n IcarusKey
            , PaymentAddress n ByronKey
            , DelegationAddress n ShelleyKey
            , DecodeAddress n
            , EncodeAddress n
            , EncodeStakeAddress n
            , DecodeStakeAddress n
            , Typeable n
            , HasNetworkId n
            )
        => Proxy n
        -> Socket
        -> ApiLayer (RndState n) ByronKey
        -> ApiLayer (SeqState n IcarusKey) IcarusKey
        -> ApiLayer (SeqState n ShelleyKey) ShelleyKey
        -> ApiLayer (SharedState n SharedKey) SharedKey
        -> StakePoolLayer
        -> NtpClient
        -> IO ()
    startServer _proxy socket byron icarus shelley multisig spl ntp = do
        serverUrl <- getServerUrl tlsConfig socket
        let settings = Warp.defaultSettings
                & setBeforeMainLoop (beforeMainLoop serverUrl)
        let application = Server.serve (Proxy @(ApiV2 n ApiStakePool)) $
                server byron icarus shelley multisig spl ntp
        Server.start settings apiServerTracer tlsConfig socket application
    withDbLayer netLayer =
        ContT $ Pool.withDecoratedDBLayer
            (fromMaybe Pool.undecoratedDB mPoolDatabaseDecorator)
            poolsDbTracer
            (Pool.defaultFilePath <$> databaseDir)
            (neverFails "withPoolsMonitoring never forecasts into the future" $
                timeInterpreter netLayer)

    withStakePoolLayer
        :: DBLayer IO
        -> NetworkLayer IO (CardanoBlock StandardCrypto)
        -> ContT ExitCode IO StakePoolLayer
    withStakePoolLayer dbLayer@DBLayer{..} netLayer = lift $ do
        gcStatus <- newTVarIO NotStarted
        forM_ settings $ atomically . putSettings
        let tr = poolsEngineTracer
        void $ forkFinally
            (monitorStakePools tr netParams netLayer dbLayer)
            (traceAfterThread (contramap MsgExitMonitoring tr))

        -- fixme: needs to be simplified as part of ADP-634
        let NetworkParameters{slottingParameters} = netParams
            startMetadataThread = forkIOWithUnmask
                ($ monitorMetadata gcStatus tr slottingParameters dbLayer)
        metadataThread <- newMVar =<< startMetadataThread
        let restartMetadataThread = modifyMVar_ metadataThread $
                killThread >=> const startMetadataThread
        newStakePoolLayer gcStatus netLayer dbLayer restartMetadataThread

    apiLayer
        :: forall s k.
            ( IsOurs s Address
            , IsOurs s RewardAccount
            , PersistAddressBook s
            , PersistPrivateKey (k 'RootK)
            , WalletKey k
            )
        => TransactionLayer k SealedTx
        -> NetworkLayer IO (CardanoBlock StandardCrypto)
        -> (WorkerCtx (ApiLayer s k) -> WalletId -> IO ())
        -> IO (ApiLayer s k)
    apiLayer txLayer netLayer coworker = do
        tokenMetaClient <- newMetadataClient tokenMetadataTracer tokenMetaUri
        dbFactory <- Sqlite.newDBFactory
            walletDbTracer
            (DefaultFieldValues
                { defaultActiveSlotCoefficient =
                    getActiveSlotCoefficient slottingParameters
                , defaultDesiredNumberOfPool =
                    desiredNumberOfStakePools protocolParameters
                , defaultMinimumUTxOValue = Coin 0
                    -- Unused; value does not matter anymore.
                , defaultHardforkEpoch = Nothing
                -- NOTE: see ADP-643
                --
                -- In ADP-470, we've made it possible to distinguish fees from
                -- deposits in the API. This however required a database
                -- migration for which the stake key deposit in vigor is needed.
                -- This value normally comes from the Shelley genesis file, but
                -- we have no direct access to it, nor can we reliably query the
                -- network layer to get the current parameters. Indeed, the
                -- `currentProtocolParameters` and `currentSlottingParameters`
                -- functions both rely on the LSQ protocol, which would:
                --
                --  a) Fail if the wallet and the node are drifting too much
                --  b) Return potentially outdated information if the node is
                --     not synced.
                --
                -- Since the migration is only strictly needed for pre-existing
                -- mainnet and testnet wallet, we currently hard-code the stake
                -- key deposit value that _should_ be used for the migration
                -- (which fortunately happens to be the same on both networks).
                --
                -- It'll do, but it ain't pretty. Without requiring the Shelley
                -- genesis to be provided as argument I currently have no better
                -- and safer idea than hard-coding it. And also have very little
                -- time to do anything fancier.
                , defaultKeyDeposit =
                    Coin 2_000_000
                }
            )
            (neverFails "db layer should never forecast into the future"
                $ timeInterpreter netLayer)
            databaseDir
        Server.newApiLayer
            walletEngineTracer
            (block0, netParams, sTolerance)
            (fromCardanoBlock genesisParameters <$> netLayer)
            txLayer
            dbFactory
            tokenMetaClient
            coworker

networkName :: forall n. NetworkDiscriminantVal n => Proxy n -> Text
networkName _ = networkDiscriminantVal @n

-- | Failure status codes for HTTP API server errors.
exitCodeApiServer :: ListenError -> Int
exitCodeApiServer = \case
    ListenErrorHostDoesNotExist _ -> 10
    ListenErrorInvalidAddress _ -> 11
    ListenErrorAddressAlreadyInUse _ -> 12
    ListenErrorOperationNotPermitted -> 13

getServerUrl :: Maybe TlsConfiguration -> Socket -> IO URI
getServerUrl tlsConfig = (fromJust . parseURI . uri <$>) . getSocketName
  where
    uri addr = scheme <> "://" <> show addr <> "/"
    scheme = maybe "http" (const "https") tlsConfig
