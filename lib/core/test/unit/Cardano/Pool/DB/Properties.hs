{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Cardano.Pool.DB.Properties
    ( properties
    , withDB
    , newMemoryDBLayer
    ) where

import Prelude

import Cardano.BM.Configuration.Model
    ( Configuration )
import Cardano.BM.Data.LogItem
    ( LogObject (..) )
import Cardano.BM.Trace
    ( traceInTVarIO )
import Cardano.DB.Sqlite
    ( SqliteContext )
import Cardano.Pool.DB
    ( DBLayer (..), ErrSlotAlreadyExists (..) )
import Cardano.Pool.DB.Arbitrary
    ( StakePoolsFixture (..) )
import Cardano.Pool.DB.Sqlite
    ( newDBLayer )
import Cardano.Wallet.Primitive.Types
    ( PoolId, SlotId (..) )
import Cardano.Wallet.Unsafe
    ( unsafeRunExceptT )
import Control.Monad
    ( forM_ )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Except
    ( runExceptT )
import Data.Functor
    ( ($>) )
import Data.List.Extra
    ( nubOrd )
import Data.Text
    ( Text )
import GHC.Conc
    ( TVar, newTVarIO )
import Test.Hspec
    ( Spec
    , SpecWith
    , beforeAll
    , beforeWith
    , describe
    , it
    , shouldBe
    , shouldReturn
    )
import Test.QuickCheck
    ( Gen, Property, classify, conjoin, counterexample, elements, property )
import Test.QuickCheck.Monadic
    ( assert, monadicIO, monitor )

import qualified Cardano.Pool.DB.MVar as MVar
import qualified Data.Map.Strict as Map

-- | Provide a DBLayer to a Spec that requires it. The database is initialised
-- once, and cleared with 'cleanDB' before each test.
withDB :: IO (DBLayer IO) -> SpecWith (DBLayer IO) -> Spec
withDB create = beforeAll create . beforeWith (\db -> cleanDB db $> db)

-- | Set up a DBLayer for testing, with the command context, and the logging
-- variable.
newMemoryDBLayer :: IO Configuration -> IO (DBLayer IO)
newMemoryDBLayer conf = snd . snd <$> (newMemoryDBLayer' conf)

newMemoryDBLayer'
    :: IO Configuration -> IO (TVar [LogObject Text], (SqliteContext, DBLayer IO))
newMemoryDBLayer' testingLogConfig = do
    logConfig <- testingLogConfig
    logVar <- newTVarIO []
    (logVar, ) <$> newDBLayer logConfig (traceInTVarIO logVar) Nothing

properties :: SpecWith (DBLayer IO)
properties = do
    describe "Stake Pool properties" $ do
        it "putPoolProduction . readPoolProduction yields expected results"
            (property . prop_putReadPoolProduction)
        it "putPoolProduction with already put slot yields error"
            (property . prop_putSlotTwicePoolProduction)
        it "Rollback of stake pool production"
            (property . prop_rollbackPools)

{-------------------------------------------------------------------------------
                                    Properties
-------------------------------------------------------------------------------}

-- | Can read put pool production
prop_putReadPoolProduction
    :: DBLayer IO
    -> StakePoolsFixture
    -> Property
prop_putReadPoolProduction db (StakePoolsFixture pairs) =
    monadicIO (setup >>= prop)
  where
    epochs = nubOrd $ map (epochNumber . snd) pairs
    setup = liftIO $ do
        cleanDB db
        db' <- MVar.newDBLayer
        cleanDB db'
        pure db'
    prop db' = liftIO $ do
        forM_ pairs $ \(pool, slot) ->
            unsafeRunExceptT $ putPoolProduction db slot pool
        forM_ pairs $ \(pool, slot) ->
            unsafeRunExceptT $ putPoolProduction db' slot pool
        forM_ epochs $ \(epoch) -> do
            res' <- readPoolProduction db' epoch
            readPoolProduction db epoch `shouldReturn` res'

-- | Cannot put pool production with already put slot
prop_putSlotTwicePoolProduction
    :: DBLayer IO
    -> StakePoolsFixture
    -> Property
prop_putSlotTwicePoolProduction db (StakePoolsFixture pairs) =
    monadicIO (setup >>= prop)
  where
    setup = liftIO $ do
        cleanDB db
        db' <- MVar.newDBLayer
        cleanDB db'
        pure db'
    prop db' = liftIO $ do
        forM_ pairs $ \(pool, slot) -> do
            let err = ErrSlotAlreadyExists slot
            runExceptT (putPoolProduction db' slot pool) `shouldReturn` Right ()
            runExceptT (putPoolProduction db' slot pool) `shouldReturn` Left err
            runExceptT (putPoolProduction db slot pool) `shouldReturn` Right ()
            runExceptT (putPoolProduction db slot pool) `shouldReturn` Left err

-- | Rolling back wipes out pool production statistics after the rollback point.
prop_rollbackPools
    :: DBLayer IO
    -> StakePoolsFixture
    -> SlotId
    -> Property
prop_rollbackPools db f@(StakePoolsFixture pairs) sl =
    monadicIO prop
  where
    prop = do
        (beforeRollback, afterRollback) <- liftIO $ do
            forM_ pairs $ \(pool, slot) ->
                runExceptT $ putPoolProduction db slot pool
            before <- map fst <$> allPoolProduction db f
            rollbackTo db sl
            after <- map fst <$> allPoolProduction db f
            pure (before, after)

        monitor $ counterexample $ unlines
            [ "Rollback point:    " <> showSlot sl
            , "Production before: " <> unwords (map showSlot beforeRollback)
            , "Production after:  " <> unwords (map showSlot afterRollback)
            ]
        monitor $ classify (any (> sl) beforeRollback) "something to roll back"
        monitor $ classify (all (<= sl) beforeRollback) "nothing to roll back"

        assert $ all (<= sl) afterRollback

    showSlot (SlotId epoch slot) = show epoch ++ "." ++ show slot

-- | Concatenate stake pool production for all epochs in the test fixture.
allPoolProduction :: DBLayer IO -> StakePoolsFixture -> IO [(SlotId, PoolId)]
allPoolProduction db (StakePoolsFixture pairs) =
    rearrange <$> mapM (readPoolProduction db) epochs
  where
    epochs = nubOrd $ map (epochNumber . snd) pairs
    rearrange ms = concat
        [ [ (sl, p) | sl <- sls ] | (p, sls) <- concatMap Map.assocs ms ]
