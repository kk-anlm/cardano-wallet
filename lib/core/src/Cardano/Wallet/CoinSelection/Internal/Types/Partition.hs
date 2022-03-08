{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{- HLINT ignore "Use camelCase" -}

module Cardano.Wallet.CoinSelection.Internal.Types.Partition
    where

import Prelude

import Cardano.Numeric.Util
    ( partitionNatural )
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Proxy
    ( Proxy )
import Numeric.Natural
    ( Natural )
import Test.QuickCheck
    ( Arbitrary, Property, checkCoverage, cover, property )
import Test.QuickCheck.Classes
    ( Laws (..) )

import qualified Data.Foldable as F

--------------------------------------------------------------------------------
-- Class
--------------------------------------------------------------------------------

class Partition a where
    partition :: a -> NonEmpty a -> (a, NonEmpty a)

--------------------------------------------------------------------------------
-- Laws
--------------------------------------------------------------------------------

-- think about how to separate coverage from statements of laws.
-- we want to have clear statements of laws.
-- we want to avoid cluttering them up with coverage checks.
--
partitionLaws
    :: forall a.
        ( Arbitrary a
        , Arbitrary (NonEmpty a)
        , Eq a
        , Monoid a
        , Partition a
        , Show a
        )
    => Proxy a
    -> Laws
partitionLaws _ = Laws "Partition"
    [ ( "Length"
      , property (partitionLaw_length @a))
    , ( "Sum"
      , property (partitionLaw_sum @a))
    ]

partitionLaw_length :: Partition a => a -> NonEmpty a -> Bool
partitionLaw_length a as =
    ((== length as) . length . snd)
    (partition a as)

partitionLaw_length' :: a -> NonEmpty a -> (a, NonEmpty a) -> Bool
partitionLaw_length' a as (r, rs) =
    (length as == length rs)

partitionLaw_sum :: (Eq a, Monoid a, Partition a) => a -> NonEmpty a -> Property
partitionLaw_sum a as = property $
    (\(r, rs) ->
        checkCoverage $
        cover 1
            (r == mempty && F.fold rs == mempty)
            "    empty remainder and     empty partition" $
        cover 1
            (r == mempty && F.fold rs /= mempty)
            "    empty remainder and non-empty partition" $
        cover 1
            (r /= mempty && F.fold rs == mempty)
            "non-empty remainder and     empty partition" $
        cover 0
            (r /= mempty && F.fold rs /= mempty)
            "non-empty remainder and non-empty partition" $
        r <> F.fold rs == a)
    (partition a as)

makeProperty f p = (\(r, rs) -> checkCoverage $ f (r, rs)) p

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Partition Natural where
    partition n as = maybe (n, 0 <$ as) (0, ) (partitionNatural n as)
