{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Wallet.Primitive.PassphraseSpec
    ( spec
    ) where

import Prelude

import Cardano.Wallet.Primitive.Passphrase
    ( ErrWrongPassphrase (..)
    , Passphrase (..)
    , PassphraseHash (..)
    , PassphraseMaxLength (..)
    , PassphraseMinLength (..)
    , PassphraseScheme (..)
    , checkPassphrase
    , encryptPassphrase
    , preparePassphrase
    )
import Control.Monad
    ( replicateM )
import Control.Monad.IO.Class
    ( liftIO )
import Data.Proxy
    ( Proxy (..) )
import Test.Hspec
    ( Spec, describe, it, shouldBe )
import Test.Hspec.Extra
    ( parallel )
import Test.QuickCheck
    ( Arbitrary (..)
    , Property
    , arbitraryPrintableChar
    , choose
    , property
    , (===)
    , (==>)
    )
import Test.QuickCheck.Arbitrary.Generic
    ( genericArbitrary )
import Test.QuickCheck.Monadic
    ( assert, monadicIO, run )
import Test.Text.Roundtrip
    ( textRoundtrip )

import qualified Data.ByteArray as BA
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

spec :: Spec
spec = do
    parallel $ describe "Text Roundtrip" $ do
        textRoundtrip $ Proxy @(Passphrase "user")

    parallel $ describe "Passphrases" $ do
        it "checkPassphrase p h(p) == Right ()" $
            property prop_passphraseRoundtrip
        it "p /= p' => checkPassphrase p' h(p) == Left ErrWrongPassphrase" $
            property prop_passphraseRoundtripFail
        it "checkPassphrase fails when hash is malformed" $
            property prop_passphraseHashMalformed

{-------------------------------------------------------------------------------
                               Properties
-------------------------------------------------------------------------------}

prop_passphraseRoundtrip
    :: Passphrase "user"
    -> Property
prop_passphraseRoundtrip pwd = monadicIO $ liftIO $ do
    (scheme, hpwd) <- encryptPassphrase pwd
    scheme `shouldBe` EncryptWithPBKDF2
    checkPassphrase EncryptWithPBKDF2 pwd hpwd `shouldBe` Right ()

prop_passphraseRoundtripFail
    :: Passphrase "user"
    -> Passphrase "user"
    -> Property
prop_passphraseRoundtripFail p p' =
    p /= p' ==> monadicIO $ do
        (_scheme, hp) <- run $ encryptPassphrase p
        assert $ checkPassphrase EncryptWithPBKDF2 p' hp
            == Left ErrWrongPassphrase

prop_passphraseHashMalformed
    :: PassphraseScheme
    -> Passphrase "user"
    -> Property
prop_passphraseHashMalformed scheme pwd =
    checkPassphrase scheme pwd (PassphraseHash mempty)
        === if not haveScrypt && scheme == EncryptWithScrypt
            then Left $ ErrPassphraseSchemeUnsupported EncryptWithScrypt
            else Left ErrWrongPassphrase

instance Arbitrary (Passphrase "user") where
    arbitrary = do
        n <- choose (passphraseMinLength p, passphraseMaxLength p)
        bytes <- T.encodeUtf8 . T.pack <$> replicateM n arbitraryPrintableChar
        return $ Passphrase $ BA.convert bytes
      where p = Proxy :: Proxy "user"

    shrink (Passphrase bytes)
        | BA.length bytes <= passphraseMinLength p = []
        | otherwise =
            [ Passphrase
            $ BA.convert
            $ B8.take (passphraseMinLength p)
            $ BA.convert bytes
            ]
      where p = Proxy :: Proxy "user"

instance Arbitrary PassphraseScheme where
    arbitrary = genericArbitrary

instance Arbitrary (Passphrase "encryption") where
    arbitrary = preparePassphrase EncryptWithPBKDF2
        <$> arbitrary @(Passphrase "user")

#if HAVE_SCRYPT
haveScrypt :: Bool
haveScrypt = True
#else
haveScrypt :: Bool
haveScrypt = False
#endif
