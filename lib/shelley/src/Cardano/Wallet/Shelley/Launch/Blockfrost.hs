{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Cardano.Wallet.Shelley.Launch.Blockfrost
    ( TokenFile
    , Token
    , readToken
    , tokenFileOption
    ) where

import Prelude

import Blockfrost.Client.Core
    ( projectFromFile )
import Blockfrost.Client.Types
    ( Project (..) )
import Options.Applicative
    ( Parser, auto, help, long, metavar, option )

newtype TokenFile = TokenFile FilePath
    deriving newtype (Eq, Show, Read)

newtype Token = Token Project

-- | --blockfrost-token-file FILE
tokenFileOption :: Parser TokenFile
tokenFileOption = option auto $ mconcat
    [ long "blockfrost-token-file"
    , metavar "FILE"
    , help $ mconcat
        [ "FILE contains an authentication token token for "
        , "BlockFrost Cardano API (https://blockfrost.io)."
        ]
    ]

readToken :: TokenFile -> IO Token
readToken (TokenFile fp) = Token <$> projectFromFile fp
