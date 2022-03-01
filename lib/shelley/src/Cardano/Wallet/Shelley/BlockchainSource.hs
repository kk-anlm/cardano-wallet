-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- Source of the blockchain data for the wallet
--
module Cardano.Wallet.Shelley.BlockchainSource
    ( BlockchainSource(..)
    ) where

import Cardano.Launcher.Node
    ( CardanoNodeConn )
import Cardano.Wallet.Primitive.Types
    ( NetworkParameters )
import Cardano.Wallet.Shelley.Compatibility
    ( NodeToClientVersionData )

import qualified Blockfrost.Client as Blockfrost

data BlockchainSource
    = NodeSource
        CardanoNodeConn
        -- ^ Socket for communicating with the node
        NetworkParameters
        -- ^ Records the complete set of parameters
        -- currently in use by the network that are relevant to the wallet.
        NodeToClientVersionData
    | BlockfrostSource Blockfrost.Project
    -- ^ Blockfrost token when working in the light mode
