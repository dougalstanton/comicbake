module Pix where

import Script
import qualified Layout as L (Bubble(..))
import Layout (Panel(..))

data Pix = Pix

panel2pix :: Panel -> Pix
panel2pix panel = Pix
