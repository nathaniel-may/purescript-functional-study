module Data.Zipper where

import Control.Pure (Pure)
import Data.ZipperM (ZipperM)

type Zipper a = ZipperM Pure a
