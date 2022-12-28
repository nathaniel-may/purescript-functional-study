module Data.Zipper where

import Data.Identity (Identity)
import Data.ZipperM (ZipperM)


type Zipper a = ZipperM Identity a
