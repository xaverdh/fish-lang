{-# language TemplateHaskell #-}
module Fish.Lang.Version where

import Development.GitRev

version :: String
version = $(gitHash)
