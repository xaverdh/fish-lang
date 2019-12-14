module Fish.Lang.Prim.Pretty where

import Fish.Lang.Prim
import Data.NText
import Text.PrettyPrint.GenericPretty
import GHC.Generics

import qualified Data.List.NonEmpty as N
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

instance Out a => Out (N.NonEmpty a)
instance Out i => Out (Indexing i)
instance Out FileMode
instance Out Fd
instance Out Glob

instance Out T.Text where
  doc = doc . T.unpack
  docPrec i = docPrec i . T.unpack

instance Out B.ByteString where
  doc = doc . BC.unpack
  docPrec i = docPrec i . BC.unpack


instance Out NText where
  doc = doc . extractText
  docPrec i = docPrec i . extractText
