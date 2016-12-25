module Fish.Pretty where

import Fish.Lang
import Data.NText
import Text.PrettyPrint.GenericPretty
import GHC.Generics

import qualified Data.List.NonEmpty as N
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

instance Out a => Out (N.NonEmpty a)
instance (Out s,Out t) => Out (Prog s t)
instance (Out s,Out t) => Out (Args s t)
instance (Out s,Out t) => Out (CompStmt s t)
instance (Out s,Out t) => Out (Stmt s t)
instance (Out s,Out t) => Out (Expr s t)
instance (Out s,Out t) => Out (SetCommand s t)
instance (Out s,Out t) => Out (VarIdent s t)
instance (Out s,Out t) => Out (FunIdent s t)
instance (Out s,Out t) => Out (CmdIdent s t)
instance (Out s,Out t) => Out (Redirect s t)
instance Out i => Out (Indexing i)
instance (Out s,Out t) => Out (VarRef s t)
instance (Out s,Out t) => Out (VarDef s t)
instance (Out s,Out t) => Out (CmdRef s t)
instance Out FileMode
instance Out Fd
instance Out Export
instance Out Scope
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
