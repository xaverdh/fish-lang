module Fish.Pretty where

import Fish.Lang
import Text.PrettyPrint.GenericPretty
import GHC.Generics

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.List.NonEmpty as N

instance Out a => Out (N.NonEmpty a)
instance Out t => Out (Prog t)
instance Out t => Out (Args t)
instance Out t => Out (CompStmt t)
instance Out t => Out (Stmt t)
instance Out t => Out (Expr t)
instance Out t => Out (SetCommand t)
instance Out t => Out (VarIdent t)
instance Out t => Out (FunIdent t)
instance Out t => Out (CmdIdent t)
instance Out t => Out (Redirect t)
instance Out i => Out (Indexing i)
instance Out t => Out (VarRef t)
instance Out t => Out (VarDef t)
instance Out t => Out (CmdRef t)
instance Out FileMode
instance Out Fd
instance Out Export
instance Out Scope
instance Out Glob

instance Out B.ByteString where
  doc = doc . BC.unpack
  docPrec i = docPrec i . BC.unpack



