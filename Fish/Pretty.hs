{-# language StandaloneDeriving, DeriveGeneric #-}
module Fish.Pretty where

import Fish.Lang
import Text.PrettyPrint.GenericPretty
import GHC.Generics

import Data.Text as T
import Data.Text.Internal
import Data.List.NonEmpty
import Data.Text.Array

instance Out a => Out (NonEmpty a)
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

instance Out Text where
  doc = doc . T.unpack
  docPrec i = docPrec i . T.unpack



