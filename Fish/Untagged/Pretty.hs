module Fish.Untagged.Pretty where

import Fish.Untagged.Lang
import Fish.Lang.Prim
import Fish.Lang.Prim.Pretty

import Text.PrettyPrint.GenericPretty
import GHC.Generics

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
instance Out t => Out (VarRef t)
instance Out t => Out (VarDef t)
instance Out t => Out (CmdRef t)



