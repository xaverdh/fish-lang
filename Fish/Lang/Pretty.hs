module Fish.Lang.Pretty where

import Fish.Lang.Prim.Pretty
import Fish.Lang

import Text.PrettyPrint.GenericPretty
import GHC.Generics

instance (Out s,Out t) => Out (Prog s t)
instance (Out s,Out t) => Out (Args s t)
instance (Out s,Out t) => Out (CompStmt s t)
instance (Out s,Out t) => Out (Stmt s t)
instance (Out s,Out t) => Out (Expr s t)
instance (Out s,Out t) => Out (SetCommand s t)
instance (Out s,Out t) => Out (VarIdent s t)
instance (Out s,Out t) => Out (FunIdent s t)
instance (Out s,Out t) => Out (CmdIdent s t)
instance Out e => Out (Redirect e)
instance (Out s,Out t) => Out (VarRef s t)
instance (Out s,Out t) => Out (VarDef s t)
instance (Out s,Out t) => Out (CmdRef s t)

