{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
module Fish.Lang.Pretty
  ()
where

import Fish.Lang.Prim.Pretty
import Fish.Lang.Base
import Fish.Lang

import Text.PrettyPrint.GenericPretty
import GHC.Generics

instance Out Void where
  doc = const mempty
  docPrec = const $ const mempty
  docList = const mempty

instance (Out s,ForallX Out t) => Out (Prog s t)
instance (Out s,ForallX Out t) => Out (Args s t)
instance (Out s,ForallX Out t) => Out (CompStmt s t)
instance (Out s,ForallX Out t) => Out (Stmt s t)
instance (Out s,ForallX Out t) => Out (Expr s t)
instance (Out s,ForallX Out t) => Out (SetCommand s t)
instance (Out s,ForallX Out t) => Out (VarIdent s t)
instance (Out s,ForallX Out t) => Out (FunIdent s t)
instance (Out s,ForallX Out t) => Out (CmdIdent s t)
instance (Out e) => Out (Redirect e)
instance (Out s,ForallX Out t) => Out (VarRef s t)
instance (Out s,ForallX Out t) => Out (VarDef s t)
instance (Out s,ForallX Out t) => Out (CmdRef s t)

