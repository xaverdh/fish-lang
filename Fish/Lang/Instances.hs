{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}
module Fish.Lang.Instances where

import Fish.Lang

deriving instance (Show s,ForallX Show t) => Show (Prog s t)
deriving instance (Show s,ForallX Show t) => Show (Args s t)
deriving instance (Show s,ForallX Show t) => Show (CompStmt s t)
deriving instance (Show s,ForallX Show t) => Show (Stmt s t)
deriving instance (Show s,ForallX Show t) => Show (Expr s t)
deriving instance (Show s,ForallX Show t) => Show (SetCommand s t)
deriving instance (Show s,ForallX Show t) => Show (VarIdent s t)
deriving instance (Show s,ForallX Show t) => Show (FunIdent s t)
deriving instance (Show s,ForallX Show t) => Show (CmdIdent s t)
deriving instance (Show s,ForallX Show t) => Show (VarRef s t)
deriving instance (Show s,ForallX Show t) => Show (VarDef s t)
deriving instance (Show s,ForallX Show t) => Show (CmdRef s t)
                
deriving instance (Eq s,ForallX Eq t) => Eq (Prog s t)
deriving instance (Eq s,ForallX Eq t) => Eq (Args s t)
deriving instance (Eq s,ForallX Eq t) => Eq (CompStmt s t)
deriving instance (Eq s,ForallX Eq t) => Eq (Stmt s t)
deriving instance (Eq s,ForallX Eq t) => Eq (Expr s t)
deriving instance (Eq s,ForallX Eq t) => Eq (SetCommand s t)
deriving instance (Eq s,ForallX Eq t) => Eq (VarIdent s t)
deriving instance (Eq s,ForallX Eq t) => Eq (FunIdent s t)
deriving instance (Eq s,ForallX Eq t) => Eq (CmdIdent s t)
deriving instance (Eq s,ForallX Eq t) => Eq (VarRef s t)
deriving instance (Eq s,ForallX Eq t) => Eq (VarDef s t)
deriving instance (Eq s,ForallX Eq t) => Eq (CmdRef s t)

deriving instance (Ord s,ForallX Ord t) => Ord (Prog s t)
deriving instance (Ord s,ForallX Ord t) => Ord (Args s t)
deriving instance (Ord s,ForallX Ord t) => Ord (CompStmt s t)
deriving instance (Ord s,ForallX Ord t) => Ord (Stmt s t)
deriving instance (Ord s,ForallX Ord t) => Ord (Expr s t)
deriving instance (Ord s,ForallX Ord t) => Ord (SetCommand s t)
deriving instance (Ord s,ForallX Ord t) => Ord (VarIdent s t)
deriving instance (Ord s,ForallX Ord t) => Ord (FunIdent s t)
deriving instance (Ord s,ForallX Ord t) => Ord (CmdIdent s t)
deriving instance (Ord s,ForallX Ord t) => Ord (VarRef s t)
deriving instance (Ord s,ForallX Ord t) => Ord (VarDef s t)
deriving instance (Ord s,ForallX Ord t) => Ord (CmdRef s t)



