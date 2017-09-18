{-# LANGUAGE FlexibleInstances, LambdaCase #-}
module Fish.Lang.Base.Conversion where

import Fish.Lang
import Fish.Lang.Base.Instances
import Fish.Lang.Base.Patterns

class ToBase f where
  toBase :: f t -> f Base


instance ToBase (Prog s) where
  toBase (Prog _ stmts) = ProgB $ toBase <$> stmts

instance ToBase (Args s) where
  toBase (Args _ exprs) = ArgsB $ toBase <$> exprs

instance ToBase (CompStmt s) where
  toBase = \case
    Simple _ stmts -> SimpleB stmts
    Piped _ fd stmts cstmts -> PipedB fd stmts cstmts
    Forked _ stmts -> ForkedB stmts

instance ToBase (Stmt s) where
  toBase = \case
    CommentSt _ text -> CommentStB text
    CmdSt _ ident args -> CmdStB ident args
    SetSt _ cmd -> SetStB cmd
    FunctionSt _ ident args stmt -> FunctionStB ident args stmt
    WhileSt _ stmt prog -> WhileStB stmt prog
    ForSt _ ident args prog -> ForStB ident args prog
    IfSt _ branches elseb -> IfStB branches elseb
    SwitchSt _ expr cases -> SwitchStB expr cases
    BeginSt _ prog -> BeginStB prog
    AndSt _ stmt -> AndStB stmt
    OrSt _ stmt -> OrStB stmt
    NotSt _ stmt -> NotStB stmt
    RedirectedSt _ stmt redirects -> RedirectedStB stmt redirects

instance ToBase (Expr s) where
  toBase = \case
    StringE _ s -> StringEB s
    GlobE _ g -> GlobEB g
    ProcE _ e -> ProcEB e
    HomeDirE _ -> HomeDirEB
    VarRefE _ b vref -> VarRefEB b vref
    BracesE _ exprs -> BracesEB exprs
    CmdSubstE _ cref -> CmdSubstEB cref
    ConcatE _ e1 e2 -> ConcatEB e1 e2

instance ToBase (SetCommand s) where
  toBase = \case
    SetSetting mscp mex vdef args ->
      SetSetting mscp mex (toBase vdef) (toBase args)
    SetList mscp mex b -> SetList mscp mex b
    SetQuery mscp mex args -> SetQuery mscp mex $ toBase args
    SetErase mscp vdefs -> SetErase mscp $ toBase <$> vdefs
    SetHelp -> SetHelp

instance ToBase (VarIdents s) where
  toBase (VarIdent _ ntext) = VarIdentB ntext

instance ToBase (FunIdent s) where
  toBase (FunIdent _ ntext) = FunIdentB ntext

instance ToBase (CmdIdent s) where
  toBase (CmdIdent _ ntext) = CmdIdentB ntext

instance ToBase (VarRef s) where
  toBase (VarRef _ vref_or_ident ref) = VarRefB vref_or_ident ref

instance ToBase (VarDef s) where
  toBase (VarDef _ ident ref) = VarDefB ident ref

instance ToBase (CmdRef s) where
  toBase (CmdRef _ prog ref) = CmdRefB prog ref


-- TODO: [..]

