{-# LANGUAGE FlexibleInstances, LambdaCase #-}
module Fish.Lang.Base.Conversion where

import Fish.Lang
import Fish.Lang.Base.Instances
import Fish.Lang.Base.Patterns

import Data.Bifunctor


class ToBase f where
  toBase :: f t -> f Base


instance ToBase (Prog s) where
  toBase (Prog _ stmts) = ProgB $ toBase <$> stmts

instance ToBase (Args s) where
  toBase (Args _ exprs) = ArgsB $ toBase <$> exprs

instance ToBase (CompStmt s) where
  toBase = \case
    Simple _ stmt -> SimpleB $ toBase stmt
    Piped _ fd stmt cstmt -> 
      PipedB fd (toBase stmt) (toBase cstmt)
    Forked _ stmt -> ForkedB $ toBase stmt

instance ToBase (Stmt s) where
  toBase = \case
    CommentSt _ text -> CommentStB text
    CmdSt _ ident args ->
      CmdStB (toBase ident) $ toBase args
    SetSt _ cmd -> SetStB $ toBase cmd
    FunctionSt _ ident args stmt ->
      FunctionStB (toBase ident) (toBase args) (toBase stmt)
    WhileSt _ stmt prog ->
      WhileStB (toBase stmt) $ toBase prog
    ForSt _ ident args prog ->
      ForStB (toBase ident) (toBase args) $ toBase prog
    IfSt _ branches elseb -> 
      IfStB (bimap toBase toBase <$> branches) $ toBase <$> elseb
    SwitchSt _ expr cases ->
      SwitchStB (toBase expr) $ bimap toBase toBase <$> cases
    BeginSt _ prog -> BeginStB $ toBase prog
    AndSt _ stmt -> AndStB $ toBase stmt
    OrSt _ stmt -> OrStB $ toBase stmt
    NotSt _ stmt -> NotStB $ toBase stmt
    RedirectedSt _ stmt redirects ->
      RedirectedStB (toBase stmt) $ fmap toBase <$> redirects

instance ToBase (Expr s) where
  toBase = \case
    StringE _ s -> StringEB s
    GlobE _ g -> GlobEB g
    ProcE _ e -> ProcEB $ toBase e
    HomeDirE _ -> HomeDirEB
    VarRefE _ b vref -> VarRefEB b $ toBase vref
    BracesE _ exprs -> BracesEB $ toBase <$> exprs
    CmdSubstE _ cref -> CmdSubstEB $ toBase cref
    ConcatE _ e1 e2 -> ConcatEB (toBase e1) $ toBase e2

instance ToBase (SetCommand s) where
  toBase = \case
    SetSetting mscp mex vdef args ->
      SetSetting mscp mex (toBase vdef) (toBase args)
    SetList mscp mex b -> SetList mscp mex b
    SetQuery mscp mex args -> SetQuery mscp mex $ toBase args
    SetErase mscp vdefs -> SetErase mscp $ toBase <$> vdefs
    SetHelp -> SetHelp

instance ToBase (VarIdent s) where
  toBase (VarIdent _ ntext) = VarIdentB ntext

instance ToBase (FunIdent s) where
  toBase (FunIdent _ ntext) = FunIdentB ntext

instance ToBase (CmdIdent s) where
  toBase (CmdIdent _ ntext) = CmdIdentB ntext

mapRef :: (a -> b) -> Ref a -> Ref b 
mapRef = fmap . fmap . fmap

instance ToBase (VarRef s) where
  toBase (VarRef _ vref_or_ident ref) =
    VarRefB (bimap toBase toBase vref_or_ident) $ mapRef toBase ref

instance ToBase (VarDef s) where
  toBase (VarDef _ ident ref) =
    VarDefB (toBase ident) $ mapRef toBase ref

instance ToBase (CmdRef s) where
  toBase (CmdRef _ prog ref) =
    CmdRefB (toBase prog) $ mapRef toBase ref



