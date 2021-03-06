{-# LANGUAGE DeriveGeneric, TypeFamilies, ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts, StandaloneDeriving #-}
module Fish.Lang
  ( module Fish.Lang.Prim
  , Prog(..)
  , Exprs(..)
  , CompStmt(..)
  , Stmt(..)
  , Expr(..)
  , VarIdent(..)
  , FunIdent(..)
  , VarRef(..)
  , CmdRef(..)
  , ForallX(..)
  , XProg
  , XExprs
  , XSimple
  , XPiped
  , XForked
  , XCommentSt
  , XCmdSt
  , XFunctionSt
  , XWhileSt
  , XForSt
  , XIfSt
  , XSwitchSt
  , XBeginSt
  , XAndSt
  , XOrSt
  , XNotSt
  , XRedirectedSt
  , XStringE
  , XGlobE
  , XHomeDirE
  , XVarRefE
  , XBracesE
  , XCmdSubstE
  , XConcatE
  , XVarIdent
  , XFunIdent
  , XVarRef
  , XCmdRef )
where

import qualified Data.List.NonEmpty as N
import qualified Data.Text as T
import Data.NText
import Data.Bifunctor
import GHC.Generics
import GHC.Exts (Constraint)

import Fish.Lang.Prim


type ForallX (f :: * -> Constraint) t =
  ( f (XProg t), f (XExprs t), f (XSimple t)
  , f (XPiped t), f (XForked t), f (XCommentSt t)
  , f (XCmdSt t), f (XFunctionSt t)
  , f (XWhileSt t), f (XForSt t), f (XIfSt t)
  , f (XSwitchSt t), f (XBeginSt t), f (XAndSt t)
  , f (XOrSt t), f (XNotSt t), f (XRedirectedSt t)
  , f (XStringE t), f (XGlobE t), f (XHomeDirE t)
  , f (XVarRefE t), f (XBracesE t), f (XCmdSubstE t)
  , f (XConcatE t), f (XVarIdent t), f (XFunIdent t)
  , f (XVarRef t) , f (XCmdRef t) )

data Prog s t = Prog (XProg t) [CompStmt s t]
  deriving (Generic)

type family XProg t

-- | A list of arguments, belonging to a command.
data Exprs s t = Exprs (XExprs t) [Expr s t]
  deriving (Generic)

type family XExprs t

-- | A composite statement.
data CompStmt s t =
  Simple (XSimple t) (Stmt s t)
  -- ^ Wraps a simple statement
  | Piped (XPiped t) Fd (Stmt s t) (CompStmt s t)
  -- ^ A pipe connecting a simple statement and a composite statement
  | Forked (XForked t) (Stmt s t)
  -- ^ A forked statement
  deriving (Generic)

type family XSimple t
type family XPiped t
type family XForked t


data Stmt s t = 
  CommentSt (XCommentSt t) T.Text
  -- ^ A /comment/
  | CmdSt (XCmdSt t) (N.NonEmpty (Expr s t))
  -- ^ A /shell command/, has an identifier and arguments
  | FunctionSt (XFunctionSt t) (FunIdent s t) (Exprs s t) (Prog s t)
  -- ^ The /function/ builtin command
  | WhileSt (XWhileSt t) (Stmt s t) (Prog s t)
  -- ^ /while/ loops
  | ForSt (XForSt t) (VarIdent s t) (Exprs s t) (Prog s t)
  -- ^ /for/ loops
  | IfSt (XIfSt t) ( N.NonEmpty (Stmt s t,Prog s t) )
                   ( Maybe (Prog s t) )
  -- ^ /if/ statements
  | SwitchSt (XSwitchSt t) (Expr s t)
             ( N.NonEmpty (Expr s t,Prog s t) )
  -- ^ /switch/ statements
  | BeginSt (XBeginSt t) (Prog s t)
  -- ^ /begin/ statements
  | AndSt (XAndSt t) (Stmt s t)
  -- ^ /and/ statement modifier
  | OrSt (XOrSt t) (Stmt s t)
  -- ^ /or/ statement modifier
  | NotSt (XNotSt t) (Stmt s t)
  -- ^ /not/ statement modifier
  | RedirectedSt (XRedirectedSt t) (Stmt s t)
                 ( N.NonEmpty (Redirect (Expr s t)) )
  -- ^ A 'Stmt', annotated with redirections
  deriving (Generic)

type family XCommentSt t
type family XCmdSt t
type family XFunctionSt t
type family XWhileSt t
type family XForSt t
type family XIfSt t
type family XSwitchSt t
type family XBeginSt t
type family XAndSt t
type family XOrSt t
type family XNotSt t
type family XRedirectedSt t


data Expr s t =
  StringE (XStringE t) s
  -- ^ String expressions, can be \"..\"-type, \'..\'-type or plain strings.
  | GlobE (XGlobE t) Glob
  -- ^ Glob patterns.
  | HomeDirE (XHomeDirE t)
  -- ^ Home directory expansion, i.e. ~ .
  | VarRefE (XVarRefE t) Bool (VarRef s t)
  -- ^ Variable references, i.e. $a. The boolean
  -- keeps track of whether the variable occured in a quoted context.
  | BracesE (XBracesE t) [Expr s t]
  -- ^ Braces expansion, i.e. {..}.
  | CmdSubstE (XCmdSubstE t) (CmdRef s t)
  -- ^ Command substitution,, i.e. (..).
  | ConcatE (XConcatE t) (Expr s t) (Expr s t)
  -- ^ One expression following the other without seperating whitespace.
  deriving (Generic)

type family XStringE t
type family XGlobE t
type family XHomeDirE t
type family XVarRefE t
type family XBracesE t
type family XCmdSubstE t
type family XConcatE t

-- | Variable identifiers
data VarIdent s t = VarIdent (XVarIdent t) NText
  deriving (Generic)

type family XVarIdent t

-- | Function identifiers
data FunIdent s t = FunIdent (XFunIdent t) NText
  deriving (Generic)

type family XFunIdent t

-- | A variable reference starting with a name,
--   which may be
--
--   * another variable reference
--   * a variable identifier
--
--   potentially followed by an index expression.
data VarRef s t = VarRef (XVarRef t)
  ( Either (VarRef s t) (VarIdent s t) )
  ( Ref (Expr s t) )
  deriving (Generic)

type family XVarRef t

-- | A command reference, given by a piece of fish code and
--   an optional index expression.
data CmdRef s t = CmdRef (XCmdRef t) (Prog s t) (Ref (Expr s t))
  deriving (Generic)

type family XCmdRef t


