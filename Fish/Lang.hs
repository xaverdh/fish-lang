{-# LANGUAGE DeriveFunctor, DeriveGeneric #-}
module Fish.Lang
  ( module Fish.Lang.Prim
  , Prog(..)
  , Args(..)
  , CompStmt(..)
  , Stmt(..)
  , Expr(..)
  , SetCommand(..)
  , VarIdent(..)
  , FunIdent(..)
  , CmdIdent(..)
  , VarRef(..)
  , VarDef(..)
  , CmdRef(..) )
where

import qualified Data.List.NonEmpty as N
import qualified Data.Text as T
import Data.NText
import Data.Bifunctor
import GHC.Generics

import Fish.Lang.Prim

-- | A fish program, consisting of several (composite) statements.
data Prog s t = Prog t [CompStmt s t]
  deriving (Eq,Ord,Show,Functor,Generic)

-- | A list of arguments, belonging to a command.
data Args s t = Args t [Expr s t]
  deriving (Eq,Ord,Show,Functor,Generic)

-- | A composite statement.
data CompStmt s t =
  Simple t (Stmt s t)
  -- ^ Wraps a simple statement
  | Piped t Fd (Stmt s t) (CompStmt s t)
  -- ^ A pipe connecting a simple statement and a composite statement
  | Forked t (Stmt s t)
  -- ^ A forked statement
  deriving (Eq,Ord,Show,Functor,Generic)

data Stmt s t = 
  CommentSt t T.Text
  -- ^ A /comment/
  | CmdSt t (CmdIdent s t) (Args s t)
  -- ^ A /shell command/, has an identifier and arguments
  | SetSt t (SetCommand s t)
  -- ^ The /set/ builtin command
  | FunctionSt t (FunIdent s t) (Args s t) (Prog s t)
  -- ^ The /function/ builtin command
  | WhileSt t (Stmt s t) (Prog s t)
  -- ^ /while/ loops
  | ForSt t (VarIdent s t) (Args s t) (Prog s t)
  -- ^ /for/ loops
  | IfSt t (N.NonEmpty (Stmt s t,Prog s t)) (Maybe (Prog s t))
  -- ^ /if/ statements
  | SwitchSt t (Expr s t) (N.NonEmpty (Expr s t,Prog s t))
  -- ^ /switch/ statements
  | BeginSt t (Prog s t)
  -- ^ /begin/ statements
  | AndSt t (Stmt s t)
  -- ^ /and/ statement modifier
  | OrSt t (Stmt s t)
  -- ^ /or/ statement modifier
  | NotSt t (Stmt s t)
  -- ^ /not/ statement modifier
  | RedirectedSt t (Stmt s t) (N.NonEmpty (Redirect (Expr s t)))
  -- ^ A 'Stmt', annotated with redirections
  deriving (Eq,Ord,Show,Functor,Generic)

data Expr s t =
  StringE t s
  -- ^ String expressions, can be \"..\"-type, \'..\'-type or plain strings.
  | GlobE t Glob
  -- ^ Glob patterns.
  | ProcE t (Expr s t)
  -- ^ Process expansion, i.e. %last .
  | HomeDirE t
  -- ^ Home directory expansion, i.e. ~ .
  | VarRefE t Bool (VarRef s t)
  -- ^ Variable references, i.e. $a. The boolean
  -- keeps track of whether the variable occured in \"\"-quotes.
  | BracesE t [Expr s t]
  -- ^ Braces expansion, i.e. {..}.
  | CmdSubstE t (CmdRef s t)
  -- ^ Command substitution,, i.e. (..).
  | ConcatE t (Expr s t) (Expr s t)
  -- ^ One expression following the other without seperating whitespace.
  deriving (Eq,Ord,Show,Functor,Generic)

data SetCommand s t = 
  SetSetting (Maybe Scope) (Maybe Export) (VarDef s t) (Args s t)
  -- ^ The /set/ builtin command in setting mode
  | SetList (Maybe Scope) (Maybe Export) Bool
  -- ^ The /set/ builtin command in list mode,
  --   boolean corresponds to the "-n" flag.
  | SetQuery (Maybe Scope) (Maybe Export) (Args s t)
  -- ^ The /set/ builtin command in query mode
  | SetErase (Maybe Scope) (N.NonEmpty (VarDef s t))
  -- ^ The /set/ builtin command in erase mode
  | SetHelp
  -- ^ The /set/ builtin command in help mode
  deriving (Eq,Ord,Show,Functor,Generic)

-- | Variable identifiers
data VarIdent s t = VarIdent t NText
  deriving (Eq,Ord,Show,Functor,Generic)

-- | Function identifiers
data FunIdent s t = FunIdent t NText
  deriving (Eq,Ord,Show,Functor,Generic)

-- | Command name identifiers
data CmdIdent s t = CmdIdent t NText
  deriving (Eq,Ord,Show,Functor,Generic)

-- | A variable reference starting with a name,
--   which may be
--
--   * another variable reference
--   * a variable identifier
--
--   potentially followed by an index expression.
data VarRef s t = VarRef t
  ( Either (VarRef s t) (VarIdent s t) )
  ( Ref (Expr s t) )
  deriving (Eq,Ord,Show,Generic)

instance Functor (VarRef s) where
  fmap f (VarRef t a b) = 
    VarRef (f t)
    ( bimap (fmap f) (fmap f) a )
    ( fmap (map (fmap $ fmap f)) b )

-- | A variable definition expression, belonging to
--   the set builtin.
--
--   The only difference from 'VarRef'
--   is that the name must be a statically known identifier.
data VarDef s t = VarDef t
  ( VarIdent s t )
  ( Ref (Expr s t) )
  deriving (Eq,Ord,Show,Functor,Generic)

-- | A command reference, given by a piece of fish code and
--   an optional index expression.
data CmdRef s t = CmdRef t (Prog s t) (Ref (Expr s t))
  deriving (Eq,Ord,Show,Functor,Generic)


