{-# LANGUAGE DeriveFunctor, DeriveGeneric #-}
module Fish.Lang where

import qualified Data.List.NonEmpty as N
import Data.Bifunctor
import GHC.Generics

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
  CommentSt t s
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
  | RedirectedSt t (Stmt s t) (N.NonEmpty (Redirect s t))
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
  deriving (Eq,Ord,Show,Functor,Generic)

-- | Export flag.
data Export = Export | UnExport
  deriving (Eq,Ord,Show,Bounded,Enum,Generic)

-- | A variable scope.
data Scope = 
  ScopeLocal
  | ScopeGlobal
  | ScopeUniversal
  deriving (Eq,Ord,Show,Bounded,Enum,Generic)

-- | Glob pattern, can be one of * ** ?
data Glob = 
  StarGl
  | DiStarGl
  | QMarkGl
  deriving (Eq,Ord,Show,Bounded,Enum,Generic)

-- | Variable identifiers
data VarIdent s t = VarIdent t s
  deriving (Eq,Ord,Show,Functor,Generic)

-- | Function identifiers
data FunIdent s t = FunIdent t s
  deriving (Eq,Ord,Show,Functor,Generic)

-- | Command name identifiers
data CmdIdent s t = CmdIdent t s
  deriving (Eq,Ord,Show,Functor,Generic)

-- | A unix file descriptor from 0 to 9
data Fd =
    Fd0 | Fd1 | Fd2 | Fd3 | Fd4
  | Fd5 | Fd6 | Fd7 | Fd8 | Fd9
  deriving (Eq,Ord,Show,Bounded,Enum,Generic)

-- | Type of a redirection, the first file descriptor
--   is the fd being redirected, the second part is
--   the target.
--
--   It can be either another fd or a file,
--   in which case the boolean tells us whether it should
--   be overwritten (False) or appended to (True).
data Redirect s t = 
  RedirectClose Fd
  | RedirectIn Fd ( Either Fd (Expr s t) )
  | RedirectOut Fd ( Either Fd (FileMode,Expr s t) )
  deriving (Eq,Ord,Show,Functor,Generic)

-- | Modes for writing to a file:
--
--   * 'FModeWrite'  : overwrite existing file
--   * 'FModeApp'    : append to existing file
--   * 'FModeNoClob' : refuse to write to existing file
data FileMode = FModeWrite | FModeApp | FModeNoClob
  deriving (Eq,Ord,Show,Bounded,Enum,Generic)
  

-- | An Index from an [..] index expression.
--   Can be either a single index or an index range.
data Indexing i = Index i | Range i i
  deriving (Eq,Ord,Show,Functor,Generic)

-- | An optional index expression, consisting
--   of a list of indices / index ranges.
type Ref i = Maybe [Indexing i]

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


