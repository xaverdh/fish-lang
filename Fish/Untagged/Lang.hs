{-# LANGUAGE MultiParamTypeClasses, DeriveGeneric, FunctionalDependencies #-}
module Fish.Untagged.Lang
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
  , Redirect(..)
  , VarRef(..)
  , VarDef(..)
  , CmdRef(..) )
where

import qualified Data.List.NonEmpty as N
import qualified Data.Text as T
import Data.NText
import Data.Bifunctor
import GHC.Generics

import qualified Fish.Lang as Tagged
import Fish.Lang.Prim

class RemovableTags f g | f -> g where
  untag :: f t -> g
  tagUnit :: g -> f ()


-- | A fish program, consisting of several (composite) statements.
data Prog s = Prog [CompStmt s]
  deriving (Eq,Ord,Show,Generic)

instance RemovableTags (Tagged.Prog s) (Prog s) where
  untag (Tagged.Prog _ stmts) = Prog (fmap untag stmts)
  tagUnit (Prog stmts) = Tagged.Prog () (fmap tagUnit stmts)


-- | A list of arguments, belonging to a command.
data Args s = Args [Expr s]
  deriving (Eq,Ord,Show,Generic)

instance RemovableTags (Tagged.Args s) (Args s) where
  untag (Tagged.Args _ exprs) = Args (fmap untag exprs)
  tagUnit (Args exprs) = Tagged.Args () (fmap tagUnit exprs)

-- | A composite statement.
data CompStmt s =
  Simple (Stmt s)
  -- ^ Wraps a simple statement
  | Piped Fd (Stmt s) (CompStmt s)
  -- ^ A pipe connecting a simple statement and a composite statement
  | Forked (Stmt s)
  -- ^ A forked statement
  deriving (Eq,Ord,Show,Generic)

instance RemovableTags (Tagged.CompStmt s) (CompStmt s) where
  untag a = case a of
    Tagged.Simple _ stmt -> Simple (untag stmt)
    Tagged.Piped _ fd stmt cstmt -> Piped fd (untag stmt) (untag cstmt)
    Tagged.Forked _ stmt -> Forked (untag stmt)
  
  tagUnit a = case a of
    Simple stmt -> Tagged.Simple () (tagUnit stmt)
    Piped fd stmt cstmt -> Tagged.Piped () fd (tagUnit stmt) (tagUnit cstmt)
    Forked stmt -> Tagged.Forked () (tagUnit stmt)


data Stmt s = 
  CommentSt T.Text
  -- ^ A /comment/
  | CmdSt (CmdIdent s) (Args s)
  -- ^ A /shell command/, has an identifier and arguments
  | SetSt (SetCommand s)
  -- ^ The /set/ builtin command
  | FunctionSt (FunIdent s) (Args s) (Prog s)
  -- ^ The /function/ builtin command
  | WhileSt (Stmt s) (Prog s)
  -- ^ /while/ loops
  | ForSt (VarIdent s) (Args s) (Prog s)
  -- ^ /for/ loops
  | IfSt (N.NonEmpty (Stmt s,Prog s)) (Maybe (Prog s))
  -- ^ /if/ statements
  | SwitchSt (Expr s) (N.NonEmpty (Expr s,Prog s))
  -- ^ /switch/ statements
  | BeginSt (Prog s)
  -- ^ /begin/ statements
  | AndSt (Stmt s)
  -- ^ /and/ statement modifier
  | OrSt (Stmt s)
  -- ^ /or/ statement modifier
  | NotSt (Stmt s)
  -- ^ /not/ statement modifier
  | RedirectedSt (Stmt s) (N.NonEmpty (Redirect s))
  -- ^ A 'Stmt', annotated with redirections
  deriving (Eq,Ord,Show,Generic)


instance RemovableTags (Tagged.Stmt s) (Stmt s) where
  untag a = case a of
    Tagged.CommentSt _ txt -> CommentSt txt
    Tagged.CmdSt _ i args -> CmdSt (untag i) (untag args)
    Tagged.SetSt _ scmd ->  SetSt (untag scmd)
    Tagged.FunctionSt _ i args prog -> FunctionSt (untag i) (untag args) (untag prog)
    Tagged.WhileSt _ stmt prog -> WhileSt (untag stmt) (untag prog)
    Tagged.ForSt _ i args p -> ForSt (untag i) (untag args) (untag p)
    Tagged.IfSt _ branches mprog -> IfSt (fmap (bimap untag untag) branches) (fmap untag mprog)
    Tagged.SwitchSt _ e branches -> SwitchSt (untag e) (fmap (bimap untag untag) branches)
    Tagged.BeginSt _ prog -> BeginSt (untag prog)
    Tagged.AndSt _ stmt -> AndSt (untag stmt)
    Tagged.OrSt _ stmt -> OrSt (untag stmt)
    Tagged.NotSt _ stmt -> NotSt (untag stmt)
    Tagged.RedirectedSt _ stmt redirs -> RedirectedSt (untag stmt) (fmap untag redirs)

  tagUnit a = case a of
    CommentSt txt -> Tagged.CommentSt () txt
    CmdSt i args -> Tagged.CmdSt () (tagUnit i) (tagUnit args)
    SetSt scmd -> Tagged.SetSt () (tagUnit scmd)
    FunctionSt i args prog -> Tagged.FunctionSt () (tagUnit i) (tagUnit args) (tagUnit prog)
    WhileSt stmt prog -> Tagged.WhileSt () (tagUnit stmt) (tagUnit prog)
    ForSt i args p -> Tagged.ForSt () (tagUnit i) (tagUnit args) (tagUnit p)
    IfSt branches mprog -> Tagged.IfSt () (fmap (bimap tagUnit tagUnit) branches) (fmap tagUnit mprog)
    SwitchSt e branches -> Tagged.SwitchSt () (tagUnit e) (fmap (bimap tagUnit tagUnit) branches)
    BeginSt prog -> Tagged.BeginSt () (tagUnit prog)
    AndSt stmt -> Tagged.AndSt () (tagUnit stmt)
    OrSt stmt -> Tagged.OrSt () (tagUnit stmt)
    NotSt stmt -> Tagged.NotSt () (tagUnit stmt)
    RedirectedSt stmt redirs -> Tagged.RedirectedSt () (tagUnit stmt) (fmap tagUnit redirs)


data Expr s =
  StringE s
  -- ^ String expressions, can be \"..\"-type, \'..\'-type or plain strings.
  | GlobE Glob
  -- ^ Glob patterns.
  | ProcE (Expr s)
  -- ^ Process expansion, i.e. %last .
  | HomeDirE
  -- ^ Home directory expansion, i.e. ~ .
  | VarRefE Bool (VarRef s)
  -- ^ Variable references, i.e. $a. The boolean
  -- keeps track of whether the variable occured in \"\"-quotes.
  | BracesE [Expr s]
  -- ^ Braces expansion, i.e. {..}.
  | CmdSubstE (CmdRef s)
  -- ^ Command substitution,, i.e. (..).
  | ConcatE (Expr s) (Expr s)
  -- ^ One expression following the other without seperating whitespace.
  deriving (Eq,Ord,Show,Generic)

instance RemovableTags (Tagged.Expr s) (Expr s) where
  untag a = case a of
    Tagged.StringE _ s -> StringE s
    Tagged.GlobE _ g -> GlobE g
    Tagged.ProcE _ e -> ProcE (untag e)
    Tagged.HomeDirE _ -> HomeDirE
    Tagged.VarRefE _ b vref -> VarRefE b (untag vref)
    Tagged.BracesE _ es -> BracesE (fmap untag es)
    Tagged.CmdSubstE _ cmdref -> CmdSubstE (untag cmdref)
    Tagged.ConcatE _  e1 e2 -> ConcatE (untag e1) (untag e2)

  tagUnit a = case a of
    StringE s -> Tagged.StringE () s
    GlobE g -> Tagged.GlobE () g
    ProcE e -> Tagged.ProcE () (tagUnit e)
    HomeDirE -> Tagged.HomeDirE ()
    VarRefE b vref -> Tagged.VarRefE () b (tagUnit vref)
    BracesE es -> Tagged.BracesE () (fmap tagUnit es)
    CmdSubstE cmdref -> Tagged.CmdSubstE () (tagUnit cmdref)
    ConcatE e1 e2 -> Tagged.ConcatE () (tagUnit e1) (tagUnit e2)


data SetCommand s = 
  SetSetting (Maybe Scope) (Maybe Export) (VarDef s) (Args s)
  -- ^ The /set/ builtin command in setting mode
  | SetList (Maybe Scope) (Maybe Export) Bool
  -- ^ The /set/ builtin command in list mode,
  --   boolean corresponds to the "-n" flag.
  | SetQuery (Maybe Scope) (Maybe Export) (Args s)
  -- ^ The /set/ builtin command in query mode
  | SetErase (Maybe Scope) (N.NonEmpty (VarDef s))
  -- ^ The /set/ builtin command in erase mode
  | SetHelp
  -- ^ The /set/ builtin command in help mode
  deriving (Eq,Ord,Show,Generic)


instance RemovableTags (Tagged.SetCommand s) (SetCommand s) where
  untag a = case a of
    Tagged.SetSetting sc ex vdef args ->
      SetSetting sc ex
        (untag vdef) (untag args)
    Tagged.SetList sc ex b -> SetList sc ex b
    Tagged.SetQuery sc ex args -> SetQuery sc ex (untag args)
    Tagged.SetErase sc vdefs -> SetErase sc (fmap untag vdefs)
    Tagged.SetHelp -> SetHelp 

  tagUnit a = case a of
    SetSetting sc ex vdef args ->
      Tagged.SetSetting sc ex
        (tagUnit vdef) (tagUnit args)
    SetList sc ex b -> Tagged.SetList sc ex b
    SetQuery sc ex args -> Tagged.SetQuery sc ex (tagUnit args)
    SetErase sc vdefs -> Tagged.SetErase sc (fmap tagUnit vdefs)
    SetHelp -> Tagged.SetHelp


-- | Variable identifiers
data VarIdent s = VarIdent NText
  deriving (Eq,Ord,Show,Generic)

instance RemovableTags (Tagged.VarIdent s) (VarIdent s) where
  untag (Tagged.VarIdent _ s) = (VarIdent s)
  tagUnit (VarIdent s) = Tagged.VarIdent () s

-- | Function identifiers
data FunIdent s = FunIdent NText
  deriving (Eq,Ord,Show,Generic)

instance RemovableTags (Tagged.FunIdent s) (FunIdent s) where
  untag (Tagged.FunIdent _ s) = (FunIdent s)
  tagUnit (FunIdent s) = Tagged.FunIdent () s

-- | Command name identifiers
data CmdIdent s = CmdIdent NText
  deriving (Eq,Ord,Show,Generic)

instance RemovableTags (Tagged.CmdIdent s) (CmdIdent s) where
  untag (Tagged.CmdIdent _ s) = (CmdIdent s)
  tagUnit (CmdIdent s) = Tagged.CmdIdent () s


-- | Type of a redirection, the first file descriptor
--   is the fd being redirected, the second part is
--   the target.
--
--   It can be either another fd or a file,
--   in which case the boolean tells us whether it should
--   be overwritten (False) or appended to (True).
data Redirect s = 
  RedirectClose Fd
  | RedirectIn Fd ( Either Fd (Expr s) )
  | RedirectOut Fd ( Either Fd (FileMode,Expr s) )
  deriving (Eq,Ord,Show,Generic)

instance RemovableTags (Tagged.Redirect s) (Redirect s) where
  untag a = case a of
    Tagged.RedirectClose fd -> RedirectClose fd
    Tagged.RedirectIn fd fd_or_e -> RedirectIn fd (second untag fd_or_e)
    Tagged.RedirectOut fd fd_or_target -> RedirectOut fd (second (second untag) fd_or_target)
  
  tagUnit a = case a of
    RedirectClose fd -> Tagged.RedirectClose fd
    RedirectIn fd fd_or_e -> Tagged.RedirectIn fd (second tagUnit fd_or_e)
    RedirectOut fd fd_or_target -> Tagged.RedirectOut fd (second (second tagUnit) fd_or_target)


-- | A variable reference starting with a name,
--   which may be
--
--   * another variable reference
--   * a variable identifier
--
--   potentially followed by an index expression.
data VarRef s = VarRef
  ( Either (VarRef s) (VarIdent s) )
  ( Ref (Expr s) )
  deriving (Eq,Ord,Show,Generic)

instance RemovableTags (Tagged.VarRef s) (VarRef s) where
  untag (Tagged.VarRef _ vref_or_i eref) = VarRef (bimap untag untag vref_or_i) (fmap (fmap (fmap untag)) eref)
  
  tagUnit (VarRef vref_or_i eref) = Tagged.VarRef () (bimap tagUnit tagUnit vref_or_i) (fmap (fmap (fmap tagUnit)) eref)

-- | A variable definition expression, belonging to
--   the set builtin.
--
--   The only difference from 'VarRef'
--   is that the name must be a statically known identifier.
data VarDef s = VarDef
  ( VarIdent s )
  ( Ref (Expr s) )
  deriving (Eq,Ord,Show,Generic)

instance RemovableTags (Tagged.VarDef s) (VarDef s) where
  untag (Tagged.VarDef _ i eref) = VarDef (untag i) (fmap (fmap (fmap untag)) eref)
  
  tagUnit (VarDef i eref) = Tagged.VarDef () (tagUnit i) (fmap (fmap (fmap tagUnit)) eref)


-- | A command reference, given by a piece of fish code and
--   an optional index expression.
data CmdRef s = CmdRef (Prog s) (Ref (Expr s))
  deriving (Eq,Ord,Show,Generic)

instance RemovableTags (Tagged.CmdRef s) (CmdRef s) where
  untag (Tagged.CmdRef _ prog eref) = CmdRef (untag prog) (fmap (fmap (fmap untag)) eref)
  
  tagUnit (CmdRef prog eref) = Tagged.CmdRef () (tagUnit prog) (fmap (fmap (fmap tagUnit)) eref)


