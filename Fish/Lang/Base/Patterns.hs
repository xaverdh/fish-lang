{-# LANGUAGE PatternSynonyms #-}
module Fish.Lang.Base.Patterns where

import Fish.Lang
import Fish.Lang.Base.Instances
import Data.NText
import qualified Data.List.NonEmpty as N
import qualified Data.Text as T
import Data.Void

void :: Void
void = error "Attempt to evaluate void"

pattern ProgB :: [CompStmt s Base] -> Prog s Base
pattern ProgB stmts <- Prog _ stmts
  where ProgB stmts = Prog void stmts

pattern ArgsB :: [Expr s Base] -> Args s Base
pattern ArgsB exprs <- Args _ exprs
  where ArgsB exprs = Args void exprs

pattern SimpleB :: Stmt s Base -> CompStmt s Base
pattern SimpleB stmt <- Simple _ stmt
  where SimpleB stmt = Simple void stmt

pattern PipedB :: Fd -> Stmt s Base -> CompStmt s Base -> CompStmt s Base
pattern PipedB fd stmt cstmt <- Piped _ fd stmt cstmt
  where PipedB fd stmt cstmt = Piped void fd stmt cstmt

pattern ForkedB :: Stmt s Base -> CompStmt s Base
pattern ForkedB stmt <- Forked _ stmt
  where ForkedB stmt = Forked void stmt


pattern CommentStB :: T.Text -> Stmt s Base
pattern CommentStB text <- CommentSt _ text
  where CommentStB text = CommentSt void text

pattern CmdStB :: CmdIdent s Base -> Args s Base -> Stmt s Base
pattern CmdStB ident args <- CmdSt _ ident args
  where CmdStB ident args = CmdSt void ident args

pattern SetStB :: SetCommand s Base -> Stmt s Base
pattern SetStB cmd <- SetSt _ cmd
  where SetStB cmd = SetSt void cmd

pattern FunctionStB :: FunIdent s Base
  -> Args s Base -> Prog s Base -> Stmt s Base
pattern FunctionStB ident args stmt <-
  FunctionSt _ ident args stmt
  where FunctionStB ident args stmt = FunctionSt void ident args stmt

pattern WhileStB :: Stmt s Base -> Prog s Base -> Stmt s Base
pattern WhileStB stmt prog <- WhileSt _ stmt prog
  where WhileStB stmt prog = WhileSt void stmt prog

pattern ForStB :: VarIdent s Base
  -> Args s Base -> Prog s Base -> Stmt s Base
pattern ForStB ident args prog <- ForSt _ ident args prog
  where ForStB ident args prog = ForSt void ident args prog

pattern IfStB :: N.NonEmpty ( Stmt s Base,Prog s Base)
  -> Maybe (Prog s Base) -> Stmt s Base
pattern IfStB branches elseb <- IfSt _ branches elseb
  where IfStB branches elseb = IfSt void branches elseb

pattern SwitchStB :: Expr s Base
  -> N.NonEmpty (Expr s Base,Prog s Base) -> Stmt s Base
pattern SwitchStB expr cases <- SwitchSt _ expr cases
  where SwitchStB expr cases = SwitchSt void expr cases

pattern BeginStB :: Prog s Base -> Stmt s Base
pattern BeginStB prog <- BeginSt _ prog
  where BeginStB prog = BeginSt void prog

pattern AndStB :: Stmt s Base -> Stmt s Base
pattern AndStB stmt <- AndSt _ stmt
  where AndStB stmt = AndSt void stmt

pattern OrStB :: Stmt s Base -> Stmt s Base
pattern OrStB stmt <- OrSt _ stmt
  where OrStB stmt = OrSt void stmt

pattern NotStB :: Stmt s Base -> Stmt s Base
pattern NotStB stmt <- NotSt _ stmt 
  where NotStB stmt = NotSt void stmt

pattern RedirectedStB :: Stmt s Base
  -> N.NonEmpty (Redirect (Expr s Base)) -> Stmt s Base
pattern RedirectedStB stmt redirects <-
  RedirectedSt _ stmt redirects
  where
    RedirectedStB stmt redirects
      = RedirectedSt void stmt redirects

pattern StringEB :: s -> Expr s Base
pattern StringEB s <- StringE _ s
  where StringEB s = StringE void s

pattern GlobEB :: Glob -> Expr s Base
pattern GlobEB g <- GlobE _ g
  where GlobEB g = GlobE void g

pattern HomeDirEB :: Expr s Base
pattern HomeDirEB <- HomeDirE _
  where HomeDirEB = HomeDirE void

pattern VarRefEB :: Bool -> VarRef s Base -> Expr s Base
pattern VarRefEB b vref <- VarRefE _ b vref
  where VarRefEB b vref = VarRefE void b vref

pattern BracesEB :: [Expr s Base] -> Expr s Base
pattern BracesEB exprs <- BracesE _ exprs
  where BracesEB exprs = BracesE void exprs

pattern CmdSubstEB :: CmdRef s Base -> Expr s Base
pattern CmdSubstEB cref <- CmdSubstE _ cref
  where CmdSubstEB cref = CmdSubstE void cref

pattern ConcatEB :: Expr s Base -> Expr s Base -> Expr s Base
pattern ConcatEB e1 e2 <- ConcatE _ e1 e2
  where ConcatEB e1 e2 = ConcatE void e1 e2

pattern VarIdentB :: NText -> VarIdent s Base
pattern VarIdentB ntext <- VarIdent _ ntext
  where VarIdentB ntext = VarIdent void ntext

pattern FunIdentB :: NText -> FunIdent s Base
pattern FunIdentB ntext <- FunIdent _ ntext
  where FunIdentB ntext = FunIdent void ntext

pattern CmdIdentB :: NText -> CmdIdent s Base
pattern CmdIdentB ntext <- CmdIdent _ ntext 
  where CmdIdentB ntext = CmdIdent void ntext

pattern VarRefB :: Either (VarRef s Base) (VarIdent s Base)
  -> Ref (Expr s Base) -> VarRef s Base
pattern VarRefB vref_or_ident ref <-
  VarRef _ vref_or_ident ref
  where VarRefB vref_or_ident ref = VarRef void vref_or_ident ref

pattern VarDefB :: VarIdent s Base
  -> Ref (Expr s Base) -> VarDef s Base
pattern VarDefB ident ref <- VarDef _ ident ref
  where VarDefB ident ref = VarDef void ident ref

pattern CmdRefB :: Prog s Base
  -> Ref (Expr s Base) -> CmdRef s Base
pattern CmdRefB prog ref <- CmdRef _ prog ref
  where CmdRefB prog ref = CmdRef void prog ref






