{-# LANGUAGE TypeFamilies #-}
module Fish.Lang.Base.Instances
  ( Base )
where

import Fish.Lang
import Data.Void

data Base

type instance XProg Base = Void
type instance XArgs Base = Void
type instance XSimple Base = Void
type instance XPiped Base = Void
type instance XForked Base = Void
type instance XCommentSt Base = Void
type instance XCmdSt Base = Void
type instance XSetSt Base = Void
type instance XFunctionSt Base = Void
type instance XWhileSt Base = Void
type instance XForSt Base = Void
type instance XIfSt Base = Void
type instance XSwitchSt Base = Void
type instance XBeginSt Base = Void
type instance XAndSt Base = Void
type instance XOrSt Base = Void
type instance XNotSt Base = Void
type instance XRedirectedSt Base = Void
type instance XStringE Base = Void
type instance XGlobE Base = Void
type instance XProcE Base = Void
type instance XHomeDirE Base = Void
type instance XVarRefE Base = Void
type instance XBracesE Base = Void
type instance XCmdSubstE Base = Void
type instance XConcatE Base = Void
type instance XVarIdent Base = Void
type instance XFunIdent Base = Void
type instance XCmdIdent Base = Void
type instance XVarRef Base = Void
type instance XVarDef Base = Void
type instance XCmdRef Base = Void



