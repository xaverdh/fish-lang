{-# LANGUAGE TypeFamilies #-}
module Fish.Lang.Unit.Instances where

import Fish.Lang

type instance XProg () = ()
type instance XArgs () = ()
type instance XSimple () = ()
type instance XPiped () = ()
type instance XForked () = ()
type instance XCommentSt () = ()
type instance XCmdSt () = ()
type instance XSetSt () = ()
type instance XFunctionSt () = ()
type instance XWhileSt () = ()
type instance XForSt () = ()
type instance XIfSt () = ()
type instance XSwitchSt () = ()
type instance XBeginSt () = ()
type instance XAndSt () = ()
type instance XOrSt () = ()
type instance XNotSt () = ()
type instance XRedirectedSt () = ()
type instance XStringE () = ()
type instance XGlobE () = ()
type instance XHomeDirE () = ()
type instance XVarRefE () = ()
type instance XBracesE () = ()
type instance XCmdSubstE () = ()
type instance XConcatE () = ()
type instance XVarIdent () = ()
type instance XFunIdent () = ()
type instance XCmdIdent () = ()
type instance XVarRef () = ()
type instance XVarDef () = ()
type instance XCmdRef () = ()


