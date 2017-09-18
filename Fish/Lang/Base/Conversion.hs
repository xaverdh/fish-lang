{-# LANGUAGE FlexibleInstances #-}
module Fish.Lang.Base.Conversion where

import Fish.Lang
import Fish.Lang.Instances
import Fish.Lang.Patterns

class ToBase f where
  toBase :: f t -> f Base


instance ToBase (Prog s) where
  toBase (Prog _ stmts) = ProgB $ toBase stmts

-- TODO: [..]

