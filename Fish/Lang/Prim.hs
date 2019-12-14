{-# LANGUAGE DeriveFunctor, DeriveGeneric #-}
module Fish.Lang.Prim where

import GHC.Generics

-- | Modes for writing to a file:
--
--   * 'FModeWrite'  : overwrite existing file
--   * 'FModeApp'    : append to existing file
--   * 'FModeNoClob' : refuse to write to existing file
data FileMode = FModeWrite | FModeApp | FModeNoClob
  deriving (Eq,Ord,Show,Bounded,Enum,Generic)

-- | A unix file descriptor from 0 to 9
data Fd =
    Fd0 | Fd1 | Fd2 | Fd3 | Fd4
  | Fd5 | Fd6 | Fd7 | Fd8 | Fd9
  deriving (Eq,Ord,Show,Bounded,Enum,Generic)

-- | Glob pattern, can be one of * **
data Glob =
  StarGl
  | DiStarGl
  deriving (Eq,Ord,Show,Bounded,Enum,Generic)

-- | An Index from an [..] index expression.
--   Can be either a single index or an index range.
data Indexing i = Index i | Range i i
  deriving (Eq,Ord,Show,Functor,Generic)

-- | An optional index expression, consisting
--   of a list of indices / index ranges.
type Ref i = Maybe [Indexing i]

-- | Type of a redirection, the first file descriptor
--   is the fd being redirected, the second part is
--   the target.
--
--   It can be either another fd or an expression for a file,
--   in which case the boolean tells us whether the file 
--   should be overwritten (False) or appended to (True).
data Redirect e =
  RedirectClose Fd
  | RedirectIn Fd ( Either Fd e )
  | RedirectOut Fd ( Either Fd (FileMode,e) )
  deriving (Eq,Ord,Show,Functor,Generic)




