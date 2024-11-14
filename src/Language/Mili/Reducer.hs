-- MIT License, Copyright (c) 2024 Marvin Borner

module Language.Mili.Reducer
  ( nf
  ) where

import           Data.Mili                      ( Nat(..)
                                                , Term(..)
                                                )
-- | Reduce term to normal form
nf :: Term -> Term
nf t = t
