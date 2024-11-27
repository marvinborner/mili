-- MIT License, Copyright (c) 2024 Marvin Borner

module Language.Mili.Compiler
  ( compile
  ) where

import           Data.Mili                      ( Nat(..)
                                                , Term(..)
                                                , shift
                                                )

data Comp = CAbs String Comp                  -- | Abstraction with pointer
              | CApp Comp Comp                -- | Application
              | CVar                          -- | Namless variable
              | CNum Nat                      -- | Peano numeral
              | CRec Comp Comp Comp Comp Comp -- | Unbounded iteration

compile :: Term -> Comp
compile t = CVar
