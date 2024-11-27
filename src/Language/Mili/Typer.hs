-- MIT License, Copyright (c) 2024 Marvin Borner

module Language.Mili.Typer
  ( typeCheck
  ) where

import           Control.Monad.State
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as M
import           Data.Mili                      ( Nat(..)
                                                , Term(..)
                                                )

data Type = Number | Lolli Type Type
  deriving (Show, Eq)
type Context = HashMap Int Type

-- | Convert de Bruijn levels to unique variables
annotate :: Term -> State (Int, [Int]) Term
annotate (Abs _ m) = do
  (s, ls) <- get
  put (s + 1, s : ls)
  an <- annotate m
  modify $ \(s', _) -> (s', ls)
  pure $ Abs s an
annotate (App a b) = (App <$> annotate a) <*> annotate b
annotate (Lvl i  ) = do
  (_, ls) <- get
  pure $ Lvl $ ls !! (length ls - i - 1)
annotate (Num Z    ) = pure $ Num Z
annotate (Num (S m)) = Num . S <$> annotate m
annotate (Rec t1 t2 u v w) =
  Rec
    <$> annotate t1
    <*> annotate t2
    <*> annotate u
    <*> annotate v
    <*> annotate w

typeCheck :: Term -> Either String Term
typeCheck t =
  Left $ show (inferGamma M.empty M.empty $ evalState (annotate t) (0, []))
