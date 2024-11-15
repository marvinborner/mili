-- MIT License, Copyright (c) 2024 Marvin Borner

module Language.Mili.Analyzer
  ( linearity
  ) where

import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as M
import           Data.List                      ( intercalate
                                                , isSuffixOf
                                                )
import           Data.Mili                      ( Nat(..)
                                                , Term(..)
                                                )
import           Debug.Trace

-- | A trace specifies the exact path to any subterm
type Trace = [Breadcrumb]

data Breadcrumb = AbsD  -- | Down into abstraction
                | AppL  -- | Left side of application
                | AppR  -- | Right side of application
                | NumS  -- | Successor of number
                | RecT1 -- | Term 1 of rec
                | RecT2 -- | Term 2 of rec
                | RecU  -- | End of rec
                | RecV  -- | Function 1 of rec
                | RecW  -- | Function 2 of rec
                | Root  -- | Root
  deriving (Eq, Show)

-- | Map abstractions to a hashmap such that de Bruijn levels correspond to traces
-- | of abstractions at that level
traceAbs :: Term -> HashMap Int [Trace]
traceAbs = go 0 [Root]
 where
  go n t (Abs _ m) =
    M.unionWith (++) (go (n + 1) (AbsD : t) m) (M.singleton n [t])
  go n t (App a b) = M.unionWith (++) (go n (AppL : t) a) (go n (AppR : t) b)
  go _ _ (Lvl _             ) = M.empty
  go _ _ (Num Z             ) = M.empty
  go n t (Num (S i)         ) = go n (NumS : t) i
  go n t (Rec (t1, t2) u v w) = foldl1
    (M.unionWith (++))
    [ go n (RecT1 : t) t1
    , go n (RecT2 : t) t2
    , go n (RecU : t)  u
    , go n (RecV : t)  v
    , go n (RecW : t)  w
    ]

-- TODO: Merge these two v^

-- | Map de Bruijn levels to a hashmap such that de Bruijn levels correspond to
-- | traces of de Bruijn levels at that level
traceLvl :: Term -> HashMap Int [Trace]
traceLvl = go [Root]
 where
  go t (Abs _ m           ) = go (AbsD : t) m
  go t (App a b) = M.unionWith (++) (go (AppL : t) a) (go (AppR : t) b)
  go t (Lvl l             ) = M.singleton l [t]
  go _ (Num Z             ) = M.empty
  go t (Num (S i)         ) = go (NumS : t) i
  go t (Rec (t1, t2) u v w) = foldl1
    (M.unionWith (++))
    [ go (RecT1 : t) t1
    , go (RecT2 : t) t2
    , go (RecU : t)  u
    , go (RecV : t)  v
    , go (RecW : t)  w
    ]

-- | Unify two two mapped traces to find levels at which the traces are no suffixes
-- | Level traces not being a suffix of abstraction traces implies nonlinearity
-- TODO: Proof?
unifyTraces :: HashMap Int [Trace] -> HashMap Int [Trace] -> HashMap Int [Bool]
unifyTraces traceA traceB = M.fromList $ map zipMap allKeys
 where
  allKeys = M.keys (traceA `M.union` traceB)
  left key = M.lookupDefault [[]] key traceA
  right key = M.lookupDefault [[]] key traceB
  zipMap key
    | length (left key) == length (right key)
    = (key, zipWith isSuffixOf (left key) (right key))
    | otherwise
    = (key, [False])

linearityError
  :: HashMap Int [Trace] -> HashMap Int [Trace] -> HashMap Int [Bool] -> String
linearityError absTrace lvlTrace = pretty . filter (any not . snd) . M.toList
 where
  pretty = intercalate "\n\n" . map
    (\(depth, _) ->
      "Linearity divergence at depth "
        <> show depth
        <> "\n\t"
        <> show (M.lookupDefault [[]] depth absTrace)
        <> "\nvs\n\t"
        <> show (M.lookupDefault [[]] depth lvlTrace)
    )

linearity :: Term -> Either String Term
linearity t =
  let absTrace = traceAbs t
      lvlTrace = traceLvl t
      unified  = unifyTraces absTrace lvlTrace
      isLinear = all (all id) unified
  in  if isLinear
        then Right t
        else trace (show t <> "\n" <> show absTrace <> "\n" <> show lvlTrace)
                   (Left $ linearityError absTrace lvlTrace unified)
