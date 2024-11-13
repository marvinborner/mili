module Language.Mili.Analyzer
  ( linearity
  ) where

import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as M
import           Data.List                      ( intercalate
                                                , isSuffixOf
                                                )
import           Data.Mili                      ( Term(..) )

-- | A trace specifies the exact path to any subterm
type Trace = [Breadcrumb]

data Breadcrumb = South -- | Down into abstraction
                | West  -- | Left hand side of application
                | East  -- | Right hand side of application
  deriving (Eq, Show)

-- | Map abstractions to a hashmap such that de Bruijn levels correspond to traces
-- | of abstractions at that level
traceAbs :: Term -> HashMap Int [Trace]
traceAbs = go 0 []
 where
  go n t (Abs m) =
    M.unionWith (++) (go (n + 1) (South : t) m) (M.singleton n [t])
  go n t (App a b) = M.unionWith (++) (go n (West : t) a) (go n (East : t) b)
  go _ _ (Lvl _  ) = M.empty

-- | Map de Bruijn levels to a hashmap such that de Bruijn levels correspond to
-- | traces of de Bruijn levels at that level
traceLvl :: Term -> HashMap Int [Trace]
traceLvl = go []
 where
  go t (Abs m  ) = go (South : t) m
  go t (App a b) = M.unionWith (++) (go (West : t) a) (go (East : t) b)
  go t (Lvl l  ) = M.singleton l [t]

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
  pretty = intercalate "\n" . map
    (\(depth, _) ->
      "Linearity divergence in depth "
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
        else Left $ linearityError absTrace lvlTrace unified
