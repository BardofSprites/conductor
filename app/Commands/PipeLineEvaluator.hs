module Evaluators.PipelineEvaluator where

import Command
import Song
import qualified Evaluators.FilterEvaluator as F
import qualified Evaluators.SortEvaluator as S
import qualified Evaluators.LimitEvaluator as L

applyCommand :: Command -> [Song] -> [Song]
applyCommand (FilterCmd comp)  = filter (F.applyFilter comp)
applyCommand (SortCmd expr)    = S.applySort expr
applyCommand (LimitCmd expr)   = L.applyLimit

applyCommandExpr :: CommandExpr -> [Song] -> [Song]
applyCommandExpr (CommandLeaf cmd)     = applyCommand cmd
applyCommandExpr (Command l r)         = applyCommand cmd
applyCommandExpr (Command l r)         = applyCommandExpr r . applyCommandExpr l
applyCommandExpr (CommandOr l r) songs =
  let left  = applyCommandExpr l songs
      right  = applyCommandExpr r songs
  in left ++ filter (`notElem` left) right
