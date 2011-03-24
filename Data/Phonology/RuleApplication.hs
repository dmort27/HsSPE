module RuleApplication where

import Control.Applicative
import Rules
import Features

choice ps = foldl (<|>) Nothing ps

applyRule :: Rule -> [Segment] -> [Segment]
applyRule _ [] = []
applyRule rule form = x:(applyRule rule xs)
    where x:xs = maybe form fst (applyRule' rule ([], form))

applyRule' :: Rule -> ([Segment], [Segment]) -> Maybe ([Segment], [Segment])
applyRule' (RSeg _) (xs, []) = Nothing
applyRule' (RSeg rw) (xs, (y:ys)) = rw y >>= \y' -> return (xs++[y'], ys)
applyRule' (RGroup []) form = Just form
applyRule' (RGroup ((ROpt r):rs)) form = applyRule' (RGroup (r:rs)) form <|> applyRule' (RGroup rs) form
applyRule' (RGroup ((RStar r):rs)) form = applyRule' (RGroup rs) form 
                                          <|> applyRule' (RGroup (r:rs)) form
                                          <|> applyRule' (RGroup (r:(RStar r):rs)) form
applyRule' (RGroup ((RChoice cs):rs)) form = choice $ map (\c -> applyRule' (RGroup (c:rs)) form) cs
applyRule' (RGroup (r:rs)) form = applyRule' r form >>= applyRule' (RGroup rs)