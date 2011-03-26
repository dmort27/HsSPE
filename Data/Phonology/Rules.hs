module Data.Phonology.Rules ( Rewrite
                            , Rule(..)
                            , applyRule
                            , applyRuleV
                            , derivation
                            , prettyDerivation
                            , maybeDerivation
                            , derivationV
                            , prettyDerivationV
                            , maybeDerivationV
                            , toString
                            ) where

import Control.Applicative
import Data.Maybe (fromMaybe)
import Data.Phonology.Features

data Rule = RSeg Rewrite
               | RGroup [Rule]
               | RStar Rule
               | ROpt Rule
               | RChoice [Rule]
               | RDelete Rule
               | RInsert [Segment]
               | RBoundary

instance Show Rule where
    show (RSeg rw) = "(RSeg " ++  show (fromMaybe sampleChar $ rw sampleChar)  ++ ")"
        where
          sampleChar = head $ readIPA defState "i"
    show (RGroup grp) = "(RGroup " ++ show grp ++ ")"
    show (RStar grp) = "(RStar " ++ show grp ++ ")"
    show (ROpt grp) = "(ROpt " ++ show grp ++ ")"
    show (RChoice grps) = "(RChoice {" ++ show grps ++ "}"
    show (RBoundary) = "#"

type Rewrite = Segment -> Maybe Segment

-- |The @derivation@ function yields the product of applying, in
-- succession, a list of phonological rules. Results are returned as a
-- list of lists of tuples of (String, FMatrix).
derivation :: [Segment] -> [Rule] -> [[Segment]]
derivation form = scanl (\acc r -> applyRule r acc) form

-- |Like @derivation@, but returns a list of 'String's. If the form is
-- unchanged by the application of a rule, \"---\" is returned for that
-- increment.
prettyDerivation :: [Segment] -> [Rule] -> [String]
prettyDerivation form rs = ((head fms):) $ map (\(a,b) -> if a==b then "---" else b) $ zip fms (tail fms)
    where fms = map toString $ derivation form rs

-- |Like @prettyDerivation@, but returns a list of 'Maybe' 'String's. If
-- the form is unchanged by the application of a rule, 'Nothing' is
-- returned for that increment.
maybeDerivation :: [Segment] -> [Rule] -> [Maybe String]
maybeDerivation form rs = ((Just (head fms)):) $ map (\(a,b) -> if a==b then Nothing else Just b) $ zip fms (tail fms)
    where fms = map toString $ derivation form rs

-- |The 'derivation' function yields the product of applying, in
-- succession, a list of phonological rules. Rules are provided in
-- string notation and variable expansion is performed.  Results are
-- returned as a list of lists of 'Segment's.
derivationV :: (String -> [Rule]) -> [Segment] -> [String] -> [[Segment]]
derivationV reader form = scanl (\acc r -> applyRuleV reader r acc) form

-- |Like @derivationV@, but returns a list of 'String's. If the form is
-- unchanged by the application of a rule, \"---\" is returned for that
-- increment.
prettyDerivationV :: (String -> [Rule]) -> [Segment] -> [String] -> [String]
prettyDerivationV reader form rs = 
    ((head fms):) 
    $ map (\(a,b) -> if a==b then "---" else b) $ zip fms (tail fms)
    where fms = map toString $ derivationV reader form rs

-- |Like @prettyDerivationV@, but returns a list of 'Maybe' 'String's. If
-- the form is unchanged by the application of a rule, 'Nothing' is
-- returned for that increment.
maybeDerivationV :: (String -> [Rule]) -> [Segment] -> [String] -> [Maybe String]
maybeDerivationV reader form rs = 
    ((Just (head fms)):) 
    $ map (\(a,b) -> if a==b then Nothing else Just b) $ zip fms (tail fms)
    where fms = map toString $ derivationV reader form rs

-- |Returns the result of applying a rule to a form (a list of 'Segment's).
applyRule :: Rule -> [Segment] -> [Segment]
applyRule _ [] = []
applyRule rule form = x:(applyRule rule xs)
    where x:xs = maybe form (\(a,b) -> a++b) (applyRule' rule ([], form))

applyRule' :: Rule -> ([Segment], [Segment]) -> Maybe ([Segment], [Segment])
applyRule' (RSeg _) (xs, []) = Nothing
applyRule' (RSeg rw) (xs, (y:ys)) = rw y >>= \y' -> return (xs++[y'], ys)
applyRule' RBoundary (xs, []) = Nothing
applyRule' RBoundary (xs, (y:ys))
    | (fst y) == "#" = return (xs++[y], ys)
    | otherwise = Nothing
applyRule' (RGroup []) form = Just form
applyRule' (RGroup ((ROpt r):rs)) form = applyRule' (RGroup (r:rs)) form 
                                         <|> applyRule' (RGroup rs) form
applyRule' (RGroup ((RStar r):rs)) form = applyRule' (RGroup rs) form 
                                          <|> applyRule' (RGroup (r:rs)) form
                                          <|> applyRule' (RGroup (r:(RStar r):rs)) form
applyRule' (RGroup ((RChoice cs):rs)) form = choice $ map (\c -> applyRule' (RGroup (c:rs)) form) cs
applyRule' (RGroup (r:rs)) form = applyRule' r form >>= applyRule' (RGroup rs)
applyRule' (RInsert segs) (xs, []) = Nothing
applyRule' (RInsert segs) (xs, ys) = return (xs++segs, ys)
applyRule' (RDelete _) (_, []) = Nothing
applyRule' (RDelete (RGroup [])) form = return form
applyRule' (RDelete (RGroup (rw:rws))) (xs, (y:ys)) = applyRule' rw (xs, y:ys) >>= 
                                                    \_ -> applyRule' (RDelete (RGroup (rws))) (xs, ys)

-- | Takes a rule parser that performs variable expansion and a rule
-- in string notation and applies the rule (in all of its expansions)
-- to a list of 'Segment's, returning the result.
applyRuleV :: (String -> [Rule]) -> String -> [Segment] -> [Segment]
applyRuleV reader rule form = foldr (\r acc -> applyRule r acc) form $ reader rule

choice ps = foldl (<|>) Nothing ps

-- |Utility function for converting lists of 'Segment's into 'String's.
toString :: [Segment] -> String
toString = concatMap fst