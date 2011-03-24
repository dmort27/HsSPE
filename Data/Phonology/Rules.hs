module Rules where

import Data.Maybe (fromJust, fromMaybe)
import Data.List (nub)

import Features
import Control.Monad (foldM)
import Text.ParserCombinators.Parsec
import Generics.Pointless.Combinators ((><))

type Rewrite = Segment -> Maybe Segment

data Rule = PRSeg Rewrite
               | PRGroup [Rule]
               | PROneOrMore [Rule]
               | PRZeroOrMore [Rule]
               | PRDisjunction [[Rule]]
               | PRDelete [Rule]
               | PRInsert [Rule]
               | PRBoundary

instance Show Rule where
    show (PRSeg rw) = "(PRSeg f)"
    show (PRGroup grp) = "(PRGroup " ++ show grp ++ ")"
    show (PROneOrMore grp) = "(PROneOrMore " ++ show grp ++ ")+"
    show (PRZeroOrMore grp) = "(PRZeroOrMore " ++ show grp ++ ")+"
    show (PRDisjunction grps) = "(PRDisjunction {" ++ show grps ++ "}"
    show (PRBoundary) = "#"

alphaVars :: String -> [Char]
alphaVars = nub . filter (`elem` ['α'..'ω'])

replace :: Char -> Char -> String -> String
replace c1 c2 = map (\x -> if x == c1 then c2 else x)

expandRule :: String -> [String]
expandRule rule = foldr (\c acc -> concat [[replace c '+' r, replace c '-' r] | r <- acc]) [rule] (alphaVars rule)

readRule :: String -> Rule
readRule input = case runParser rule (defSegments, defMacros, defDiacritics) "rule" input of
                      Right fm -> fm
                      Left e -> error $ show e

envToken :: GenParser Char RuleState Rule
envToken = try envBoundary 
           <|> try envMacro 
           <|> try envIPASegment 
           <|> try envFMatrix 
           <|> try envMany1 
           <|> try envMany0 
           <|> try envGroup

envBoundary :: GenParser Char RuleState Rule
envBoundary = char '#' >> return PRBoundary

envFMatrix :: GenParser Char RuleState Rule
envFMatrix = fMatrix >>= \fm -> (getState >>= \st -> (return (PRSeg $ rewriteMatrix st ("", fm) ("", fm))))

envMacro :: GenParser Char RuleState Rule
envMacro = getState >>= \st@(_, ms, _) -> 
           (oneOf (map (head . fst) ms) >>= 
                      \m -> (return (fromJust $ lookup (m:[]) ms) >>=
                       \fm -> (return (PRSeg $ rewriteMatrix st ("", fm) ("", fm)))))

envIPASegment :: GenParser Char RuleState Rule
envIPASegment = ipaSegment >>= ipaDiacritics >>= \seg -> return (PRSeg $ rewriteLit seg seg)

rewriteLit :: Segment -> Segment -> Rewrite
rewriteLit (s, fm) newSeg (s', fm')
    | s' == s = Just newSeg
    | otherwise = Nothing 

rewriteMatrix :: RuleState -> Segment -> Segment -> Rewrite
rewriteMatrix (segs, _, dias) (_, fm) (_, fm') (s, fm'')
    | fm |?| fm' = Just $ segmentFromFeatures segs dias (fm |>| fm'')
    | otherwise = Nothing

envGroup :: GenParser Char RuleState Rule
envGroup = char '(' >> spaces >> (many1 envToken) >>= \toks -> (spaces >> char ')' >> return (PRGroup toks))

envMany1 :: GenParser Char RuleState Rule
envMany1 = envGroup >>= \(PRGroup grp) -> string "_1" >> spaces >> return (PROneOrMore grp)

envMany0 :: GenParser Char RuleState Rule
envMany0 = envGroup >>= \(PRGroup grp) -> string "_0" >> spaces >> return (PRZeroOrMore grp)

envTarget :: GenParser Char RuleState ()
envTarget = string "_" >> return ()

envPart :: GenParser Char RuleState Rule
envPart = many envToken >>= return . PRGroup

environment :: GenParser Char RuleState (Rule, Rule)
environment = envPart >>= \a -> (envTarget >> envPart >>= \b -> return (a, b))

editArrow :: GenParser Char RuleState ()
editArrow = spaces >> string "->" >> spaces

ruleSlash :: GenParser Char RuleState ()
ruleSlash = spaces >> string "/" >> spaces

editPart :: GenParser Char RuleState Rule
editPart = editFMatrix2FMatrix <|> editLiteral2Literal

editFMatrix2FMatrix :: GenParser Char RuleState Rule
editFMatrix2FMatrix = fMatrix >>= 
                      \fm1 -> (editArrow >> fMatrix >>= 
                               \fm2 -> getState >>= 
                                       (\st -> (return (PRSeg $ rewriteMatrix st ("", fm1) ("", fm2)))))

editLiteral2Literal :: GenParser Char RuleState Rule
editLiteral2Literal = ipaSegment >>= ipaDiacritics >>= 
                      \seg1 -> (editArrow >> ipaSegment >>= ipaDiacritics >>= 
                                    \seg2 -> return (PRSeg $ rewriteLit seg1 seg2))

rule :: GenParser Char RuleState Rule
rule = editPart >>= \t -> (ruleSlash >> environment >>= \(a, b) -> return (PRGroup [a, t, b]))

-- Will not work as currently formulated
applyRule :: Rule -> [Segment] -> [Segment]
applyRule _ [] = []
applyRule rule form = applyRule rule $ maybe (tail form) (tail . fst) (applyRule' rule ([], form))

applyRule' :: Rule -> ([Segment], [Segment]) -> Maybe ([Segment], [Segment])
applyRule' (PRSeg _) (xs, []) = Nothing
applyRule' (PRSeg rw) (xs, (y:ys)) = case rw y of
                                       Just y' -> Just (xs++[y], ys)
                                       Nothing -> Nothing
applyRule' (PRGroup []) (xs, ys) = Just (xs, ys)
applyRule' (PRGroup (r:rs)) (xs, ys) = case applyRule' r (xs, ys) of
                                         Just (xs', ys') -> applyRule' (PRGroup rs) (xs', ys')
                                         Nothing -> Nothing