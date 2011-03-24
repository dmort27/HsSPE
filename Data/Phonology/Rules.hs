module Rules where

import Data.Maybe (fromJust, fromMaybe)
import Data.List (nub)

import Features
import Control.Monad (foldM)
import Text.ParserCombinators.Parsec
import Generics.Pointless.Combinators ((><))

type Rewrite = Segment -> Maybe Segment

data Rule = RSeg Rewrite
               | RGroup [Rule]
               | RStar Rule
               | ROpt Rule
               | RDisjunction [Rule]
               | RDelete Rule
               | RInsert Rule
               | RBoundary

instance Show Rule where
    show (RSeg rw) = "(RSeg f)"
    show (RGroup grp) = "(RGroup " ++ show grp ++ ")"
    show (RStar grp) = "(RStar " ++ show grp ++ ")"
    show (ROpt grp) = "(ROpt " ++ show grp ++ ")"
    show (RDisjunction grps) = "(RDisjunction {" ++ show grps ++ "}"
    show (RBoundary) = "#"

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
           <|> try envManyN
           <|> try envOpt

envBoundary :: GenParser Char RuleState Rule
envBoundary = char '#' >> return RBoundary

envFMatrix :: GenParser Char RuleState Rule
envFMatrix = fMatrix >>= \fm -> (getState >>= \st -> (return (RSeg $ rewriteMatrix st ("", fm) ("", fm))))

envMacro :: GenParser Char RuleState Rule
envMacro = getState >>= \st@(_, ms, _) -> 
           (oneOf (map (head . fst) ms) >>= 
                      \m -> (return (fromJust $ lookup (m:[]) ms) >>=
                       \fm -> (return (RSeg $ rewriteMatrix st ("", fm) ("", fm)))))

envIPASegment :: GenParser Char RuleState Rule
envIPASegment = ipaSegment >>= ipaDiacritics >>= \seg -> return (RSeg $ rewriteLit seg seg)

rewriteLit :: Segment -> Segment -> Rewrite
rewriteLit (s, fm) newSeg (s', fm')
    | s' == s = Just newSeg
    | otherwise = Nothing 

rewriteMatrix :: RuleState -> Segment -> Segment -> Rewrite
rewriteMatrix (segs, _, dias) (_, fm) (_, fm') (s, fm'')
    | fm |?| fm' = Just $ segmentFromFeatures segs dias (fm |>| fm'')
    | otherwise = Nothing

envGroup :: GenParser Char RuleState Rule
envGroup = char '(' >> spaces >> (many1 envToken) >>= \toks -> (spaces >> char ')' >> return (RGroup toks))

envOpt :: GenParser Char RuleState Rule
envOpt = envGroup >>= return . ROpt

envManyN :: GenParser Char RuleState Rule
envManyN = envGroup >>= \grp -> ((many1 digit) >>= \n -> return (RGroup $ (replicate (read n) grp) ++ [RStar grp]))

envTarget :: GenParser Char RuleState ()
envTarget = string "_" >> return ()

envPart :: GenParser Char RuleState Rule
envPart = many envToken >>= return . RGroup

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
                                       (\st -> (return (RSeg $ rewriteMatrix st ("", fm1) ("", fm2)))))

editLiteral2Literal :: GenParser Char RuleState Rule
editLiteral2Literal = ipaSegment >>= ipaDiacritics >>= 
                      \seg1 -> (editArrow >> ipaSegment >>= ipaDiacritics >>= 
                                    \seg2 -> return (RSeg $ rewriteLit seg1 seg2))

rule :: GenParser Char RuleState Rule
rule = editPart >>= \t -> (ruleSlash >> environment >>= \(a, b) -> return (RGroup [a, t, b]))
