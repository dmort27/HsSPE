module Data.Phonology.RuleParsers ( readRule
                                  , testRule
                                  , readRuleV
                                  , testRuleV
                                  , expandRule
                                  ) where

import Data.Maybe (fromJust, fromMaybe)
import Data.List (nub)

import Data.Phonology.Representations
import Data.Phonology.Rules

import Control.Monad (foldM, mzero)
import Text.ParserCombinators.Parsec
import Generics.Pointless.Combinators ((><))

-- | Expand a rule containing variables into the equivalent set of
-- rules (as a list of strings).
expandRule :: String -> [String]
expandRule rule = foldr (\c acc -> concat [[replace c '+' r, replace c '-' r] | r <- acc]) 
                  [rule] (alphaVars rule)
    where
      alphaVars :: String -> String
      alphaVars = nub . filter (`elem` ['α'..'ω'])
      replace :: Char -> Char -> String -> String
      replace c1 c2 = map (\x -> if x == c1 then c2 else x)

-- | Parses a rule in string notation and returns a 'Rule'. Performs
-- no expansion of alpha variables.
readRule :: RuleState -> String -> Rule
readRule st input = case runParser rule st "rule" input of
                      Right fm -> fm
                      Left e -> error $ show e

testRule :: RuleState -> String -> Bool
testRule st input = case runParser rule st "rule" input of
                      Right fm -> True
                      Left e -> False

-- | Parses a rule in string notation and returns all 'Rules' implied
-- by expansion of variables.
readRuleV :: RuleState -> String -> [Rule]
readRuleV st = map (readRule st) . expandRule

testRuleV :: RuleState -> String -> [Bool]
testRuleV st = map (testRule st) . expandRule

rule :: GenParser Char RuleState Rule
rule = editPart >>= \t -> (ruleSlash >> environment >>= \(a, b) -> return (RGroup [a, t, b]))

editPart :: GenParser Char RuleState Rule
editPart = choice $ map try [ editZero2Literal
                            , editLiteral2Zero
                            , editMatrix2Zero
                            , editFMatrix2FMatrix
                            , editLiteral2Literal
                            , editMacro2Zero
                            ]

editArrow :: GenParser Char RuleState ()
editArrow = spaces >> (string "->" <|> string "→") >> spaces

editFMatrix2FMatrix :: GenParser Char RuleState Rule
editFMatrix2FMatrix = fMatrix >>= 
                      \fm1 -> (editArrow >> fMatrix >>= 
                               \fm2 -> getState >>= 
                                       (\st -> return (RSeg $ rewriteMatrix st fm1 fm2)))

editLiteral2Literal :: GenParser Char RuleState Rule
editLiteral2Literal = ipaSegment >>= ipaDiacritics >>= 
                      \seg1 -> (editArrow >> ipaSegment >>= ipaDiacritics >>= 
                                    \seg2 -> return (RSeg $ rewriteLit seg1 seg2))

editZero2Literal :: GenParser Char RuleState Rule
editZero2Literal = fmap RInsert (editZero >> editArrow >> (many1 (ipaSegment >>= ipaDiacritics)))

editLiteral2Zero :: GenParser Char RuleState Rule
editLiteral2Zero = many1 (ipaSegment >>= ipaDiacritics) >>= 
                   \segs -> (fmap (RDelete . RGroup . map (\ seg -> RSeg (rewriteLit seg seg))) 
                             (editArrow >> editZero >> return segs))
                            
editMatrix2Zero :: GenParser Char RuleState Rule
editMatrix2Zero = fMatrix >>= 
                  \fm -> editArrow >> editZero >> return (RDelete $ RGroup [RSeg $ matchMatrix fm])

editMacro2Zero :: GenParser Char RuleState Rule
editMacro2Zero = getState >>= \(_, ms, _) -> 
                 (oneOf (map (head . fst) ms) >>= 
                  \m -> editArrow >> editZero >> 
                        return (fromJust $ lookup ([m]) ms >>=
                                \fm -> (return (RDelete $ RGroup [RSeg $ matchMatrix fm]))))

editZero :: GenParser Char RuleState Char
editZero = oneOf "0∅"

rewriteAnyLit :: Segment -> Rewrite
rewriteAnyLit newSeg _ = Just newSeg

rewriteLit :: Segment -> Segment -> Rewrite
rewriteLit (s, fm) newSeg (s', fm')
    | s' == s = return newSeg
    | otherwise = mzero 

rewriteMatrix :: RuleState -> FMatrix -> FMatrix -> Rewrite
rewriteMatrix (segs, _, dias) fm1 fm2 (s, fm)
    | fm |?| fm1 = return $ segmentFromFeatures segs dias $ fm |>| fm2
    | otherwise = mzero

matchMatrix :: FMatrix -> Rewrite
matchMatrix fm1 (s, fm)
    | fm |?| fm1 = return (s, fm)
    | otherwise = mzero

ruleSlash :: GenParser Char RuleState ()
ruleSlash = spaces >> string "/" >> spaces

environment :: GenParser Char RuleState (Rule, Rule)
environment = envPart >>= \a -> (envTarget >> envPart >>= \b -> return (a, b))

envTarget :: GenParser Char RuleState ()
envTarget = string "_" >> return ()

envPart :: GenParser Char RuleState Rule
envPart = fmap RGroup (many envToken)

envToken :: GenParser Char RuleState Rule
envToken = try envBoundary 
           <|> try envMacro 
           <|> try envIPASegment 
           <|> try envFMatrix 
           <|> try envManyN
           <|> try envOpt
           <|> try envChoice

envGroup :: GenParser Char RuleState Rule
envGroup = char '(' >> spaces >> (many1 envToken) >>= \toks -> (spaces >> char ')' >> return (RGroup toks))

envOpt :: GenParser Char RuleState Rule
envOpt = envGroup >>= return . ROpt

envManyN :: GenParser Char RuleState Rule
envManyN = envGroup >>= 
           \grp -> (many1 digit >>= 
                    \n -> return (RGroup $ (replicate (read n) grp) ++ [RStar grp]))

envChoice :: GenParser Char RuleState Rule
envChoice = char '{' >> spaces >> sepBy (many1 envToken) (spaces >> char ',' >> spaces) >>= 
            \cs -> (spaces >> char '}' >> return (RChoice (map RGroup cs)))

envBoundary :: GenParser Char RuleState Rule
envBoundary = char '#' >> return RBoundary

envFMatrix :: GenParser Char RuleState Rule
envFMatrix = fMatrix >>= return . RSeg . matchMatrix

envMacro :: GenParser Char RuleState Rule
envMacro = getState >>= \st@(_, ms, _) -> 
           (oneOf (map (head . fst) ms) >>= 
                      \m -> (return (fromJust $ lookup ([m]) ms) >>=
                       \fm -> (return (RSeg $ matchMatrix fm))))

envIPASegment :: GenParser Char RuleState Rule
envIPASegment = ipaSegment >>= ipaDiacritics >>= 
                \seg -> return $ RSeg $ rewriteLit seg seg

