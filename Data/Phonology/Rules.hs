module Rules where

import Data.Maybe (fromJust)

import Features
import Control.Monad (foldM)
import Text.ParserCombinators.Parsec

type Rewrite = Segment -> Segment

data RuleTokens = PRLit String Rewrite 
                | PRFMat FMatrix Rewrite
                | PRMacro FMatrix Rewrite
                | PRGroup [RuleTokens]
                | PROneOrMore [RuleTokens]
                | PRZeroOrMore [RuleTokens]
                | PRDisjunction [[RuleTokens]]
                | Zero [Segment]
                | PRBoundary

instance Show RuleTokens where
    show (PRLit s _) = "(PRLit " ++ s ++ " f)"
    show (PRFMat fm _) = "(PRFMat " ++ show fm ++ " f)"
    show (PRMacro fm _) = "(PRMacro " ++ show fm ++ " f)"
    show (PRGroup grp) = "(PRGroup " ++ show grp ++ ")"
    show (PROneOrMore grp) = "(PROneOrMore " ++ show grp ++ ")+"
    show (PRZeroOrMore grp) = "(PRZeroOrMore " ++ show grp ++ ")+"
    show (PRDisjunction grps) = "(PRDisjunction {" ++ show grps ++ "}"
    show (Zero _) = "Zero"
    show (PRBoundary) = "#"

readRule input = case parse envToken "rule" input of
                      Right fm -> fm
                      Left e -> error $ show e

envToken :: GenParser Char st RuleTokens
envToken = try (char '#' >> return PRBoundary)
           <|> try (oneOf "CVN" >>= \m -> return (PRFMat (fromJust (lookup (m:[]) defMacros)) id))
           <|> try (ipaSegment defSegments >>= ipaDiacritics defDiacritics >>= \(lit, fm) -> return (PRLit lit id))
           <|> try (fMatrix >>= \fm -> return (PRFMat fm id))
           <|> (try envMany1)
           <|> (try envMany0)
           <|> (try envGroup)

envGroup :: GenParser Char st RuleTokens
envGroup = char '(' >> spaces >> (many1 envToken) >>= \toks -> (spaces >> char ')' >> return (PRGroup toks))

envMany1 :: GenParser Char st RuleTokens
envMany1 = envGroup >>= \(PRGroup grp) -> string "_1" >> spaces >> return (PROneOrMore grp)

envMany0 :: GenParser Char st RuleTokens
envMany0 = envGroup >>= \(PRGroup grp) -> string "_0" >> spaces >> return (PRZeroOrMore grp)
