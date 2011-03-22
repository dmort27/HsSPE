module Rules where

import Data.Maybe (fromJust)

import Features
import Control.Monad (foldM)
import Text.ParserCombinators.Parsec
import Generics.Pointless.Combinators ((><))

type Rewrite = Segment -> Segment

data RuleToken = PRLit String Rewrite 
                | PRFMat FMatrix Rewrite
                | PRMacro FMatrix Rewrite
                | PRGroup [RuleToken]
                | PROneOrMore [RuleToken]
                | PRZeroOrMore [RuleToken]
                | PRDisjunction [[RuleToken]]
                | PRDelete [RuleToken]
                | Insert [RuleToken]
                | PRBoundary

instance Show RuleToken where
    show (PRLit s _) = "(PRLit " ++ s ++ " f)"
    show (PRFMat fm _) = "(PRFMat " ++ show fm ++ " f)"
    show (PRMacro fm _) = "(PRMacro " ++ show fm ++ " f)"
    show (PRGroup grp) = "(PRGroup " ++ show grp ++ ")"
    show (PROneOrMore grp) = "(PROneOrMore " ++ show grp ++ ")+"
    show (PRZeroOrMore grp) = "(PRZeroOrMore " ++ show grp ++ ")+"
    show (PRDisjunction grps) = "(PRDisjunction {" ++ show grps ++ "}"
    show (PRBoundary) = "#"

readRule :: String -> RuleToken
readRule input = case runParser envToken (defSegments, defMacros, defDiacritics) "rule" input of
                      Right fm -> fm
                      Left e -> error $ show e

envToken :: GenParser Char RuleState RuleToken
envToken = try envBoundary <|> try envMacro <|> try envIPASegment <|> try envFMatrix <|> try envMany1 <|> try envMany0 <|> try envGroup

envBoundary :: GenParser Char RuleState RuleToken
envBoundary = char '#' >> return PRBoundary

envFMatrix :: GenParser Char RuleState RuleToken
envFMatrix = fMatrix >>= \fm -> return (PRFMat fm id)

envMacro :: GenParser Char RuleState RuleToken
envMacro = getState >>= \(_, ms, _) -> 
           (oneOf (map (head . fst) ms) >>= \m -> return (PRFMat (fromJust (lookup (m:[]) ms)) id))

envIPASegment :: GenParser Char RuleState RuleToken
envIPASegment = ipaSegment >>= ipaDiacritics >>= \(lit, fm) -> return (PRLit lit id)

envGroup :: GenParser Char RuleState RuleToken
envGroup = char '(' >> spaces >> (many1 envToken) >>= \toks -> (spaces >> char ')' >> return (PRGroup toks))

envMany1 :: GenParser Char RuleState RuleToken
envMany1 = envGroup >>= \(PRGroup grp) -> string "_1" >> spaces >> return (PROneOrMore grp)

envMany0 :: GenParser Char RuleState RuleToken
envMany0 = envGroup >>= \(PRGroup grp) -> string "_0" >> spaces >> return (PRZeroOrMore grp)

editArrow :: GenParser Char RuleState ()
editArrow = spaces >> string "->" >> spaces

ruleSlash :: GenParser Char RuleState ()
ruleSlash = spaces >> string "/" >> spaces

editPart :: GenParser Char RuleState RuleToken
editPart = editArrow >> return PRBoundary

editSegByFM :: [Segment] -> [(String, FMatrix -> FMatrix)] -> FMatrix -> Segment -> Segment
editSegByFM segs dias fm' (_, fm) = segmentFromFeatures segs dias (fm |>| fm')

editFMatrix2FMatrix :: GenParser Char RuleState RuleToken
editFMatrix2FMatrix = fMatrix >>= \fm1 -> 
                      (editArrow >> fMatrix >>= \fm2 -> 
                           getState >>= (\(segs, _, dias) -> 
                                         (return (PRFMat fm1 (\(_, fm) -> segmentFromFeatures segs dias (fm |>| fm2))))))                            