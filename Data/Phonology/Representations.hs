module Data.Phonology.Representations ( FValue(..)
                                      , FMatrix(..)
                                      , Segment
                                      , RuleState
                                      , (|>|)
                                      , (|?|)
                                      , flipFValue
                                      , fMatrix
                                      , readIPA
                                      , readableIPA
                                      , readFMatrix
                                      , ipaSegment
                                      , ipaDiacritics
                                      , segmentFromFeatures
                                      , includeFts
                                      , toFMatrixPairs
                                      , diacriticFunctions
                                      , defFeatures
                                      , defSegments
                                      , defMacros
                                      , defDiacritics
                                      , defState
                                      , mkRuleState
                                      ) where

import Data.Maybe (fromJust)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (intercalate, sortBy)
import Data.Ord (comparing)
import Debug.Trace (trace)
import Control.Monad (foldM)
import Text.ParserCombinators.Parsec

import Generics.Pointless.Combinators ((><))

data FValue = Plus | Minus | Unspec | Var String
              deriving (Eq, Ord)
instance Show FValue where
    show Plus = "+"
    show Minus = "-"
    show Unspec = "0"
    show (Var s) = s
instance Read FValue where
    readsPrec _ value =
        tryParse ([('+', Plus), ('-', Minus), ('0',Unspec)] ++ [(s, Var ([s])) | s <- ['α'..'ω']])
            where
              tryParse [] = []
              tryParse ((attempt, result):xs) =
                  if head value == attempt
                  then [(result, tail value)]
                  else tryParse xs

data FMatrix = FMatrix (Map String FValue)
             deriving (Eq, Ord)
instance Show FMatrix where
    show (FMatrix fm) = (\x -> "[" ++ x ++ "]") $ intercalate "," $ map (\(k,v) -> show v ++ k) $ Map.toAscList fm

fmapFM f (FMatrix fm) = FMatrix $ f fm

fromFM (FMatrix fm) = fm

type Segment = (String, FMatrix)
type RuleState = ([Segment], [Segment], [(String, FMatrix -> FMatrix)])

--infixr 5 |>|
--infixr 5 |?|

-- | Updates an FMatrix. @a |>| b@ returns an 'FMatrix' identical to
-- @a@ except for feature specifications where @a@ disagrees with @b@,
-- which are returned as in @b@.
(|>|) :: FMatrix -> FMatrix -> FMatrix
FMatrix fm2 |>| FMatrix fm1 = FMatrix $ Map.union fm1 fm2

-- | Checks for matches between feature matrices. @a |?| b@ returns
-- true iff, for all feature specifications in @b@, matching feature
-- specifications are found in @a@.
(|?|) :: FMatrix -> FMatrix -> Bool
FMatrix comparandum |?| FMatrix comparator = 
                        Map.foldrWithKey 
                               (\k v acc -> 
                                    Map.findWithDefault (flipFValue v) k comparandum `elem` [Unspec, v] && acc)
                           True comparator

flipFValue :: FValue -> FValue
flipFValue Plus = Minus
flipFValue Minus = Plus
flipFValue _ = Unspec

readFMatrix :: String -> FMatrix
readFMatrix input = case parse fMatrix "feature matrix" input of
                      Right fm -> fm
                      Left e -> error $ show e

fMatrix :: GenParser Char st FMatrix
fMatrix = char '[' >> spaces >> sepBy feature (char ',') >>= 
          \fm -> spaces >> char ']' >> return (FMatrix $ Map.fromList fm)

fName :: GenParser Char st String
fName = many (oneOf ['a'..'z'])

fValue :: GenParser Char st FValue
fValue = oneOf ("+-0" ++ ['α'..'ω']) >>= return . read . (:[])

feature :: GenParser Char st (String, FValue)
feature = spaces >> fValue >>= \v -> (fName >>= \k -> spaces >> return (k, v))

-- | Give a 'RuleState' and a transcription as a 'String', returns the
-- representation as a list of 'Segment's.
readIPA :: RuleState -> String -> [Segment]
readIPA (segs, macs, dias) input = case runParser ipaString (segs, macs, dias) "feature matrix" input of
                  Right fm -> fm
                  Left e -> error $ show e

readableIPA :: RuleState -> String -> Bool
readableIPA (segs, macs, dias) input = case runParser ipaString (segs, macs, dias) "feature matrix" input of
                  Right fm -> True
                  Left e -> False

ipaString :: GenParser Char RuleState [Segment]
ipaString = many (ipaSegment >>= ipaDiacritics)

ipaSegment :: GenParser Char RuleState (String, FMatrix)
ipaSegment = getState >>= \(segs, _, _) -> (char '#' >> return ("#", FMatrix Map.empty)) <|>
             choice (map (\(l,fs) -> try $ string l >> return (l, fs)) segs)

ipaDiacritics :: (String, FMatrix) -> GenParser Char RuleState (String, FMatrix)
ipaDiacritics (seg, fm) = getState >>= \(_, _, dias) -> 
                          (many (choice (map (\(d, f) -> try $ string d >> return (d, f)) dias)) >>=
                                return . foldr (\(d, f) (seg', fm') -> (seg' ++ d, f fm')) (seg, fm))
                                    
toFMatrixPairs :: [(String, String)] -> [(String, FMatrix)]
toFMatrixPairs segs = reverse $ sortBy (comparing (length . fst)) [(seg, readFMatrix fs) | (seg, fs) <- segs]

diacriticFunctions :: [(String, String)] -> [(String, FMatrix -> FMatrix)]
diacriticFunctions dias = [(dia, \fm -> fm |>| readFMatrix fts) | (dia, fts) <- dias]

includeFts :: [String] -> (String, FMatrix) -> (String, FMatrix)
includeFts fts = id >< fmapFM (Map.filterWithKey (\k _ -> k `elem` fts))

bestSegmentMatch :: [Segment] -> FMatrix -> Segment
bestSegmentMatch segs fm = snd $ head $ sortBy (comparing fst) [(fmEditDistance fm fm', (s', fm')) | (s', fm') <- segs]

segmentFromFeatures :: [Segment] -> [(String, FMatrix -> FMatrix)] -> FMatrix -> Segment
segmentFromFeatures segs dias fm = segmentFromFeatures' segs' bestSeg bestDist
    where
      bestSeg = bestSegmentMatch segs fm
      segs' = applyDiacritics defDiacritics bestSeg
      bestDist = fmEditDistance fm (snd bestSeg)

      segmentFromFeatures' :: [Segment] -> Segment -> Int -> Segment
      segmentFromFeatures' segs'' seg dist
          | dist == 0 = seg
          | dist <= dist' = seg
          | otherwise = segmentFromFeatures' segs''' bestSeg' dist'
          where 
            bestSeg' = bestSegmentMatch segs'' fm
            dist' = fmEditDistance fm (snd bestSeg')
            segs''' = applyDiacritics defDiacritics bestSeg'

fmEditDistance :: FMatrix -> FMatrix -> Int
fmEditDistance (FMatrix fm1) (FMatrix fm2) = 
    Map.size $ Map.union (difference fm1 fm2) (difference fm2 fm1)
        where
          difference = Map.differenceWithKey (\_ a' b' -> if a' /= b' then Just a' else Nothing)
                                             
applyDiacritics :: [(String, FMatrix -> FMatrix)] -> Segment -> [Segment]
applyDiacritics dias (s, fm) = map (\(dia, f) -> (s++dia, f fm)) dias

-- | Default feature names.
defFeatures :: [String]
defFeatures = ["syl","son","cons","cont","delrel","lat","nas","voi","cg","sg","ant","cor","distr","hi","lo","back","round","tense"]

defSegments = map (includeFts defFeatures) $ toFMatrixPairs segments
defMacros = map (includeFts defFeatures) $ toFMatrixPairs macros
defDiacritics = diacriticFunctions diacritics

-- | Default 'RuleState' to be passed to parsers.
defState :: RuleState
defState = (defSegments, defMacros, defDiacritics)

-- | Given segment, macro, and diacritic definitions as lists of
-- ('String', 'String')s, and a list of active features as 'String's,
-- returns a 'RuleState' that can be passed to rule and transcription
-- parsers.
mkRuleState :: [(String, String)] -> [(String, String)] -> [(String, String)] -> [String] -> RuleState
mkRuleState segs macs dias fts = ( map (includeFts fts) $ toFMatrixPairs segs
                                 , map (includeFts fts) $ toFMatrixPairs macs
                                 , diacriticFunctions diacritics
                                 )

getDefMacro :: Char -> FMatrix
getDefMacro m = fromJust $ lookup (m:[]) defMacros

diacritics = [ ("ʷ", "[+back,+round]")
             , ("ʲ", "[+hi]")
             , ("ˤ", "[+low,+back]")
             , ("ˠ", "[+hi,+back]")
             , ("ʰ", "[-voi,-cg,+sg]")
             , ("ʼ", "[-voi,+cg,-sg]")
             , ("̤", "[+voi,-cg,+sg]")
             , ("̰", "[+voi,+cg,-sg]")
             , ("̃", "[+nas]")
             , ("̥", "[-voi]")
             , ("̩", "[+syl]")
             , ("ː", "[+long]")
             ]

macros = [ ("C", "[-syl]")
         , ("V", "[+syl]")
         , ("N", "[-syl,+nas]")
         , ("X", "[]")
         ]

segments = [ ("p","[-syl,-son,+cons,-cont,-delrel,-lat,-nas,-voi,-cg,-sg,+ant,+lab,-cor,0distr,-hi,-lo,-back,-round,0tense,-long]")
           , ("p͡f","[-syl,-son,+cons,-cont,+delrel,-lat,-nas,-voi,-cg,-sg,+ant,+lab,-cor,0distr,-hi,-lo,-back,-round,0tense,-long]")
           , ("t","[-syl,-son,+cons,-cont,-delrel,-lat,-nas,-voi,-cg,-sg,+ant,-lab,+cor,-distr,-hi,-lo,-back,-round,0tense,-long]")
           , ("t̪","[-syl,-son,+cons,-cont,-delrel,-lat,-nas,-voi,-cg,-sg,+ant,-lab,+cor,+distr,-hi,-lo,-back,-round,0tense,-long]")
           , ("t͡s","[-syl,-son,+cons,-cont,+delrel,-lat,-nas,-voi,-cg,-sg,+ant,-lab,+cor,-distr,-hi,-lo,-back,-round,0tense,-long]")
           , ("t͡ʃ","[-syl,-son,+cons,-cont,+delrel,-lat,-nas,-voi,-cg,-sg,-ant,-lab,+cor,+distr,-hi,-lo,-back,-round,0tense,-long]")
           , ("ʈ","[-syl,-son,+cons,-cont,-delrel,-lat,-nas,-voi,-cg,-sg,-ant,-lab,+cor,-distr,-hi,-lo,-back,-round,0tense,-long]")
           , ("c","[-syl,-son,+cons,-cont,-delrel,-lat,-nas,-voi,-cg,-sg,-ant,-lab,-cor,0distr,+hi,-lo,-back,-round,0tense,-long]")
           , ("k","[-syl,-son,+cons,-cont,-delrel,-lat,-nas,-voi,-cg,-sg,-ant,-lab,-cor,0distr,+hi,-lo,+back,-round,0tense,-long]")
           , ("q","[-syl,-son,+cons,-cont,-delrel,-lat,-nas,-voi,-cg,-sg,-ant,+lab,-cor,0distr,-hi,-lo,+back,-round,0tense,-long]")
           , ("b","[-syl,-son,+cons,-cont,-delrel,-lat,-nas,+voi,-cg,-sg,+ant,+lab,-cor,0distr,-hi,-lo,-back,-round,0tense,-long]")
           , ("bv","[-syl,-son,+cons,-cont,+delrel,-lat,-nas,+voi,-cg,-sg,+ant,+lab,-cor,0distr,-hi,-lo,-back,-round,0tense,-long]")
           , ("d","[-syl,-son,+cons,-cont,-delrel,-lat,-nas,+voi,-cg,-sg,+ant,-lab,+cor,-distr,-hi,-lo,-back,-round,0tense,-long]")
           , ("d̪","[-syl,-son,+cons,-cont,-delrel,-lat,-nas,+voi,-cg,-sg,+ant,-lab,+cor,+distr,-hi,-lo,-back,-round,0tense,-long]")
           , ("d͡z","[-syl,-son,+cons,-cont,+delrel,-lat,-nas,+voi,-cg,-sg,+ant,-lab,+cor,-distr,-hi,-lo,-back,-round,0tense,-long]")
           , ("d͡ʒ","[-syl,-son,+cons,-cont,+delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,+cor,+distr,-hi,-lo,-back,-round,0tense,-long]")
           , ("ɖ","[-syl,-son,+cons,-cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,+cor,-distr,-hi,-lo,-back,-round,0tense,-long]")
           , ("ɟ","[-syl,-son,+cons,-cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,0distr,+hi,-lo,-back,-round,0tense,-long]")
           , ("g","[-syl,-son,+cons,-cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,0distr,+hi,-lo,+back,-round,0tense,-long]")
           , ("ɢ","[-syl,-son,+cons,-cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,0distr,-hi,-lo,+back,-round,0tense,-long]")
           , ("ɓ","[-syl,-son,+cons,-cont,-delrel,-lat,-nas,+voi,+cg,-sg,+ant,+lab,-cor,0distr,-hi,-lo,-back,-round,0tense,-long]")
           , ("ɗ","[-syl,-son,+cons,-cont,-delrel,-lat,-nas,+voi,+cg,-sg,+ant,-lab,+cor,-distr,-hi,-lo,-back,-round,0tense,-long]")
           , ("ʄ","[-syl,-son,+cons,-cont,-delrel,-lat,-nas,+voi,+cg,-sg,-ant,-lab,-cor,0distr,+hi,-lo,-back,-round,0tense,-long]")
           , ("ɠ","[-syl,-son,+cons,-cont,-delrel,-lat,-nas,+voi,+cg,-sg,-ant,-lab,-cor,0distr,+hi,-lo,+back,-round,0tense,-long]")
           , ("ʛ","[-syl,-son,+cons,-cont,-delrel,-lat,-nas,+voi,+cg,-sg,-ant,-lab,-cor,0distr,-hi,-lo,+back,-round,0tense,-long]")
           , ("ɸ","[-syl,-son,+cons,+cont,-delrel,-lat,-nas,-voi,-cg,-sg,+ant,+lab,-cor,0distr,-hi,-lo,-back,-round,0tense,-long]")
           , ("β","[-syl,-son,+cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,+ant,+lab,-cor,0distr,-hi,-lo,-back,-round,0tense,-long]")
           , ("f","[-syl,-son,+cons,+cont,-delrel,-lat,-nas,-voi,-cg,-sg,+ant,+lab,-cor,0distr,-hi,-lo,-back,-round,0tense,-long]")
           , ("v","[-syl,-son,+cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,+ant,+lab,-cor,0distr,-hi,-lo,-back,-round,0tense,-long]")
           , ("θ","[-syl,-son,+cons,+cont,-delrel,-lat,-nas,-voi,-cg,-sg,+ant,-lab,+cor,+distr,-hi,-lo,-back,-round,0tense,-long]")
           , ("ð","[-syl,-son,+cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,+ant,-lab,+cor,+distr,-hi,-lo,-back,-round,0tense,-long]")
           , ("s","[-syl,-son,+cons,+cont,-delrel,-lat,-nas,-voi,-cg,-sg,+ant,-lab,+cor,-distr,-hi,-lo,-back,-round,0tense,-long]")
           , ("s̪","[-syl,-son,+cons,+cont,-delrel,-lat,-nas,-voi,-cg,-sg,+ant,-lab,+cor,+distr,-hi,-lo,-back,-round,0tense,-long]")
           , ("z","[-syl,-son,+cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,+ant,-lab,+cor,-distr,-hi,-lo,-back,-round,0tense,-long]")
           , ("ʃ","[-syl,-son,+cons,+cont,-delrel,-lat,-nas,-voi,-cg,-sg,-ant,-lab,+cor,+distr,-hi,-lo,-back,-round,0tense,-long]")
           , ("ʒ","[-syl,-son,+cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,+cor,+distr,-hi,-lo,-back,-round,0tense,-long]")
           , ("ʂ","[-syl,-son,+cons,+cont,-delrel,-lat,-nas,-voi,-cg,-sg,-ant,-lab,+cor,-distr,-hi,-lo,-back,-round,0tense,-long]")
           , ("ʐ","[-syl,-son,+cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,+cor,-distr,-hi,-lo,-back,-round,0tense,-long]")
           , ("ç","[-syl,-son,+cons,+cont,-delrel,-lat,-nas,-voi,-cg,-sg,-ant,-lab,-cor,0distr,+hi,-lo,-back,-round,0tense,-long]")
           , ("ʝ","[-syl,-son,+cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,0distr,+hi,-lo,-back,-round,0tense,-long]")
           , ("x","[-syl,-son,+cons,+cont,-delrel,-lat,-nas,-voi,-cg,-sg,-ant,-lab,-cor,0distr,+hi,-lo,+back,-round,0tense,-long]")
           , ("ɣ","[-syl,-son,+cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,0distr,+hi,-lo,+back,-round,0tense,-long]")
           , ("χ","[-syl,-son,+cons,+cont,-delrel,-lat,-nas,-voi,-cg,-sg,-ant,-lab,-cor,0distr,-hi,-lo,+back,-round,0tense,-long]")
           , ("ʁ","[-syl,-son,+cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,0distr,-hi,-lo,+back,-round,0tense,-long]")
           , ("ħ","[-syl,-son,+cons,+cont,-delrel,-lat,-nas,-voi,-cg,-sg,-ant,-lab,-cor,0distr,-hi,+lo,+back,-round,0tense,-long]")
           , ("ʕ","[-syl,-son,+cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,0distr,-hi,+lo,+back,-round,0tense,-long]")
           , ("h","[-syl,-son,+cons,+cont,-delrel,-lat,-nas,-voi,-cg,+sg,-ant,-lab,-cor,0distr,-hi,-lo,-back,-round,0tense,-long]")
           , ("ɦ","[-syl,-son,+cons,+cont,-delrel,-lat,+nas,-voi,-cg,+sg,-ant,-lab,-cor,0distr,-hi,-lo,-back,-round,0tense,-long]")
           , ("m","[-syl,+son,+cons,-cont,-delrel,-lat,+nas,+voi,-cg,-sg,+ant,+lab,-cor,0distr,-hi,-lo,-back,-round,0tense,-long]")
           , ("n","[-syl,+son,+cons,-cont,-delrel,-lat,+nas,+voi,-cg,-sg,+ant,-lab,+cor,-distr,-hi,-lo,-back,-round,0tense,-long]")
           , ("n̪","[-syl,+son,+cons,-cont,-delrel,-lat,+nas,+voi,-cg,-sg,+ant,-lab,+cor,+distr,-hi,-lo,-back,-round,0tense,-long]")
           , ("ɲ","[-syl,+son,+cons,-cont,-delrel,-lat,+nas,+voi,-cg,-sg,+ant,-lab,-cor,0distr,+hi,-lo,-back,-round,0tense,-long]")
           , ("ɳ","[-syl,+son,+cons,-cont,-delrel,-lat,+nas,+voi,-cg,-sg,-ant,-lab,+cor,0distr,-hi,-lo,-back,-round,0tense,-long]")
           , ("ŋ","[-syl,+son,+cons,-cont,-delrel,-lat,+nas,+voi,-cg,-sg,-ant,-lab,-cor,0distr,+hi,-lo,+back,-round,0tense,-long]")
           , ("ɴ","[-syl,+son,+cons,-cont,-delrel,-lat,+nas,+voi,-cg,-sg,-ant,-lab,-cor,0distr,-hi,-lo,+back,-round,0tense,-long]")
           , ("ɹ","[-syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,+cor,-distr,+hi,-lo,+back,+round,0tense,-long]")
           , ("r","[-syl,+son,+cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,+cor,-distr,+hi,-lo,+back,+round,0tense,-long]")
           , ("ɻ","[-syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,+cor,-distr,-hi,-lo,-back,-round,0tense,-long]")
           , ("l","[-syl,+son,+cons,+cont,-delrel,+lat,-nas,+voi,-cg,-sg,+ant,-lab,+cor,-distr,-hi,-lo,-back,-round,0tense,-long]")
           , ("l̪","[-syl,+son,+cons,+cont,-delrel,+lat,-nas,+voi,-cg,-sg,+ant,-lab,+cor,+distr,-hi,-lo,-back,-round,0tense,-long]")
           , ("j","[-syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,0distr,+hi,-lo,-back,-round,0tense,-long]")
           , ("w","[-syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,+lab,-cor,0distr,+hi,-lo,+back,+round,0tense,-long]")
           , ("t͡l","[-syl,+son,+cons,+cont,+delrel,+lat,-nas,-voi,-cg,-sg,+ant,-lab,+cor,-distr,-hi,-lo,-back,-round,0tense,-long]")
           , ("d͡l","[-syl,+son,+cons,+cont,+delrel,+lat,-nas,+voi,-cg,-sg,+ant,-lab,+cor,-distr,-hi,-lo,-back,-round,0tense,-long]")
           , ("ʔ","[-syl,+son,-cons,-cont,-delrel,-lat,-nas,-voi,+cg,-sg,-ant,-lab,-cor,0distr,-hi,-lo,-back,-round,0tense,-long]")
           , ("i","[+syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,-distr,+hi,-lo,-back,-round,+tense,-long]")
           , ("y","[+syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,+lab,-cor,-distr,+hi,-lo,-back,+round,+tense,-long]")
           , ("ɨ","[+syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,-distr,+hi,-lo,+back,-round,+tense,-long]")
           , ("u","[+syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,+lab,-cor,-distr,+hi,-lo,+back,+round,+tense,-long]")
           , ("e","[+syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,-distr,-hi,-lo,-back,-round,+tense,-long]")
           , ("ø","[+syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,-distr,-hi,-lo,-back,+round,+tense,-long]")
           , ("ʌ","[+syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,-distr,-hi,-lo,+back,-round,+tense,-long]")
           , ("o","[+syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,-distr,-hi,-lo,+back,+round,+tense,-long]")
           , ("æ","[+syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,-distr,-hi,+lo,-back,-round,+tense,-long]")
           , ("ɶ","[+syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,-distr,-hi,+lo,-back,+round,+tense,-long]")
           , ("a","[+syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,-distr,-hi,+lo,+back,-round,+tense,-long]")
           , ("ɑ","[+syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,-distr,-hi,+lo,+back,-round,+tense,-long]")
           , ("ɒ","[+syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,-distr,-hi,+lo,+back,+round,+tense,-long]")
           , ("ɪ","[+syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,-distr,+hi,-lo,-back,-round,-tense,-long]")
           , ("ʏ","[+syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,-distr,+hi,-lo,-back,+round,-tense,-long]")
           , ("ɯ","[+syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,-distr,+hi,-lo,+back,-round,-tense,-long]")
           , ("ʊ","[+syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,-distr,+hi,-lo,+back,+round,-tense,-long]")
           , ("ɛ","[+syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,-distr,-hi,-lo,-back,-round,-tense,-long]")
           , ("œ","[+syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,-distr,-hi,-lo,-back,+round,-tense,-long]")
           , ("ə","[+syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,-distr,-hi,-lo,+back,-round,-tense,-long]")
           , ("ɔ","[+syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,-distr,-hi,-lo,+back,+round,-tense,-long]")
           ]
