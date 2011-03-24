module Features where

import Data.Maybe (fromJust)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (intercalate, sortBy)
import Data.Ord (comparing)
import Debug.Trace (trace)
import Control.Monad (foldM)
--import qualified System.IO.UTF8 as UTF8
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
        tryParse ([('+', Plus), ('-', Minus), ('0',Unspec)] ++ [(s, Var (s:[])) | s <- ['α'..'ω']])
            where
              tryParse [] = []
              tryParse ((attempt, result):xs) =
                  if (head value) == attempt
                  then [(result, tail value)]
                  else tryParse xs

data FMatrix = FMatrix (Map String FValue)
             deriving (Eq, Ord)
instance Show FMatrix where
    show (FMatrix fm) = (\x -> "[" ++ x ++ "]") $ intercalate "," $ map (\(k,v) -> (show v) ++ k) $ Map.toAscList fm

fmapFM f (FMatrix fm) = FMatrix $ f fm

fromFM (FMatrix fm) = fm

type Segment = (String, FMatrix)
type RuleState = ([Segment], [Segment], [(String, FMatrix -> FMatrix)])

infixr 5 |>|
infixr 5 |?|

(|>|) :: FMatrix -> FMatrix -> FMatrix
FMatrix fm2 |>| FMatrix fm1 = FMatrix $ Map.union fm1 fm2

(|?|) :: FMatrix -> FMatrix -> Bool
FMatrix comparandum |?| FMatrix comparison = 
                        Map.foldrWithKey 
                               (\k v acc -> 
                                    (Map.findWithDefault (flipFValue v) k comparandum) `elem` [Unspec, v]) 
                           True comparison

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
          \fm -> spaces >> char ']' >> (return $ FMatrix $ Map.fromList fm)

fName :: GenParser Char st String
fName = many (oneOf ['a'..'z'])

fValue :: GenParser Char st FValue
fValue = oneOf ("+-0" ++ ['α'..'ω']) >>= return . read . (:[])

feature :: GenParser Char st (String, FValue)
feature = spaces >> fValue >>= \v -> (fName >>= \k -> spaces >> return (k, v))

readIPA :: [Segment] -> [Segment] -> [(String, FMatrix -> FMatrix)] -> String -> [Segment]
readIPA segs macs dias input = case runParser ipaString (segs, macs, dias) "feature matrix" input of
                  Right fm -> fm
                  Left e -> error $ show e

ipaString :: GenParser Char RuleState [Segment]
ipaString = many (ipaSegment >>= ipaDiacritics)

ipaSegment :: GenParser Char RuleState (String, FMatrix)
ipaSegment = getState >>= \(segs, _, _) -> 
             (choice (map (\(l,fs) -> try $ string l >> return (l, fs)) segs))

ipaDiacritics :: (String, FMatrix) -> GenParser Char RuleState (String, FMatrix)
ipaDiacritics (seg, fm) = getState >>= \(_, _, dias) -> 
                          (many (choice (map (\(d, f) -> try $ string d >> return (d, f)) dias)) >>=
                                return . foldr (\(d, f) (seg', fm') -> (seg' ++ d, f fm')) (seg, fm))
                                    
toFMatrixPairs :: [(String, String)] -> [(String, FMatrix)]
toFMatrixPairs segs = reverse $ sortBy (comparing (length . fst)) [(seg, readFMatrix fs) | (seg, fs) <- segs]

diacriticFunctions :: [(String, String)] -> [(String, (FMatrix -> FMatrix))]
diacriticFunctions dias = [(dia, \fm -> fm |>| (readFMatrix fts)) | (dia, fts) <- dias]

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
          difference a b = Map.differenceWithKey (\_ a' b' -> if a' /= b' then Just a' else Nothing) a b
                                             
applyDiacritics :: [(String, FMatrix -> FMatrix)] -> Segment -> [Segment]
applyDiacritics dias (s, fm) = map (\(dia, f) -> (s++dia, f fm)) dias

defFeatures = ["syl","son","cons","cont","delrel","lat","nas","voi","cg","sg","ant","cor","distr","hi","lo","back","round","tense"]

defDiacritics = diacriticFunctions diacritics
defSegments = map (includeFts defFeatures) $ toFMatrixPairs segments
defMacros = map (includeFts defFeatures) $ toFMatrixPairs macros

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
             ]

macros = [ ("C", "[-syl]")
         , ("V", "[+syl]")
         , ("N", "[-syl,+nas]")
         , ("X", "[]")
         ]

segments = [ ("p","[-syl,-son,+cons,-cont,-delrel,-lat,-nas,-voi,-cg,-sg,+ant,+lab,-cor,0distr,-hi,-lo,-back,-round,0tense]")
           , ("pf","[-syl,-son,+cons,-cont,+delrel,-lat,-nas,-voi,-cg,-sg,+ant,+lab,-cor,0distr,-hi,-lo,-back,-round,0tense]")
           , ("t","[-syl,-son,+cons,-cont,-delrel,-lat,-nas,-voi,-cg,-sg,+ant,-lab,+cor,-distr,-hi,-lo,-back,-round,0tense]")
           , ("t̪","[-syl,-son,+cons,-cont,-delrel,-lat,-nas,-voi,-cg,-sg,+ant,-lab,+cor,+distr,-hi,-lo,-back,-round,0tense]")
           , ("ts","[-syl,-son,+cons,-cont,+delrel,-lat,-nas,-voi,-cg,-sg,+ant,-lab,+cor,-distr,-hi,-lo,-back,-round,0tense]")
           , ("tʃ","[-syl,-son,+cons,-cont,+delrel,-lat,-nas,-voi,-cg,-sg,-ant,-lab,+cor,+distr,-hi,-lo,-back,-round,0tense]")
           , ("ʈ","[-syl,-son,+cons,-cont,-delrel,-lat,-nas,-voi,-cg,-sg,-ant,-lab,+cor,-distr,-hi,-lo,-back,-round,0tense]")
           , ("c","[-syl,-son,+cons,-cont,-delrel,-lat,-nas,-voi,-cg,-sg,-ant,-lab,-cor,0distr,+hi,-lo,-back,-round,0tense]")
           , ("k","[-syl,-son,+cons,-cont,-delrel,-lat,-nas,-voi,-cg,-sg,-ant,-lab,-cor,0distr,+hi,-lo,+back,-round,0tense]")
           , ("q","[-syl,-son,+cons,-cont,-delrel,-lat,-nas,-voi,-cg,-sg,-ant,+lab,-cor,0distr,-hi,-lo,+back,-round,0tense]")
           , ("b","[-syl,-son,+cons,-cont,-delrel,-lat,-nas,+voi,-cg,-sg,+ant,+lab,-cor,0distr,-hi,-lo,-back,-round,0tense]")
           , ("bv","[-syl,-son,+cons,-cont,+delrel,-lat,-nas,+voi,-cg,-sg,+ant,+lab,-cor,0distr,-hi,-lo,-back,-round,0tense]")
           , ("d","[-syl,-son,+cons,-cont,-delrel,-lat,-nas,+voi,-cg,-sg,+ant,-lab,+cor,-distr,-hi,-lo,-back,-round,0tense]")
           , ("d̪","[-syl,-son,+cons,-cont,-delrel,-lat,-nas,+voi,-cg,-sg,+ant,-lab,+cor,+distr,-hi,-lo,-back,-round,0tense]")
           , ("dz","[-syl,-son,+cons,-cont,+delrel,-lat,-nas,+voi,-cg,-sg,+ant,-lab,+cor,-distr,-hi,-lo,-back,-round,0tense]")
           , ("dʒ","[-syl,-son,+cons,-cont,+delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,+cor,+distr,-hi,-lo,-back,-round,0tense]")
           , ("ɖ","[-syl,-son,+cons,-cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,+cor,-distr,-hi,-lo,-back,-round,0tense]")
           , ("ɟ","[-syl,-son,+cons,-cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,0distr,+hi,-lo,-back,-round,0tense]")
           , ("g","[-syl,-son,+cons,-cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,0distr,+hi,-lo,+back,-round,0tense]")
           , ("ɢ","[-syl,-son,+cons,-cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,0distr,-hi,-lo,+back,-round,0tense]")
           , ("ɓ","[-syl,-son,+cons,-cont,-delrel,-lat,-nas,+voi,+cg,-sg,+ant,+lab,-cor,0distr,-hi,-lo,-back,-round,0tense]")
           , ("ɗ","[-syl,-son,+cons,-cont,-delrel,-lat,-nas,+voi,+cg,-sg,+ant,-lab,+cor,-distr,-hi,-lo,-back,-round,0tense]")
           , ("ʄ","[-syl,-son,+cons,-cont,-delrel,-lat,-nas,+voi,+cg,-sg,-ant,-lab,-cor,0distr,+hi,-lo,-back,-round,0tense]")
           , ("ɠ","[-syl,-son,+cons,-cont,-delrel,-lat,-nas,+voi,+cg,-sg,-ant,-lab,-cor,0distr,+hi,-lo,+back,-round,0tense]")
           , ("ʛ","[-syl,-son,+cons,-cont,-delrel,-lat,-nas,+voi,+cg,-sg,-ant,-lab,-cor,0distr,-hi,-lo,+back,-round,0tense]")
           , ("ɸ","[-syl,-son,+cons,+cont,-delrel,-lat,-nas,-voi,-cg,-sg,+ant,+lab,-cor,0distr,-hi,-lo,-back,-round,0tense]")
           , ("β","[-syl,-son,+cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,+ant,+lab,-cor,0distr,-hi,-lo,-back,-round,0tense]")
           , ("f","[-syl,-son,+cons,+cont,-delrel,-lat,-nas,-voi,-cg,-sg,+ant,+lab,-cor,0distr,-hi,-lo,-back,-round,0tense]")
           , ("v","[-syl,-son,+cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,+ant,+lab,-cor,0distr,-hi,-lo,-back,-round,0tense]")
           , ("θ","[-syl,-son,+cons,+cont,-delrel,-lat,-nas,-voi,-cg,-sg,+ant,-lab,+cor,+distr,-hi,-lo,-back,-round,0tense]")
           , ("ð","[-syl,-son,+cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,+ant,-lab,+cor,+distr,-hi,-lo,-back,-round,0tense]")
           , ("s","[-syl,-son,+cons,+cont,-delrel,-lat,-nas,-voi,-cg,-sg,+ant,-lab,+cor,-distr,-hi,-lo,-back,-round,0tense]")
           , ("s̪","[-syl,-son,+cons,+cont,-delrel,-lat,-nas,-voi,-cg,-sg,+ant,-lab,+cor,+distr,-hi,-lo,-back,-round,0tense]")
           , ("z","[-syl,-son,+cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,+ant,-lab,+cor,-distr,-hi,-lo,-back,-round,0tense]")
           , ("ʃ","[-syl,-son,+cons,+cont,-delrel,-lat,-nas,-voi,-cg,-sg,-ant,-lab,+cor,+distr,-hi,-lo,-back,-round,0tense]")
           , ("ʒ","[-syl,-son,+cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,+cor,+distr,-hi,-lo,-back,-round,0tense]")
           , ("ʂ","[-syl,-son,+cons,+cont,-delrel,-lat,-nas,-voi,-cg,-sg,-ant,-lab,+cor,-distr,-hi,-lo,-back,-round,0tense]")
           , ("ʐ","[-syl,-son,+cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,+cor,-distr,-hi,-lo,-back,-round,0tense]")
           , ("ç","[-syl,-son,+cons,+cont,-delrel,-lat,-nas,-voi,-cg,-sg,-ant,-lab,-cor,0distr,+hi,-lo,-back,-round,0tense]")
           , ("ʝ","[-syl,-son,+cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,0distr,+hi,-lo,-back,-round,0tense]")
           , ("x","[-syl,-son,+cons,+cont,-delrel,-lat,-nas,-voi,-cg,-sg,-ant,-lab,-cor,0distr,+hi,-lo,+back,-round,0tense]")
           , ("ɣ","[-syl,-son,+cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,0distr,+hi,-lo,+back,-round,0tense]")
           , ("χ","[-syl,-son,+cons,+cont,-delrel,-lat,-nas,-voi,-cg,-sg,-ant,-lab,-cor,0distr,-hi,-lo,+back,-round,0tense]")
           , ("ʁ","[-syl,-son,+cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,0distr,-hi,-lo,+back,-round,0tense]")
           , ("ħ","[-syl,-son,+cons,+cont,-delrel,-lat,-nas,-voi,-cg,-sg,-ant,-lab,-cor,0distr,-hi,+lo,+back,-round,0tense]")
           , ("ʕ","[-syl,-son,+cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,0distr,-hi,+lo,+back,-round,0tense]")
           , ("h","[-syl,-son,+cons,+cont,-delrel,-lat,-nas,-voi,-cg,+sg,-ant,-lab,-cor,0distr,-hi,-lo,-back,-round,0tense]")
           , ("ɦ","[-syl,-son,+cons,+cont,-delrel,-lat,+nas,-voi,-cg,+sg,-ant,-lab,-cor,0distr,-hi,-lo,-back,-round,0tense]")
           , ("m","[-syl,+son,+cons,-cont,-delrel,-lat,+nas,+voi,-cg,-sg,+ant,+lab,-cor,0distr,-hi,-lo,-back,-round,0tense]")
           , ("n","[-syl,+son,+cons,-cont,-delrel,-lat,+nas,+voi,-cg,-sg,+ant,-lab,+cor,-distr,-hi,-lo,-back,-round,0tense]")
           , ("n̪","[-syl,+son,+cons,-cont,-delrel,-lat,+nas,+voi,-cg,-sg,+ant,-lab,+cor,+distr,-hi,-lo,-back,-round,0tense]")
           , ("ɲ","[-syl,+son,+cons,-cont,-delrel,-lat,+nas,+voi,-cg,-sg,+ant,-lab,-cor,0distr,+hi,-lo,-back,-round,0tense]")
           , ("ɳ","[-syl,+son,+cons,-cont,-delrel,-lat,+nas,+voi,-cg,-sg,-ant,-lab,+cor,0distr,-hi,-lo,-back,-round,0tense]")
           , ("ŋ","[-syl,+son,+cons,-cont,-delrel,-lat,+nas,+voi,-cg,-sg,-ant,-lab,-cor,0distr,+hi,-lo,+back,-round,0tense]")
           , ("ɴ","[-syl,+son,+cons,-cont,-delrel,-lat,+nas,+voi,-cg,-sg,-ant,-lab,-cor,0distr,-hi,-lo,+back,-round,0tense]")
           , ("ɹ","[-syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,+cor,-distr,+hi,-lo,+back,+round,0tense]")
           , ("ɻ","[-syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,+cor,-distr,-hi,-lo,-back,-round,0tense]")
           , ("l","[-syl,+son,+cons,+cont,-delrel,+lat,-nas,+voi,-cg,-sg,+ant,-lab,+cor,-distr,-hi,-lo,-back,-round,0tense]")
           , ("l̪","[-syl,+son,+cons,+cont,-delrel,+lat,-nas,+voi,-cg,-sg,+ant,-lab,+cor,+distr,-hi,-lo,-back,-round,0tense]")
           , ("j","[-syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,0distr,+hi,-lo,-back,-round,0tense]")
           , ("w","[-syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,+lab,-cor,0distr,+hi,-lo,+back,+round,0tense]")
           , ("tl","[-syl,+son,+cons,+cont,+delrel,+lat,-nas,-voi,-cg,-sg,+ant,-lab,+cor,-distr,-hi,-lo,-back,-round,0tense]")
           , ("dl","[-syl,+son,+cons,+cont,+delrel,+lat,-nas,+voi,-cg,-sg,+ant,-lab,+cor,-distr,-hi,-lo,-back,-round,0tense]")
           , ("ʔ","[-syl,+son,-cons,-cont,-delrel,-lat,-nas,-voi,+cg,-sg,-ant,-lab,-cor,0distr,-hi,-lo,-back,-round,0tense]")
           , ("i","[+syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,-distr,+hi,-lo,-back,-round,+tense]")
           , ("y","[+syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,+lab,-cor,-distr,+hi,-lo,-back,+round,+tense]")
           , ("ɨ","[+syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,-distr,+hi,-lo,+back,-round,+tense]")
           , ("u","[+syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,+lab,-cor,-distr,+hi,-lo,+back,+round,+tense]")
           , ("e","[+syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,-distr,-hi,-lo,-back,-round,+tense]")
           , ("ø","[+syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,-distr,-hi,-lo,-back,+round,+tense]")
           , ("ʌ","[+syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,-distr,-hi,-lo,+back,-round,+tense]")
           , ("o","[+syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,-distr,-hi,-lo,+back,+round,+tense]")
           , ("æ","[+syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,-distr,-hi,+lo,-back,-round,+tense]")
           , ("ɶ","[+syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,-distr,-hi,+lo,-back,+round,+tense]")
           , ("a","[+syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,-distr,-hi,+lo,+back,-round,+tense]")
           , ("ɒ","[+syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,-distr,-hi,+lo,+back,+round,+tense]")
           , ("ɪ","[+syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,-distr,+hi,-lo,-back,-round,-tense]")
           , ("ʏ","[+syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,-distr,+hi,-lo,-back,+round,-tense]")
           , ("ɯ","[+syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,-distr,+hi,-lo,+back,-round,-tense]")
           , ("ʊ","[+syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,-distr,+hi,-lo,+back,+round,-tense]")
           , ("ɛ","[+syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,-distr,-hi,-lo,-back,-round,-tense]")
           , ("œ","[+syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,-distr,-hi,-lo,-back,+round,-tense]")
           , ("ə","[+syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,-distr,-hi,-lo,+back,-round,-tense]")
           , ("ɔ","[+syl,+son,-cons,+cont,-delrel,-lat,-nas,+voi,-cg,-sg,-ant,-lab,-cor,-distr,-hi,-lo,+back,+round,-tense]")
           ]
