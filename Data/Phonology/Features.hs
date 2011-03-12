module Features where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.List (intercalate)

import Control.Monad (foldM)

import Text.ParserCombinators.Parsec

data FValue = Plus | Minus | Unspec
              deriving (Eq, Ord)
instance Show FValue where
    show Plus = "+"
    show Minus = "-"
    show Unspec = "0"
instance Read FValue where
    readsPrec _ value =
        tryParse [('+', Plus), ('-', Minus), ('0',Unspec)]
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

type Segment = (String, FMatrix)

infixr 5 |>|
infixr 5 |?|

(|>|) :: FMatrix -> FMatrix -> FMatrix
FMatrix fm2 |>| FMatrix fm1 = FMatrix $ Map.union fm1 fm2

(|?|) :: FMatrix -> FMatrix -> Bool
FMatrix comparandum |?| FMatrix comparison = 
                        Map.foldWithKey 
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
fValue = oneOf "+-0" >>= return . read . (:[])

feature :: GenParser Char st (String, FValue)
feature = spaces >> fValue >>= \v -> (fName >>= \k -> spaces >> return (k, v))

readIPA :: String -> [Segment]
readIPA input = case parse ipaString "feature matrix" input of
                  Right fm -> fm
                  Left e -> error $ show e

ipaString :: GenParser Char st [Segment]
ipaString = many ipaSegment

ipaSegment = choice (map (\(l,fs) -> try $ string l >> return (l, readFMatrix fs)) segments)

segmentFMatrices segs = [(seg, readFMatrix fs) | (seg, fs) <- segs]

segments = [ ("p", "[-syl,-son,-cont,-nas,0high,0back,0low,-cor,-dor]")
           , ("t", "[-syl,-son,-cont,-nas,0high,0back,0low,+cor,-dor]")
           , ("k", "[-syl,-son,-cont,-nas,0high,0back,0low,-cor,+dor]")
           , ("m", "[-syl,+son,-cont,+nas,0high,0back,0low,-cor,-dor]")
           , ("n", "[-syl,+son,-cont,+nas,0high,0back,0low,+cor,-dor]")
           , ("ŋ", "[-syl,+son,-cont,+nas,0high,0back,0low,-cor,+dor]")
           , ("s", "[-syl,-son,+cont,-nas,0high,0back,0low,+cor,-dor]")
           , ("l", "[-syl,+son,+cont,-nas,0high,0back,0low,-cor,-dor]")
           , ("i", "[+syl,+son,+cont,-nas,+high,-back,-low,-cor,-dor]")
           , ("u", "[+syl,+son,+cont,-nas,+high,+back,-low,-cor,-dor]")
           , ("e", "[+syl,+son,+cont,-nas,-high,-back,-low,-cor,-dor]")
           , ("o", "[+syl,+son,+cont,-nas,-high,+back,-low,-cor,-dor]")
           , ("a", "[+syl,+son,+cont,-nas,-high,-back,+low,-cor,-dor]")
           ]