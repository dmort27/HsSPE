module Syllables where

import Data.List (sortBy)
import Data.Ord (comparing)

import Text.ParserCombinators.Parsec

type Syllable = (String, String, String)
type ONC = ([String], [String], [String])

syllabifyString onc input = case parse (ipaSyllable onc) "(unknown)" input of
                          Right fm -> fm
                          Left e -> error $ show e

ipaSyllable :: ONC -> GenParser Char st [Syllable]
ipaSyllable onc@(os, _, _) = (ipaOnset os) >>= (\ons -> (ipaRhyme onc) >>= (\((nuc, cod), syls) -> return ((ons, nuc, cod):syls)))

ipaOnset :: [String] -> GenParser Char st String
ipaOnset os = choice $ map (try . string) os

ipaRhyme :: ONC -> GenParser Char st ((String, String), [Syllable])
ipaRhyme onc@(_, ns, _) = choice (map (\s -> try (string s >>= \nuc -> (ipaNext onc >>= \(cod, syls) -> return ((nuc, cod), syls)))) ns)

ipaNext :: ONC -> GenParser Char st (String, [Syllable])
ipaNext onc@(_, _, cs) = (eof >> return ("", []))
          <|> try (ipaSyllable onc >>= \syls -> return ("", syls))
          <|> try (ipaCoda cs >>= \coda -> eof >> return (coda, []))
          <|> try (ipaCoda cs >>= \coda -> (ipaSyllable onc >>= \syls -> return (coda, syls)))

ipaCoda :: [String] -> GenParser Char st String
ipaCoda cs = choice $ map (try . string) cs

onsets = reverse $ sortBy (comparing length) 
         ["","p","t","k","r","m","n","ŋ","s","j","w","pr","tr","kr","sp","st","sk","sn"]
nuclei = sortBy (comparing length) 
         ["a","e","i","o","u","aw","aj"]
codas = reverse $ sortBy (comparing length) 
        ["", "p","t","k","r","m","n","ŋ","s"]