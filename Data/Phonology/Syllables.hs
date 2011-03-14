module Syllables where

import Data.List (sortBy)
import Data.Ord (comparing)

import Text.ParserCombinators.Parsec


syllabifyString input = case parse ipaSyllable "(unknown)" input of
                          Right fm -> fm
                          Left e -> error $ show e

--ipaSyllable :: GenParser Char st [String]
--ipaSyllable = ipaOnset >>= (\ons -> ipaNucleus >>= (\nuc -> ipaNext >>= (\(cod, syls) -> return ([ons, nuc, cod]++syls))))

ipaSyllable :: GenParser Char st [[String]]
ipaSyllable = ipaOnset >>= (\ons -> ipaRhyme >>= (\([nuc, cod], syls) -> return ([[ons, nuc, cod]] ++ syls)))

ipaOnset :: GenParser Char st String
ipaOnset = choice $ map (try . string) onsets

ipaNucleus :: GenParser Char st String
ipaNucleus = choice $ map (try . string) nuclei

ipaRhyme :: GenParser Char st ([String], [[String]])
ipaRhyme = choice (map (\s -> try (string s >>= \nuc -> (ipaNext >>= \(cod, syls) -> return ([nuc, cod], syls)))) nuclei)

ipaNext :: GenParser Char st (String, [[String]])
ipaNext = (eof >> return ("", []))
          <|> try (ipaSyllable >>= \syls -> return ("", syls))
          <|> try (ipaCoda >>= \coda -> eof >> return (coda, []))
          <|> try (ipaCoda >>= \coda -> (ipaSyllable >>= \syls -> return (coda, syls)))
ipaCoda = choice $ map (try . string) codas

onsets = reverse $ sortBy (comparing length) 
         ["","p","t","k","r","m","n","ŋ","s","j","w","pr","tr","kr","sp","st","sk","sn"]
nuclei = reverse $ sortBy (comparing length) 
         ["a","e","i","o","u","aw","aj"]
codas = reverse $ sortBy (comparing length) 
        ["", "p","t","k","r","m","n","ŋ","s"]