module Syllables where

import Data.List (sortBy)
import Data.Ord (comparing)

import Text.ParserCombinators.Parsec


syllabifyString input = case parse ipaSyllable "(unknown)" input of
                          Right fm -> fm
                          Left e -> error $ show e

ipaSyllable = ipaOnset >>= (\ons -> ipaNucleus >>= (\nuc -> ipaNext >>= (\(coda, syls) -> return ([ons, nuc, coda]++syls))))
ipaOnset = choice $ map (try . string) onsets
ipaNucleus = choice $ map (try . string) nuclei
ipaNext = (eof >> return ("", []))
          <|> try (ipaSyllable >>= \syls -> return ("", syls))
          <|> try (ipaCoda >>= \coda -> eof >> return (coda, []))
          <|> try (ipaCoda >>= \coda -> (ipaSyllable >>= \syls -> return (coda, syls)))
ipaCoda = choice $ map (try . string) codas

onsets = reverse $ sortBy (comparing length) 
         ["p","t","k","r","m","n","ŋ","s","j","w","pr","tr","kr","sp","st","sk","sn"]
nuclei = reverse $ sortBy (comparing length) 
         ["a","e","i","o","u","aw","aj"]
codas = sortBy (comparing length) 
        ["p","t","k","r","m","n","ŋ","s"]