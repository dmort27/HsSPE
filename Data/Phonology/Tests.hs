module Data.Phonology.Tests where

import Data.Phonology.Features
import Data.Phonology.Rules
import Data.Phonology.RuleParsers

r1 = readRule defState "[+cons]->[+voi]/[+voi]_"
t1 = readIPA defState "#bp#"
t2 = readIPA defState "#cbacabc#"

rs = ["[-syl]->[Avoi]/[-syl,Avoi]_", "[-syl]->[-voi]/_#"]
rs' = ["a->b/b_","b->a/a_"]

simple = applyRule (readRule defState "[-syl]->[+voi]/[+voi]_") (readIPA defState "u")

main = prettyDerivationV (readRuleV defState) t1 rs