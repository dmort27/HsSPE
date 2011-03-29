module Data.Phonology.Tests where

import Data.Phonology.Features
import Data.Phonology.Rules
import Data.Phonology.RuleParsers

r1 = readRule defState "[+cons]->[+voi]/[+voi]_"
t1 = readIPA defState "appb"
t2 = readIPA defState "#cbacabc#"

rs = ["[-syl]->[+voi]/[+voi]_", "[-syl]->[-voi]/_#"]
rs' = ["a->b/b_","b->a/a_"]

main = prettyDerivationV (readRuleV defState) t1 rs