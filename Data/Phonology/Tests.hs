module Data.Phonology.Tests where

import Data.Phonology.Features
import Data.Phonology.Rules
import Data.Phonology.RuleParsers

r1 = readRule defState "[+cons]->[+voi]/[+voi]_"
t1 = readIPA defState "#ampakda#"
t2 = readIPA defState "#cbacabc#"

rs = ["[-syl,-son]->[αvoi]/[-syl,αvoi]_", "0->a/C_C", "C -> 0 / _#"]

main = prettyDerivationV (readRuleV defState) t1 rs