--
-- L system
--
-- Only part of http://paulbourke.net/fractals/lsys/ is implemented
--

{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module L ( Variable(..)
         , Item (..)
         , Rule (..)
         , apply
         , applyOne
         ) where

import           Data.Text

data Variable = Char Char
              | F
              | Fd
              deriving (Show, Eq)

data Item = RotL
          | RotR
          | Reverse
          | IncL
          | DecL
          | Push
          | Pop
          | Variable Variable
          deriving (Show, Eq)

data Rule = Rule Variable [Item] deriving (Show, Eq)

applyOne :: [Rule] -> Variable -> [Item]
applyOne [] var                          = [Variable var]
applyOne ((Rule v it):rs) var | v == var = it
applyOne ((Rule v it):rs) var            = applyOne rs var


applyRules :: [Rule] -> [Item] -> [Item]
applyRules rules []              = []
applyRules rules (Variable v:xs) = (applyOne rules v) ++ (applyRules rules xs)
applyRules rules (x:xs)          = [x] ++ (applyRules rules xs)

apply :: Int -> [Rule] -> [Item] -> [Item]
apply 0 _ x = x
apply n r x = apply (n - 1) r (applyRules r x)
