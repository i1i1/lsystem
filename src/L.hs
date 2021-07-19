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

import           Data.List  (find)
import           Data.Maybe (maybe)

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
applyOne rules var = maybe
                       [Variable var]
                       (\(Rule _ it) -> it)
                       (find (\(Rule v _) -> (v == var)) rules)


applyRules :: [Rule] -> [Item] -> [Item]
applyRules rules arr = concat $ map (\x ->
                                       case x of
                                         Variable v -> applyOne rules v
                                         _          -> [x]
                                    ) arr

apply :: Int -> [Rule] -> [Item] -> [Item]
apply 0 _ x = x
apply n r x = apply (n - 1) r (applyRules r x)
