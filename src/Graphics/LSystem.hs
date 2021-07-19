{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Graphics.LSystem ( generate
                        , Variable(..)
                        , Item(..)
                        , Rule(..)
                        , Settings
                        , LSystem
                        , defaultSettings
                        , applyL
                        ) where

import           Draw         (Settings (..), draw)
import           Graphics.Svg (Element)
import           L            (Item (..), Rule (..), Variable (..), apply)


data LSystem = LSystem { axiom :: [Item]
                       , rules :: [Rule]
                       } deriving (Show, Eq)

applyL :: LSystem -> Int -> [Item]
applyL LSystem { axiom, rules } n = apply n rules axiom

generate :: LSystem -> Int -> Settings -> Element
generate lsys n s = draw s (applyL lsys n)

char :: Char -> Item
char ch = Variable (Char ch)

example1 :: LSystem
example1 = LSystem { axiom = [ Variable F, RotL, char 'X', Variable F, RotL, Variable F, RotL, char 'X', Variable F ]
                   , rules = [
                   --  X -> XF-F+F-XF+F+XF-F+F-X
                         Rule (Char 'X') [
                               -- XF-F+F-
                               char 'X', Variable F, RotR, Variable F, RotL, Variable F, RotR,
                               -- XF+F+
                               char 'X', Variable F, RotL, Variable F, RotL,
                               -- XF-F+F-X
                               char 'X', Variable F, RotR, Variable F, RotL, Variable F, RotR,
                               char 'X'
                         ] ]
                   }


-- axiom = F+F+F+F
-- F -> FF+F++F+F
-- angle = 90
example2 :: LSystem
example2 = LSystem { axiom = [ Variable F, RotL, Variable F, RotL, Variable F, RotL, Variable F ]
                   , rules = [
                         Rule F [
                               Variable F, Variable F, RotL, Variable F, RotL, RotL, Variable F, RotL, Variable F
                         ] ]
                   }

defaultSettings :: Settings
defaultSettings = Settings { start       = (1000, 100)
                           , size        = (1000, 1000)
                           , line_length = 2
                           , line_width  = 0.5
                           , line_mult   = 1.5
                           , by_angle    = pi / 2
                           }
