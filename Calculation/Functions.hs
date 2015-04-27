module Calculation.Functions (doSum, doMult, doDiv, doMinus) where

-- Calcular média
import Data.Char

doSum :: [Float] -> Int -> Float
doSum [] qnt= 0.0
doSum _ 0 = 0.0
doSum (x:xs) 1 = x 
doSum (x:xs) qnt = doSum (xs) (qnt-1) + x

doMinus :: [Float] -> Int -> Float
doMinus [] qnt= 0.0
doMinus _ 0 = 0.0
doMinus (x:xs) 1 = - x 
doMinus (x:xs) qnt = - doMinus (xs) (qnt-1) - x

doMult :: [Float] -> Int -> Float
doMult [] qnt= 1
doMult _ 0 = 1
doMult (x:xs) 1 = x 
doMult (x:xs) qnt = doMult (xs) (qnt-1) * x

doDiv :: [Float] -> Int -> Float
doDiv [] qnt= 1
doDiv _ 0 = 1
doDiv (x:xs) 1 = x 
doDiv (x:xs) qnt = doDiv (xs) (qnt-1) / x