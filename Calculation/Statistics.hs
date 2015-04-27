module Calculation.Statistics
( sumList, average, variance, stdDeviation
) where

-- Sum a list
sumList :: [Float] -> Float
sumList [] = 0
sumList (headList:tailList) = headList + sumList tailList

-- Average
average :: Float -> Float -> Float
average 0 totalSum = 0
average qtdSamples totalSum = (totalSum/qtdSamples)

-- Variance
variance :: [Float] -> Float -> Float
variance [] total = 0
variance list total = (sumList([(i-(average total (sumList list)))^2 | i <- list]))/total

-- Standard Deviation
stdDeviation :: [Float] -> Float -> Float
stdDeviation [] total = 0
stdDeviation list total = sqrt ((sumList([(i-(average total (sumList list)))^2 | i <- list]))/total)
