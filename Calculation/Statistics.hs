module Statistics (averag) where

-- Calcular média
average :: [Float] -> Float
average [] = 0.0
average (x:xs) = sum(x:xs) / (fromIntegral (length(x:xs)))


