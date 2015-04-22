module Financial (
sacPMT
) where

-- Amortization (SAC)
sacAmor :: Int -> Float -> Float
sacAmor n value = value/(fromIntegral n)

sacPMT :: Int -> Float -> Float-> [Float]
sacPMT 0 _ _ = []
sacPMT n i value = let pmt = (sacAmor n value) + value*i
						in pmt:(sacPMT (n-1) i (value - sacAmor n value))

sacPV :: Int -> Float -> Float -> [Float]
sacPV 0 _ _ = []
sacPV n i value = value:
		(sacPV (n-1) i (value - sacAmor n value))

sacInterest :: Int -> Float -> Float -> [Float]
sacInterest n i value = [pv*i| pv <- sacPV n i value]

-- Amortization (PRICE)
pricePMT :: Int -> Float -> Float -> Float
pricePMT n i value = (value*i)/(1 - 1/(1+i)^n)

pricePV :: Int -> Float -> Float -> [Float]
pricePV 0 _ _ = []
pricePV n i value = value:
	(pricePV (n-1) i (value*(1+i) - pricePMT n i value))

priceInterest :: Int -> Float -> Float-> [Float]
priceInterest n i value = [pv*i | pv <- (pricePV n i value)]

priceAmor :: Int -> Float -> Float -> [Float]
priceAmor n i value = [(pricePMT n i value)-j| 
	j <- priceInterest n i value]