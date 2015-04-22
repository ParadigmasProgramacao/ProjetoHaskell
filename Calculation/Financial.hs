module Financial (
interest, simpleInterest, compoundInterest,
sacAmor, sacPMT, sacPV, sacInterest,
priceAmor, pricePMT, priceInterest, pricePV
) where

interest :: Float -> Float -> Float
interest i value = value*i

simpleInterest :: Int -> Float -> Float -> Float
simpleInterest n i value = (fromIntegral n)*(interest i value)

-- j=value*(1*i)^n - value
compoundInterest :: Int -> Float -> Float -> Float
compoundInterest 0 _ _ = 0
compoundInterest n i value = let x = (interest i value) 
		in x + compoundInterest (n-1) i (value + x) 

-- Amortization (SAC)
sacAmor :: Int -> Float -> Float
sacAmor n value = value/(fromIntegral n)

sacPMT :: Int -> Float -> Float-> [Float]
sacPMT 0 _ _ = []
sacPMT n i value = let pmt = (sacAmor n value) + (interest value i)
						in pmt:(sacPMT (n-1) i (value - sacAmor n value))

sacPV :: Int -> Float -> Float -> [Float]
sacPV 0 _ _ = []
sacPV n i value = value:
		(sacPV (n-1) i (value - sacAmor n value))

sacInterest :: Int -> Float -> Float -> [Float]
sacInterest n i value = [pv*i| pv <- sacPV n i value]

-- Amortization (PRICE)
pricePMT :: Int -> Float -> Float -> Float
pricePMT n i value = ((interest value i))/(1 - 1/(1+i)^n)

pricePV :: Int -> Float -> Float -> [Float]
pricePV 0 _ _ = []
pricePV n i value = value:
	(pricePV (n-1) i (value*(1+i) - pricePMT n i value))

priceInterest :: Int -> Float -> Float-> [Float]
priceInterest n i value = [pv*i | pv <- (pricePV n i value)]

priceAmor :: Int -> Float -> Float -> [Float]
priceAmor n i value = [(pricePMT n i value)-j
	|j <- priceInterest n i value]