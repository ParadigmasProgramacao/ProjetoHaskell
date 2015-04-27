module Second where

import Text.Printf
import Util.Input
import System.Exit
import Data.Char
import Calculation.Functions


addVariable :: Float -> [Float] -> [Float]
addVariable value variables = (value:variables)

dropVariable :: [Float] -> [Float]
dropVariable variables = tail variables

addCommand value commands = (value:commands)

printVariables variables = print (head variables)


calculate variables i n pv fv = do

	value <- getLine
	case value of
		"quit" -> exitSuccess
		_		-> do
						if(isDigit  (head value) == True )
							then do
									let newVar = (addVariable (read value:: Float) variables)
									calculate newVar i n pv fv
							else do
									case value of
										"clear"	-> do
													calculate [] 0 0 0 0

										"sum" 	-> do
													let newVar = addVariable (doSum variables 2) (drop 2 variables)
													print (doSum variables 2)
													calculate newVar i n pv fv

										"minus" -> do
													let newVar = addVariable (doMinus variables 2) (drop 2 variables)
													print (doMinus variables 2)
													calculate newVar i n pv fv

										"mult" 	-> do
													let newVar = addVariable (doMult variables 2) (drop 2 variables)
													print (doMult variables 2)
													calculate newVar i n pv fv

										"div" 	-> do
													let newVar = addVariable (doDiv variables 2) (drop 2 variables)
													print (doDiv variables 2)
													calculate newVar i n pv fv

										"i"		-> do
													if((n /= 0) && (pv /= 0) && (fv /= 0))
														then do
															let newI = ((fv/pv) **(1/n)) - 1
															print newI
															let newVar = addVariable newI variables
															calculate newVar newI n pv fv
													else do
														let newI = (head variables)
														let newVar = dropVariable variables
														calculate newVar newI n pv fv
													

										"n"		-> do
													if((i /= 0) && (pv /= 0) && (fv /= 0))
														then do
															let newN = (log(fv) - log(pv))/log(1+i)
															print newN
															let newVar = addVariable newN variables
															calculate newVar i newN pv fv
													else do
														let newN = (head variables)
														let newVar = dropVariable variables
														calculate newVar i newN pv fv
													

										"pv"		-> do
													if((n /= 0) && (i /= 0) && (fv /= 0))
														then do
															let newPv = fv / (1 + i) ** n
															print newPv
															let newVar = addVariable newPv variables
															calculate newVar i n newPv fv
													else do
														let newPv = (head variables)
														let newVar = dropVariable variables
														calculate newVar i n newPv fv
													

										"fv"		-> do
													if((n /= 0) && (pv /= 0) && (i /= 0))
														then do
															let newFv = pv * (1 + i) ** n
															print newFv
															let newVar = addVariable newFv variables
															calculate newVar i n pv newFv
													else do
														let newFv = (head variables)
														let newVar = dropVariable variables
														calculate newVar i n pv newFv
													

										_	-> do 
													putStrLn "Comando nao reconhecido"
													calculate variables i n pv fv


									
						


main = do
	let variables = []
	calculate variables 0 0 0 0
	

	