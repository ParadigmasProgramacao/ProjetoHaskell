module Main where

import Calculation.Financial
import Calculation.Statistics
import Text.Printf
import Util.Input

options = ["1 - Calcular SAC", "2 - Calcular Price", "3 - Calcular Media", "4 - Calcular variância", "5 - Calcular Desvio Padrao", "6 - Sair"]

showMenu [] = putStr ""
showMenu (h:t) = do
	putStrLn h
	(showMenu t)

tuplaToString (a,b,c,d) = "PMT | "++(printf "%.2f" a)
	++" | Amor. | "++(printf "%.2f"  b)
	++" | Juros | "++(printf "%.2f"  c)
	++" | PV | "++(printf "%.2f"  d)++" |"

showTable [] _ _ _ = putStrLn "\n"
showTable (h:t) (x:s) (l:g)  (n:m)= do
	putStrLn (tuplaToString (h,x,l, n))
	showTable t s g m

showOption 1 = do
	let n = getInt "Insira o numero de prestacoes:"  
	let i = getFloat "Insira o valor da taxa (0 - 1):" 
	let value = getFloat "Insira o montante:"
	
	putStrLn $ (show n)++"\n"++ (show i)++"\n"++ (show value)++"\n"

	let pmt = (sacPMT n i value)
	let amor = [sacAmor n value | _ <- pmt]
	
	showTable 
		pmt
		amor
		(sacInterest n i value)
		(sacPV n i value)
	
showOption 2 = do
	let n = getInt "Insira o numero de prestacoes:"  
	let i = getFloat "Insira o valor da taxa (0 - 1):" 
	let value = getFloat "Insira o montante:" 

	putStrLn $ (show n)++"\n"++ (show i)++"\n"++ (show value)++"\n"

	showTable
		[pricePMT n i value | _ <- priceAmor n i value]
		(priceAmor n i value)
		(priceInterest n i value)
		(pricePV n i value)

showOption 3 = do
	let n = getFloat "Numero de amostragem:"
	putStrLn $ (show n)
	putStrLn "Lista de amostragens:"
	samplesList <- getLine
	let list = map read $ words samplesList :: [Float]
	let sumOfTheList = (sumList list)
	let averageValue = (average n sumOfTheList)

	putStrLn "Media: "
	putStrLn $ (show averageValue)

showOption 4 = do
	let n = getFloat "Numero de amostragem:"
	putStrLn $ (show n)
	putStrLn "Lista de amostragens:"
	samplesList <- getLine
	let list = map read $ words samplesList :: [Float]
	let averageValue = (variance list n)

	putStrLn "Variancia da amostragem: "
	putStrLn $ (show averageValue)

showOption 5 = do
	let n = getFloat "Numero de amostragem:"
	putStrLn $ (show n)
	putStrLn "Lista de amostragens:"
	samplesList <- getLine
	let list = map read $ words samplesList :: [Float]
	let standardDev = (stdDeviation list n)

	putStrLn "Desvio Padrao: "
	putStrLn $ (show standardDev)

showOption _ = putStrLn "Opcao invalida"

main = do
	showMenu options
	inpOption <- getLine

	let intOption = (read inpOption :: Int)
	case intOption of
		6 	->  putStrLn "Finalizando..."
		_	-> 	do
					showOption intOption
					main