module Main where

import Calculation.Financial
import Text.Printf
import Util.Input

options = ["1 - Calcular SAC", "2 - Calcular Price", "3 - Sair"]

showMenu [] = putStrLn "\n"
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

showOption _ = putStrLn "Opcao invalida"

main = do
	showMenu options
	inpOption <- getLine
	if ((read inpOption :: Int) == 3)
		then putStrLn "bye-bye"
		else do
			showOption (read inpOption :: Int)
			main