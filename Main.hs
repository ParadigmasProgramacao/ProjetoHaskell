module Main where

import Calculation.Financial
import Text.Printf

options = ["1 - Calcular SAC", "2 - Calcular Price", "3 - Sair"]

showMenu [] = putStrLn "\n"
showMenu (h:t) = do
	putStrLn h
	(showMenu t)

tuplaToString (a,b,c,d) = (printf "%.2f" a)
	++"  "++(printf "%.2f"  b)
	++"  "++(printf "%.2f"  c)
	++"  "++(printf "%.2f"  d)

showTable [] _ _ _ = putStrLn "\n"
showTable (h:t) (x:s) (l:g)  (n:m)= do
	putStrLn (tuplaToString (h,x,l, n))
	showTable t s g m


showOption 1 = do
	putStrLn "Insira o numero de prestacoes:"
	n <- getLine
	putStrLn $ "Insira o valor da taxa (0 - 1):"
	i <-getLine 
	putStrLn $ "Insira o montante:"
	value <- getLine
	putStrLn "PMT     Amor.     Juros     PV"
	let pmt = (sacPMT (read n::Int) (read i::Float) (read value::Float))
	let amor = [sacAmor (read n::Int) (read value::Float) | _ <- pmt]
	showTable 
		pmt
		amor
		(sacInterest (read n::Int) (read i::Float) (read value::Float))
		(sacPV (read n::Int) (read i::Float) (read value::Float))
	
showOption 2 = do
	putStrLn "Insira o numero de prestacoes:"
	n <- getLine
	putStrLn $ "Insira o valor da taxa (0 - 1):"
	i <-getLine 
	putStrLn $ "Insira o montante:"
	value <- getLine
	putStrLn "PMT     Amor.     Juros     PV"
	let amor = (priceAmor (read n::Int) (read i::Float) (read value::Float))
	let pmt = pricePMT (read n::Int) (read i::Float) (read value::Float)
	showTable
		[pmt | _ <- amor]
		amor
		(priceInterest (read n::Int) (read i::Float) (read value::Float))
		(pricePV (read n::Int) (read i::Float) (read value::Float))	

showOption _ = putStrLn "Opcao invalida"

main = do
	showMenu options
	inpOption <- getLine
	if ((read inpOption :: Int) == 3)
		then putStrLn "bye-bye"
		else do
			showOption (read inpOption :: Int)
			main