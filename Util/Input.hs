module Util.Input where

import System.IO.Unsafe

get t = do
	putStrLn t
	getLine

gInt :: String -> IO Int
gInt t = do
	putStrLn t
	inp <- getLine
	return (read inp::Int)

getInt :: String -> Int
getInt t = unsafePerformIO $ gInt t

gFloat :: String -> IO Float
gFloat t = do
	putStrLn t
	inp <- getLine
	return (read inp::Float)

getFloat :: String -> Float
getFloat t = unsafePerformIO (gFloat t)