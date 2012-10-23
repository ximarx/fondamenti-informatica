module Esercizi where

import Queue
import Stack
import STree
import Tree

{-=== Data una lista e un predicato, 
prendere tutti gli elementi fino a quando 
il predicato non risulta falso ===-}
takeWhile' :: [a] -> (a -> Bool) -> [a]
takeWhile' (x:xs) prop 
				| prop x == True = x: (takeWhile' xs prop)
				| otherwise = []
takeWhile' [] _ = []

{-=== Data una lista e un predicato,
prendere tutti gli elementi a partire dal primo
per il quale la proprieta risulta falsa ===-}
dropWhile' :: [a] -> (a -> Bool) -> [a]
dropWhile' (x:xs) prop
				| prop x == True = dropWhile' xs prop
				| otherwise = (x:xs)
dropWhile' [] _ = []

{-=== Data una lista e un predicato,
prendere tutti gli elementi per cui
il predicato e' vero ===-}
findAll :: [a] -> (a -> Bool) -> [a]
findAll xs prop = [x | x <- xs, prop x == True]
findAll [] _ = []

{-=== Date due lista, dire se la prima e' il prefisso della seconda ===-}
isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' (x:xs) (y:ys)
					| x == y = (isPrefixOf' xs ys)
					| otherwise = False
isPrefixOf' [] (y:ys) = True
isPrefixOf' (x:xs) [] = False

{-=== Dato un albero e un predicato, prendere gli elementi
dell'albero che soddisfano il predicato e memorizzarli in una lista ===-}
take :: Eq a => Tree a -> (a -> Bool) -> [a]
take albero prop = Tree.take albero prop
{-=== Proviamo a controllare se si riesce a fare proxy ===-}

{-=== Data una enumerazione di elementi memorizzarli
in un albero in modo tale da poterli stampare in modo ordinato ===-}
take_2 :: Ord a => [a] -> [a]
take_2 xs = STree.flatten (STree.buildSTree xs)

{-=== verificare quante volte un elemento compare in un albero ===-}
elemento :: Eq a => Tree a -> a -> Int
elemento albero elemento = Tree.count albero elemento

{-=== simulazione di applicazione che fa un po di tutto con le varie funzioni ===-}
main_ :: STree String -> IO ()
main_ albero = do
					putStr "Continuare?"
					y <- getChar
					if y == 'Y' then
						do
							x <- getChar
							main_ (STree.insert albero (bla x))
					else
						putStr (concat (STree.flatten albero))
			
			
bla :: Char -> String
bla x = [x]



main :: IO ()
main = do
			putStrLn "Programmino babumba. Cosa vuoi fare?"
			putStrLn "> 1: Operazioni su lista"
			putStrLn "> 2: Operazioni su coda [pigro, non fatta interfaccia]"
			putStrLn "> 3: Operazioni su pila [pigro, non fatta interfaccia]" 
			putStrLn "> 4: Operazioni su albero [pigro, non fatta interfaccia]"
			putStrLn "> 5: Operazioni su albero di ricerca [pigro, non fatta interfaccia]"
			putStrLn "> -Altro-: esce dall'applicazione"
			scelta <- getLine
			{-putStrLn ""-}
			case scelta of
				"1"			-> do
									mainListe
									main {- Garantisce ricorsione -}
				{-
				"2"			-> do
									mainCode
									main
				"3" 		-> do
									mainPile
									main
				"4"			-> do
									mainTree
									main
				"5"			-> do
									mainSTree
									main
				-}
				otherwise 	-> return () {- Termina il programma -}

{- ======================-}
{- Giochetti sulle liste -}
{- ======================-}
				
mainListe :: IO ()
mainListe = do
			putStrLn "Ci piace lavorare con i numeri,"
			putStrLn "quindi spara un po di numeri a caso"
			putStrLn "e vediamo cosa succede"
			_acqLista []
			
			
_acqLista :: [Int] -> IO ()
_acqLista (xs) = do
					putStr "   dammi un numero o altro per fermarti> "
					x <- getLine
					if isNumeric x then
						do
							_acqLista ( xs ++ [(read x :: Int)] )
							return ()
					else 
						do
							{- Array acquisito, ci lavoro -}
							_lavLista xs
							return ()
							
_lavLista :: [Int] -> IO ()
_lavLista xs = do
					putStrLn "La lista di partenza:"
					putStr "       "
					print xs
					putStrLn ""
					{- takeWhile' -}
					putStrLn "--- Prendiamo tutti i pari fino al primo dispari:"
					putStrLn "(takeWhile' xs even)"
					putStr "       "
					print (takeWhile' xs even)
					putStrLn ""
					{- dropWhile' -}
					putStrLn "--- Prendiamo tutti i numeri dal primo dispari:"
					putStrLn "(dropWhile' xs even)"
					putStr "       "
					print (dropWhile' xs even)
					putStrLn ""
					{- findAll -}
					putStrLn "--- Prendiamo tutti i numeri dispari:"
					putStrLn "(findAll xs odd)"
					putStr "       "
					print (findAll xs odd)
					putStrLn ""
					{- isPrefixOf' -}
					putStrLn "Vediamo se la lista e' un prefisso di se stessa:"
					putStrLn "--- isPrefixOf' xs (xs ++ [100])"
					putStr "       "
					print (isPrefixOf' xs (xs ++ [100]))
					putStrLn ""
					{- !isPrefixOf' -}
					putStrLn "--- Vediamo se cambiamo la prima lista:"
					putStrLn "isPrefixOf' (3:xs) (xs ++ [100])"
					putStr "       "
					print (isPrefixOf' (3:xs) (xs ++ [100]))
					putStrLn ""
					{- take_2 -}
					putStrLn "--- Ordiniamo la lista usando un albero di ricerca:"
					putStrLn "take_2 xs"
					putStr "       "
					print (take_2 xs)
					putStrLn ""
					
					return ()
					
{- ==================================================== -}					
{- Funzioni per valutare se l'acquisizione e' un numero -}
{- ==================================================== -}
isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False
 
isDouble s = case reads s :: [(Double, String)] of
  [(_, "")] -> True
  _         -> False
 
isNumeric :: String -> Bool
isNumeric s = isInteger s || isDouble s

			
{-
test :: Int -> [Int] -> IO ()
test 0 array = do
					print array
					return ()
test (n+1) array = do
					putStr "Numero: "
					x <- getLine
					if isNumeric x then
						do
							test n ( (read x :: Int) :array)
							return ()
					else 
						do
							test 0 array
							return ()
							
-}				