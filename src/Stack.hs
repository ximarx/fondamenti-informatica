module Stack where

{-=== Definizione tipo ===-}
data Stack a = St [a]
{- una alternativa potrebbe essere:
	type Stack a = [a] deriving Show
	che mi permette di utilizzare le funzioni
	normali sulle []
-}
instance Show a => Show (Stack a) where
	show (St (x:xs)) = show (St xs) ++ show x ++ " "
	show (St []) = ""
	
{-=== Aggiunge un elemento alla pila ===-}
push :: Stack a -> a -> Stack a
push (St xs) valore = St (valore:xs)
push (St []) valore = St [valore]

{-=== Ritorna il primo elemento della pila ===-}
pop :: Stack a -> (a, Stack a)
pop (St (x:xs)) = (x, St xs)
pop (St []) = error "Stack vuoto"

{-=== Verifica se lo stack e' vuoto ===-}
isEmpty :: Stack a -> Bool
isEmpty (St []) = True
isEmpty (St _) = False

{-=== Crea uno stack vuoto ===-}
emptyStack :: Stack a
emptyStack = St []

{-=== Indica la dimensione dello stack ===-}
length :: Stack a -> Int
length (St []) = 0
{- uso Stack.length perche length e' gia in Prelude -}
length (St (x:xs)) = 1 + ( Stack.length (St xs) )

{-=== Popola lo stack con gli elementi del vettore ===-}
build :: [a] -> Stack a
build (x:xs) = push (build xs) x
build [] = emptyStack
