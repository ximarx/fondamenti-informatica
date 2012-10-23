module Tree where

data Tree a = Nodo a (Tree a) (Tree a)
			| Vuoto
			deriving Show
		

{-=== Costruttore albero vuoto ===-}
emptyTree :: Tree a
emptyTree = Vuoto

{-=== Costruttore bottom-up ===-}
createTree :: a -> Tree a -> Tree a -> Tree a
createTree root sx dx = (Nodo root sx dx)

{-=== Costruttore foglie ===-}
leaf :: a -> Tree a
leaf value = createTree value Vuoto Vuoto

{-=== Albero -> Array con intervisita ===-}
flatten :: Tree a -> [a]
flatten (Nodo root sx dx) = flatten sx ++ [root] ++ flatten dx
flatten Vuoto = []

{-=== Cerca l'elemento nell'albero ===-}
search :: Eq a => Tree a -> a -> Bool
search (Nodo root sx dx) value = root == value
								|| search sx value
								|| search dx value
search Vuoto _ = False

{-=== Conta il numero di occorrenze dell'elemento nell'albero ===-}
count :: Eq a => Tree a -> a -> Int
count (Nodo root sx dx) value
							| root == value = 1 + (count sx value) + (count dx value)
							| otherwise = (count sx value) + (count dx value)
count Vuoto _ = 0

{-=== Esporta in lista l'elenco di elementi che soddisfano la proprieta' ===-}
take :: Eq a => Tree a -> (a -> Bool) -> [a]
take (Nodo root sx dx) prop
					| prop root == True = [root] ++ (Tree.take sx prop) ++ (Tree.take dx prop)
					| otherwise = (Tree.take sx prop) ++ (Tree.take dx prop)
take Vuoto _ = []

		
