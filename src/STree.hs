module STree where

{-=== Tipo SearchTree ===-}
data STree a = SNodo a (STree a) (STree a)
						| Vuoto

{-=== Overload Show ===-}
instance Show a => Show (STree a) where
			show (SNodo root sx dx) = "(" ++ show sx ++ ") " ++ show root ++ " (" ++ show dx ++ ")"
			show (Vuoto) = ""


{-=== Inserisce un nuovo elemento nell'albero di ricerca ===-}
insert :: Ord a => STree a -> a -> STree a
insert (SNodo root sx dx) valore
						| valore < root = SNodo root (insert sx valore) dx
						| valore > root = SNodo root sx (insert dx valore)
						| valore == root = (SNodo root sx dx)
insert Vuoto valore = (SNodo valore Vuoto Vuoto)

{-=== Restituisce la lista ordinata di componenti dell'albero ===-}
flatten :: Ord a => STree a -> [a]
flatten (SNodo root sx dx) = flatten sx ++ [root] ++ flatten dx
flatten Vuoto = []

{-=== Costruisce l'albero da un vettore di elementi ===-}
buildSTree :: Ord a => [a] -> STree a
buildSTree (x:xs) = insert (buildSTree xs) x
buildSTree [] = Vuoto

{-=== Indica la presenza di un elemento nell'albero ===-}
search :: Ord a => STree a -> a -> Bool
search (SNodo root sx dx) valore
					| valore == root = True
					| valore < root = search sx valore
					| valore > root = search dx valore
search Vuoto valore = False

{-=== Restituisce la lista di nodi che soddisfano una proprieta' ===-}
take :: Ord a => STree a -> (a -> Bool) -> [a]
take albero proprieta = [x | x <- (flatten albero), proprieta x == True]
take Vuoto _ = []

		