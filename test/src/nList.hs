
data NList a = Node [NList a] | Leaf a deriving Show

mkNList :: [Int] -> [a] -> NList a
mkNList [] xs = Node $ map Leaf xs
mkNList na@(_ : ns) xs = Node $ map (mkNList ns) $ groupN (product na) xs

groupN :: Int -> [a] -> [[a]]
groupN _ [] = []
groupN n xs = take n xs : groupN n (drop n xs)
