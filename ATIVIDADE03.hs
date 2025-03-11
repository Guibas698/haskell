atividade = "3"
nome = "Guilherme Lopes dos Santos"
matricula = "556470"

tls :: String -> [(Char, Int)]
tls s = nubBy (\(x,_)(y,_) -> x == y) result
    where
        result = zip s (comparator s)
        count a s = length [x | x <- s, x == a]
        comparator s = [count c s | c <- s] 



isAlphaNum_ :: Char -> Bool
isAlphaNum_ ch = isAlpha ch || isDigit ch
    where
        isAlpha ch = ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z')
        isDigit ch = '0' <= ch && ch <= '9'

filterIsAlpha :: String -> String
filterIsAlpha = filter (\ch -> isAlphaNum_ ch && notElem ch ['0' .. '9'] || ch == ' ')

tlsString :: [String] -> [(String, Int)]
tlsString [] = []
tlsString (x:xs) = (x, length (filter (==x) (x:xs))) : tlsString (filter (/=x) xs)

maximumBy_ ::[(String, Int)] -> (String, Int)
maximumBy_ [] = error "lista vazia"
maximumBy_ (x:xs) = foldl (\(maxStr, maxInt) (str, int) -> if int > maxInt then (str, int) else (maxStr, maxInt)) x xs


sfq :: String -> (String, Int)
sfq str = maximumBy_ (tlsString (words (filterIsAlpha str)))
