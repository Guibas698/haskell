atividade = "04"
nome = "Guilherme Lopes dos Santos"
matricula = "556470"

--  1

replace' :: [Char] -> [Char] -> [Char] -> [Char] -> [Char]
replace' _ _ _ [] = []
replace' from to acc xs@(y:ys)
      isPrefixOf from xs = replace' from to (acc ++ to) (drop (length from) xs)
      otherwise = replace' from to (acc ++ [y]) ys

replace :: [Char] -> [Char] -> [Char] -> [Char]
replace text from to = replace' from to "" text


-- 2

lsSplit :: [Int] -> ([Int], Int, [Int])
lsSplit ls = split' [] ls
    where
        split' _ [] = error "Lista vazia"
        split' _ [x] = error "Lista com apenas um elemento"
        split' acc (x:y:ys)
            | x > y = (reverse acc, x, y:ys)
            | otherwise = split' (x:acc) (y:ys)


-- 3

removeMax :: Ord a => [a] -> (a, [a])
removeMax [] = error "Lista vazia"
removeMax [x] = (x, [])
removeMax (x:xs) =
    let (maxRest, rest) = removeMax xs
    in if x >= maxRest
        then (x, maxRest : rest)
        else (maxRest, x : rest)

selectionSort :: Ord a => [a] -> [a]
selectionSort [] = []
selectionSort ls =
    let (max, rest) = removeMax ls
    in max : selectionSort rest




