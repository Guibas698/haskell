atividade = "06"
nome = "Guilherme Lopes dos Santos"
matricula = "556470"

-- 01
-- crie uma função que determine se uma string é anagrama de outra
sortString :: [Char] -> [Char]
sortString [] = []
sortString (x:xs) = sortString [y | y <- xs, y <= x] ++ [x] ++ sortString [y | y <- xs, y > x]

anagrama :: [Char] -> [Char] -> Bool
anagrama str1 str2 = sortString str1 == sortString str2
-- teste 
-- $ anagrama rau loa
-- true

-- 02
-- construa função que elimine repetções de uma dada string s
--  sem alterar a sequência original 
-- dos caracteres de s.
unique :: [Char] -> [Char]
unique s = uniqueHelper s []

uniqueHelper :: [Char] -> [Char] -> [Char]
uniqueHelper [] _ = []
uniqueHelper (x:xs) seen
    | x elem seen = uniqueHelper xs seen
    | otherwise     = x : uniqueHelper xs (x:seen)
-- teste
-- $ unique "aabbxa" 
-- $ "abx"

-- 03
-- implemente uma função que determine a string formada pelos 
-- caracteres comuns a duas strins de entrada a e b. A saida não 
-- deve ter duplicadas.
intersec :: [Char] -> [Char] -> [Char]
intersec a b = unique [x | x <- a, x elem b]

unique :: [Char] -> [Char]
unique s = uniqueHelper s []

uniqueHelper :: [Char] -> [Char] -> [Char]
uniqueHelper [] _ = []
uniqueHelper (x:xs) seen
    | x elem seen = uniqueHelper xs seen
    | otherwise     = x : uniqueHelper xs (x:seen)
-- teste
-- $ intersec "abcd" "cdef"
-- $ "cd"

-- 04
-- dado três listas zipálas numa lista de triplas de forma 
-- semelhante ao comando zip. 
zip'linha :: [a] -> [b] -> [c] -> [(a, b, c)]
zip'linha (x:xs) (y:ys) (z:zs) = (x, y, z) : zip'linha xs ys zs
zip'linha _ _ _ = []
-- teste 01
-- zip'linha
-- $ [1,2,3] "abc" [TRUE,FALSE,TRUE] 
-- $ [(1,"a", TRUE), (2, "b", FALSE), (3, "c", TRUE)] 
-- teste 02
-- $ zip'linha [1,2,3,4] "abc" [TRUE] 
-- $ [(1,"a",TRUE)]