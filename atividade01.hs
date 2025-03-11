atividade = "1"

nome = "Guilherme Lopes dos Santos"
matricula = "556470"

-- 1
-- Recebe uma string e 
-- retorna-a sem as vogais.   
noVog :: String -> String
noVog s = [x | x<- s, x `notElem` "AEIOUaeiou"]

-- 2
-- retorna quantas vezes x é divisível por n
num'divs :: Int -> Int -> Int
num'divs x n | x `mod` n == 0 = 1+ num'divs (x `div ` n) n | otherwise = 0 

-- 3
-- Dado um inteiro n. determinar se
-- ele é ou não um número primeo
is'prime :: Int -> Bool
is'prime n = length [n | x <- [1..n],  n `mod` x == 0] == 2

-- 4
-- inverte um inteiro, por exemplo
-- o inverso de 251 é 152.
int'inv :: Int -> Int
int'inv x = read (reverse (show x)) :: Int

