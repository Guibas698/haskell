atividade = "07"
nome = "Guilherme Lopes dos Santos"
matricula = "556470" 


data Stack a = Empty | Value a (Stack a) deriving Show

-- Função para empilhar um valor
push :: Stack a -> a -> Stack a
push stack x = Value x stack

-- Função para desempilhar (pop)
pop :: Stack a -> Stack a
pop Empty = Empty
pop (Value _ rest) = rest

-- Função para ver o elemento no topo da pilha
top :: Stack a -> Maybe a
top Empty = Nothing
top (Value x _) = Just x

-- Função para verificar se a pilha está vazia
isEmpty :: Stack a -> Bool
isEmpty Empty = True
isEmpty _ = False

-- Precedência dos operadores
precedence :: Char -> Int
precedence '^' = 3
precedence '*' = 2
precedence '/' = 2
precedence '+' = 1
precedence '-' = 1
precedence _   = 0

-- Verifica se um caractere é um operador
isOperator :: Char -> Bool
isOperator c = c `elem` "+-*/^"

-- Algoritmo de conversão para notação pós-fixa
posFixa :: [Char] -> [Char]
posFixa = posFixa' [] Empty

posFixa' :: [Char] -> Stack Char -> [Char] -> [Char]
posFixa' output stack [] = output ++ popRemaining stack
posFixa' output stack (x:xs)
    | x `elem` "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789" = posFixa' (output ++ [x]) stack xs
    | x == '(' = posFixa' output (push stack x) xs
    | x == ')' = let (output', stack') = popTillOpenParen output stack
                 in posFixa' output' stack' xs
    | isOperator x = let (output', stack') = popOperators output stack x
                     in posFixa' output' (push stack' x) xs
    | otherwise = "Caractere inválido na entrada!"

-- Desempilha até o parêntese de abertura
popTillOpenParen :: [Char] -> Stack Char -> ([Char], Stack Char)
popTillOpenParen output Empty = (output, Empty)
popTillOpenParen output (Value '(' rest) = (output, rest)
popTillOpenParen output (Value x rest) = popTillOpenParen (output ++ [x]) rest

-- Desempilha operadores com precedência maior ou igual ao atual
popOperators :: [Char] -> Stack Char -> Char -> ([Char], Stack Char)
popOperators output Empty _ = (output, Empty)
popOperators output stack@(Value t rest) x
    | isOperator t && precedence t >= precedence x = popOperators (output ++ [t]) rest x
    | otherwise = (output, stack)

-- Desempilha qualquer operador restante na pilha no final
popRemaining :: Stack Char -> [Char]
popRemaining Empty = []
popRemaining (Value x rest) = x : popRemaining rest