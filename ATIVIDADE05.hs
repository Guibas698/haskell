atividade = "04"
nome = "Guilherme Lopes dos Santos"
matricula = "556470"

-- 1

bis :: Int -> Bool
bis x
     (x `mod` 4 == 0) && (x `mod` 100 /= 0) || (x `mod` 400 == 0) = True
     otherwise = False

-- 2

temp :: Float -> Char -> Char -> Float
temp t fr to =
    if fr == 'c' && to == 'f'
        then (t * (9/5)) + 32
    else if fr == 'f' && to == 'c'
        then (t - 32) * (5/9)
    else if fr == 'c' && to == 'k'
        then t + 273.15
    else if fr == 'k' && to == 'c'
        then t - 273.15
    else if fr == 'f' && to == 'k'
        then (t - 32) * (5/9) + 273.15
    else if fr == 'k' && to == 'f'
        then (t - 273.15) * (9/5) + 32
    else t


-- 3
coin :: String -> [(char, Float)] -> Float 
coin s m = sum [v | (c, v) <- m, elem c s]

