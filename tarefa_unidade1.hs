-- Programa finalizado em 26/07/2025
-- Programa criado por Kevin Gabriel Morais Mangueira

import Data.Char (toLower)

-- Função para normalizar o input
normalizar :: Char -> Char // recebe um caractere
normalizar c
    -- A com acento.
    | c `elem` "ÁÀÂÃÄáàâãä" = 'a'
    -- E com acento.
    | c `elem` "ÉÈÊËéèêë" = 'e'
    -- I com acento.
    | c `elem` "ÍÌÎÏíìîï" = 'i'
    -- O com acento.
    | c `elem` "ÓÒÔÕÖóòôõö" = 'o'
    -- U com acento.
    | c `elem` "ÚÙÛÜúùûü" = 'u'
    -- Ç para C
    | c `elem` "Çç" = 'c'
    -- Letras maiúsculas sem acento para letras minúsculas sem acento.
    | c `elem` "ABCDEFGHIJKLMNOPQRSTUVWXYZ" = toLower c
    -- Outros caracteres
    | otherwise = c


-- Função para realizar a substituição em um caractere normalizado.
cifra :: [Char] -> Char -> Char -- Recebe a chave e um caractere.
cifra chave c
    | c >= 'a' && c <= 'z' = head [y| (x, y) <- zip ['a', 'b'.. 'z'] chave, c == x]
    | otherwise = c

-- Função de criptografia em String.
monoAlphaCipherE :: [Char] -> String -> String // Recebe a chave e uma string
monoAlphaCipherE chave xs = [cifra chave (normalizar(x))| x <- xs]

-- Função para realizar a substituição contrária em um único caractere.
cifraCont :: [Char] -> Char -> Char // Recebe a chave e um caractere
cifraCont chave c
    | c >= 'A' && c <= 'Z' = head [y| (x, y) <- zip chave ['a', 'b' .. 'z'], x == c]
    | otherwise = c

-- Função para descriptografar a string.
descriptografar :: [Char] -> String -> String // Recebe a chave e uma string
descriptografar chave xs = [cifraCont chave x| x <- xs]

-- Exemplos:

-- Exemplo1 - criptografia:
exemplo1 = monoAlphaCipherE ['Z', 'Y', 'N', 'G', 'W', 'Q', 'A', 'M', 'X', 'P', 'K', 'V', 'U', 'L', 'C', 'E', 'F', 'R', 'I', 'B', 'S', 'J', 'D', 'O', 'T', 'H'] "OLA"

-- Exemplo2 - descriptografia:
exemplo2 = descriptografar ['Z', 'Y', 'N', 'G', 'W', 'Q', 'A', 'M', 'X', 'P', 'K', 'V', 'U', 'L', 'C', 'E', 'F', 'R', 'I', 'B', 'S', 'J', 'D', 'O', 'T', 'H'] "CVZ"

-- Exemplo3 - Exemplo no pdf:
exemplo3 = monoAlphaCipherE ['Z', 'Y', 'N', 'G', 'W', 'Q', 'A', 'M', 'X', 'P', 'K', 'V', 'U', 'L', 'C', 'E', 'F', 'R', 'I', 'B', 'S', 'J', 'D', 'O', 'T', 'H'] "criptografia monoalfabetica"

-- Exemplo4 - Exemplo no pdf:
exemplo4 = descriptografar ['Z', 'Y', 'N', 'G', 'W', 'Q', 'A', 'M', 'X', 'P', 'K', 'V', 'U', 'L', 'C', 'E', 'F', 'R', 'I', 'B', 'S', 'J', 'D', 'O', 'T', 'H'] "NRXEBCARZQXZ UCLCZVQZYWBXNZ"