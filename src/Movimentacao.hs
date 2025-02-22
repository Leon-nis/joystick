module Movimentacao where

import Forma (Forma(..))
import System.Random (randomRIO)
import Control.Monad (replicateM)

-- Tipo para a grade 3x3
type Grade = [[Forma]]

-- Função para mover as formas
moverFormas :: Grade -> Grade
moverFormas grade = 
  let flattened = concat grade              -- Achatamos a grade em uma lista
      rotated = last flattened : init flattened  -- Rotacionamos: último elemento vai pro início
  in chunksOf 3 rotated                     -- Dividimos em linhas de 3 elementos

-- Divide uma lista em pedaços de tamanho n
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- Gerar uma forma aleatória
gerarFormaAleatoria :: IO Forma
gerarFormaAleatoria = do
  r <- randomRIO (0 :: Int, 3 :: Int)  -- Tipos explícitos
  return $ case r of
    0 -> X
    1 -> T
    2 -> Q
    3 -> C
    _ -> error "Número fora do intervalo" -- Não deve ocorrer

-- Gerar grade inicial aleatória com 9 formas
gerarGradeInicial :: IO Grade
gerarGradeInicial = do
  formas <- replicateM 9 gerarFormaAleatoria  -- Gera 9 formas aleatórias
  return $ chunksOf 3 formas                   -- Divide em 3 linhas de 3
