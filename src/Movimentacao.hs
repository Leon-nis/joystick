module Movimentacao where

import Forma (Forma(..))
import System.Random (randomRIO)
import Control.Monad (replicateM)

-- Tipo para a grade 3x3
type Grade = [[Forma]]

-- Função para mover as formas em sentido horário, mantendo o centro fixo
moverFormas :: Grade -> Grade
moverFormas [[a, b, c], [d, e, f], [g, h, i]] =
  [[d, a, b],  -- Primeira linha: (1,0) -> (0,0), (0,0) -> (0,1), (0,1) -> (0,2)
   [g, e, c],  -- Segunda linha: (2,0) -> (1,0), centro fixo, (0,2) -> (1,2)
   [h, i, f]]  -- Terceira linha: (2,1) -> (2,0), (2,2) -> (2,1), (1,2) -> (2,2)
moverFormas _ = error "Grade deve ser 3x3"

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

-- Divide uma lista em pedaços de tamanho n
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)
