module Render where

import SDL
import SDL.Primitive
import Control.Monad (unless)
import Linear (V2(..), V4(..))
import Forma (Forma(..))
import Movimentacao (Grade, moverFormas)

-- Função para desenhar uma forma em uma posição específica
desenharForma :: Renderer -> Forma -> V2 Int -> IO ()
desenharForma renderer forma pos = 
  let (V2 i j) = pos                       -- i é a linha, j é a coluna
      x = fromIntegral (j * 100)           -- Coordenada x na tela
      y = fromIntegral (i * 100)           -- Coordenada y na tela
      centro = V2 (x + 50) (y + 50)        -- Centro da célula
      cor = case forma of                  -- Definir a cor de cada forma
              X -> V4 0 119 216 255        -- Azul PlayStation
              T -> V4 0 206 107 255        -- Verde PlayStation
              Q -> V4 255 10 178 255       -- Rosa PlayStation
              C -> V4 255 23 23 255        -- Vermelho PlayStation
  in case forma of
       X -> do
         line renderer (V2 (x+20) (y+20)) (V2 (x+80) (y+80)) cor          -- Diagonal 1
         line renderer (V2 (x+80) (y+20)) (V2 (x+20) (y+80)) cor          -- Diagonal 2
       T -> triangle renderer (V2 (x+50) (y+20)) (V2 (x+20) (y+80)) (V2 (x+80) (y+80)) cor
       Q -> rectangle renderer (V2 (x+20) (y+20)) (V2 (x+80) (y+80)) cor
       C -> circle renderer centro 30 cor

-- Loop principal
appLoop :: Renderer -> Grade -> IO ()
appLoop renderer grade = do
  rendererDrawColor renderer $= V4 0 0 0 255  -- Fundo preto
  clear renderer

  -- Desenhar todas as formas na grade
  mapM_ (\(i, row) -> mapM_ (\(j, forma) -> desenharForma renderer forma (V2 i j)) (zip [0..] row)) (zip [0..] grade)

  present renderer   -- Atualizar a tela

  SDL.delay 400      -- Esperar 250ms

  -- Mover as formas para a próxima posição
  let novaGrade = moverFormas grade

  -- Verificar se o usuário quer sair
  events <- pollEvents
  let quit = any (\e -> case eventPayload e of SDL.QuitEvent -> True; _ -> False) events
  unless quit $ appLoop renderer novaGrade  -- Continuar o loop se não houver saída
