module Interacao where

import SDL
import Forma (Forma(..))
import Movimentacao (Grade)

-- Função para processar eventos de teclado e trocar formas
processarTeclas :: [Event] -> Grade -> Grade
processarTeclas events grade = foldl processarEvento grade events

-- Processa um evento individual
processarEvento :: Grade -> Event -> Grade
processarEvento grade event =
  case eventPayload event of
    KeyboardEvent keyboardEvent ->
      if keyboardEventKeyMotion keyboardEvent == Pressed
      then case keysymKeycode (keyboardEventKeysym keyboardEvent) of
             KeycodeUp    -> trocarVerticalCima grade
             KeycodeDown  -> trocarVerticalBaixo grade
             KeycodeLeft  -> trocarHorizontalEsquerda grade
             KeycodeRight -> trocarHorizontalDireita grade
             _            -> grade
      else grade
    _ -> grade

-- Troca: (0,1) -> (2,1) -> (1,1) -> (0,1)
trocarVerticalCima :: Grade -> Grade
trocarVerticalCima [[a, b, c], [d, e, f], [g, h, i]] = [[a, e, c], [d, h, f], [g, b, i]]
trocarVerticalCima _ = error "Grade deve ser 3x3"

-- Troca: (1,1) -> (2,1) -> (0,1) -> (1,1)
trocarVerticalBaixo :: Grade -> Grade
trocarVerticalBaixo [[a, b, c], [d, e, f], [g, h, i]] = [[a, h, c], [d, b, f], [g, e, i]]
trocarVerticalBaixo _ = error "Grade deve ser 3x3"

-- Troca: (1,0) -> (1,1) -> (1,2) -> (1,0)
trocarHorizontalEsquerda :: Grade -> Grade
trocarHorizontalEsquerda [[a, b, c], [d, e, f], [g, h, i]] = [[a, b, c], [e, f, d], [g, h, i]]
trocarHorizontalEsquerda _ = error "Grade deve ser 3x3"

-- Troca: (1,1) -> (1,2) -> (1,0) -> (1,1)
trocarHorizontalDireita :: Grade -> Grade
trocarHorizontalDireita [[a, b, c], [d, e, f], [g, h, i]] = [[a, b, c], [f, d, e], [g, h, i]]
trocarHorizontalDireita _ = error "Grade deve ser 3x3"
