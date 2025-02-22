module Main where

import SDL
import qualified Data.Text as T
import Render (appLoop)
import Movimentacao (gerarGradeInicial)

main :: IO ()
main = do
  initializeAll
  window <- createWindow (T.pack "Movimentação de Formas") $ defaultWindow { windowInitialSize = V2 300 300 }
  renderer <- createRenderer window (-1) defaultRenderer

  -- Grade inicial aleatória
  gradeInicial <- gerarGradeInicial

  appLoop renderer gradeInicial

  destroyRenderer renderer
  destroyWindow window
  quit
