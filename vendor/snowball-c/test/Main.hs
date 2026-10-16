module Main (main) where

import           Control.Exception (evaluate)
import           Control.Monad (unless)
import qualified Data.Text as T
import           System.Exit (exitFailure)

import           Text.Stemmer.Snowball


-- (algorithm, word, expected stem)
cases :: [(Algorithm, T.Text, T.Text)]
cases =
  [ (English, "connections", "connect")
  , (English, "running", "run")
  , (French, "été", "été")
  , (French, "élèves", "élev")
  , (French, "développement", "développ")
  , (French, "développer", "développ")
  , (French, "chevaux", "cheval")
  , (French, "cœur", "cœur")
  , (Spanish, "canción", "cancion")
  , (Spanish, "niños", "niñ")
  , (German, "häuser", "haus")
  , (Italian, "abbandonata", "abbandon")
  , (Russian, "книги", "книг")
  , (Russian, "книга", "книг")
  ]

main :: IO ()
main = do
  -- sb_stemmer_new fails (exception) if an algorithm is missing from cbits
  mapM_ (\a -> evaluate (T.length (stem a "test"))) [minBound .. maxBound :: Algorithm]
  let failures = [ (a, w, e, r) | (a, w, e) <- cases, let r = stem a w, r /= e ]
      batch    = stems French ["élèves", "chevaux"]
  mapM_ report failures
  unless (batch == ["élev", "cheval"]) $
    putStrLn $ "stems French: " ++ show batch
  unless (null failures && batch == ["élev", "cheval"]) exitFailure
  putStrLn $ show (length cases) ++ " stems OK"
  where
    report (a, w, e, r) = putStrLn $ show a ++ " " ++ T.unpack w ++ ": expected "
                                     ++ T.unpack e ++ ", got " ++ T.unpack r
