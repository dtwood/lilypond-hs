module Text.Pretty where
  class Pretty a where
    pretty :: a -> String

  pPrint :: Pretty a => a -> IO ()
  pPrint = putStrLn <$> pretty
