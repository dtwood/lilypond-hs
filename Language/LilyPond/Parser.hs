module Language.LilyPond.Parser (lilyPondParser) where
  import Text.Parsec
  import Data.Char
  import Data.Functor.Identity
  import Language.LilyPond.Types

  pitchClass = do
    pc <- oneOf "abcdefg"
    return $ case pc of
      'a' -> A
      'b' -> B
      'c' -> C
      'd' -> D
      'e' -> E
      'f' -> F
      'g' -> G

  octave = option (Octave 0) octaveHelper

  octaveHelper = do
    o <- oneOf "\',"
    rest <- octave

    return $ case (o, rest) of
      ('\'', Octave x) -> Octave (x + 1)
      (',', Octave x) -> Octave (x - 1)

  pitch = do
    pc <- pitchClass
    o <- octave
    return $ Pitch pc o

  lilyPondParser :: ParsecT String () Identity Pitch
  lilyPondParser = pitch
