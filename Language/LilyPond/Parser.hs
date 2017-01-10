module Language.LilyPond.Parser (lilyPondParser) where
  import Control.Monad
  import Data.Char
  import Data.Functor.Identity
  import Language.LilyPond.Types
  import Text.Parsec

  pitchClass = do
    pc <- oneOf "abcdefg" <?> "note"
    return $ case pc of
      'a' -> A
      'b' -> B
      'c' -> C
      'd' -> D
      'e' -> E
      'f' -> F
      'g' -> G

  quotes = option (Quotes 0) (quoteUpHelper <|> quoteDownHelper)

  quoteUpHelper = do
    o <- char '\''
    rest <- quotes

    return $ case rest of
      Quotes x -> Quotes (x + 1)

  quoteDownHelper = do
    o <- char ','
    rest <- quotes

    return $ case rest of
      Quotes x -> Quotes (x - 1)

  pitch = do
    pc <- pitchClass
    o <- quotes

    return $ Pitch pc o

  exclamationsHelper = do
    char '!'
    rest <- exclamations

    return $ case rest of
      Exclamations e -> Exclamations (e + 1)

  exclamations = option (Exclamations 0) exclamationsHelper

  questionsHelper = do
    char '?'
    rest <- questions

    return $ case rest of
      Questions e -> Questions (e + 1)

  questions = option (Questions 0) questionsHelper

  octaveCheckHelper :: ParsecT String u Identity OctaveCheck
  octaveCheckHelper = do
    char '='
    q <- quotes
    return $ OctaveCheck q

  octaveCheck :: ParsecT String u Identity OctaveCheck
  octaveCheck =
    option NoOctaveCheck octaveCheckHelper

  maybeNotemodeDuration = do
    return MaybeNotemodeDurationNotImplemented

  optionalRestHelper = do
    string "\\rest"
    return OptionalRest

  optionalRest =
    option (NoOptionalRest) optionalRestHelper

  postEvents = do
    return PostEventsNotImplemented

  newChord = unexpected "not implemented"

  pitchOrMusicPitch = do
    p <- pitch
    e <- exclamations
    q <- questions
    oc <- octaveCheck
    mnd <- maybeNotemodeDuration
    or_ <- optionalRest
    pe <- postEvents
    return $ PitchOrMusicPitch p e q oc mnd or_ pe

  pitchOrMusicChord = do
    nc <- newChord
    pe <- postEvents
    return $ PitchOrMusicChord nc pe

  pitchOrMusic = pitchOrMusicPitch <|> pitchOrMusicChord

  lilyPondParser :: ParsecT String () Identity [PitchOrMusic]
  lilyPondParser = do
    a <- many (pitchOrMusic <* many1 space)
    eof
    return a
