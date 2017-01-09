module Language.LilyPond.Parser (lilyPondParser) where
  import Control.Monad
  import Data.Char
  import Data.Functor.Identity
  import Language.LilyPond.Types
  import Text.Parsec

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

  quotes = option (Quotes 0) quotesHelper

  quotesHelper = do
    o <- oneOf "\',"
    spaces
    rest <- quotes

    return $ case (o, rest) of
      ('\'', Quotes x) -> Quotes (x + 1)
      (',', Quotes x) -> Quotes (x - 1)

  pitch = do
    pc <- pitchClass
    spaces
    o <- quotes

    return $ Pitch pc o

  exclamationsHelper = do
    char '!'
    spaces
    rest <- exclamations

    return $ case rest of
      Exclamations e -> Exclamations (e + 1)

  exclamations = option (Exclamations 0) exclamationsHelper

  questionsHelper = do
    char '?'
    spaces
    rest <- questions

    return $ case rest of
      Questions e -> Questions (e + 1)

  questions = option (Questions 0) questionsHelper

  octaveCheckHelper :: ParsecT String u Identity OctaveCheck
  octaveCheckHelper = do
    char '='
    spaces
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
    spaces
    e <- exclamations
    spaces
    q <- questions
    spaces
    oc <- octaveCheck
    spaces
    mnd <- maybeNotemodeDuration
    spaces
    or_ <- optionalRest
    spaces
    pe <- postEvents
    return $ PitchOrMusicPitch p e q oc mnd or_ pe

  pitchOrMusicChord = do
    nc <- newChord
    spaces
    pe <- postEvents
    return $ PitchOrMusicChord nc pe

  pitchOrMusic = choice [pitchOrMusicPitch, pitchOrMusicChord]

  lilyPondParser :: ParsecT String () Identity [PitchOrMusic]
  lilyPondParser = do
    spaces
    a <- many pitchOrMusic
    spaces
    eof
    return a
