module Language.LilyPond.Types where
  data PitchClass = A
             | B
             | C
             | D
             | E
             | F
             | G
             | Other Char
             deriving (Show)

  newtype Quotes = Quotes Int
    deriving (Show)

  data Pitch = Pitch PitchClass Quotes
    deriving (Show)

  newtype Exclamations = Exclamations Int
    deriving (Show)

  newtype Questions = Questions Int
    deriving (Show)

  data OctaveCheck = OctaveCheck Quotes
                   | NoOctaveCheck
    deriving (Show)

  data MaybeNotemodeDuration = MaybeNotemodeDurationNotImplemented
    deriving (Show)

  data OptionalRest = OptionalRest
                    | NoOptionalRest
    deriving (Show)

  data PostEvents = PostEventsNotImplemented
    deriving (Show)

  data NewChord = NewChordNotImplemented
    deriving (Show)

  data PitchOrMusic = PitchOrMusicPitch Pitch Exclamations Questions OctaveCheck MaybeNotemodeDuration OptionalRest PostEvents
                    | PitchOrMusicChord NewChord PostEvents
    deriving (Show)
