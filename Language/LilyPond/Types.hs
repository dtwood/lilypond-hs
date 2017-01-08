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

  newtype Octave = Octave Int
    deriving (Show)

  data Pitch = Pitch PitchClass Octave
    deriving (Show)
