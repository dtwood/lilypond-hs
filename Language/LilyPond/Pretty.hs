{-# LANGUAGE FlexibleInstances #-}

module Language.LilyPond.Pretty where
  import Language.LilyPond.Types
  import Text.Pretty


  instance Pretty PitchClass where
    pretty A = "a"
    pretty B = "b"
    pretty C = "c"
    pretty D = "d"
    pretty E = "e"
    pretty F = "f"
    pretty G = "g"

  instance Pretty Quotes where
    pretty (Quotes q) | q >= 0 = replicate q '\''
                      | q < 0 = replicate (-q) ','

  instance Pretty Pitch where
    pretty (Pitch pc o) = pretty pc ++ pretty o

  instance Pretty Exclamations where
    pretty (Exclamations e) = replicate e '!'

  instance Pretty Questions where
    pretty (Questions q) = replicate q '?'

  instance Pretty OctaveCheck where
    pretty (OctaveCheck q) = "=" ++ pretty q
    pretty (NoOctaveCheck) = ""

  instance Pretty MaybeNotemodeDuration where
    pretty MaybeNotemodeDurationNotImplemented = ""

  instance Pretty OptionalRest where
    pretty OptionalRest = "\\rest"
    pretty NoOptionalRest = ""

  instance Pretty PostEvents where
    pretty PostEventsNotImplemented = ""

  instance Pretty NewChord where
    pretty NewChordNotImplemented = ""

  instance Pretty PitchOrMusic where
    pretty (PitchOrMusicPitch p e q oc mnd or_ pe) = pretty p ++ pretty e ++ pretty q ++ pretty oc ++ pretty mnd ++ pretty or_ ++ pretty pe
    pretty (PitchOrMusicChord nc pe) = pretty nc ++ pretty pe

  instance Pretty [PitchOrMusic] where
    pretty (x:[]) = pretty x
    pretty (x:xs) = pretty x ++ " " ++ pretty xs
