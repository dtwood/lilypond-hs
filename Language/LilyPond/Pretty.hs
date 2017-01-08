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

  instance Pretty Octave where
    pretty (Octave o) | o == 0 = ""
                    | o > 0 = replicate o '\''
                    | o < 0 = replicate (-o) ','

  instance Pretty Pitch where
    pretty (Pitch pc o) = (pretty pc) ++ (pretty o)
