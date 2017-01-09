import Language.LilyPond.Parser
import Language.LilyPond.Pretty
import Language.LilyPond.Types
import Text.Parsec
import Text.Pretty

main :: IO ()
main = do
    let fileName = "clarinet-1.ly"
    file <- readFile fileName
    let result = Text.Parsec.parse lilyPondParser fileName file
    case result of
      Left error -> print error
      Right success -> do
        pPrint success
        print success
