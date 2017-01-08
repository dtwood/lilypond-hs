import Data.Either.Unwrap
import Language.LilyPond.Parser
import Language.LilyPond.Pretty
import Language.LilyPond.Types
import Text.Parsec
import Text.Pretty

main :: IO ()
main = do
    let fileName = "clarinet-1.ly"
    file <- readFile fileName
    let result = fromRight $ Text.Parsec.parse lilyPondParser fileName file
    pPrint result
    print result
