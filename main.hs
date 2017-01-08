import Language.LilyPond.Pretty
import Language.LilyPond.Parser
import Text.Parsec
import Data.Either.Unwrap

main :: IO ()
main = do
    let fileName = "clarinet-1.ly"
    file <- readFile fileName
    let result = fromRight $ Text.Parsec.parse lilyPondParser fileName file
    putStrLn $ pretty result
    putStrLn $ show result
