import BasicPrelude hiding (getArgs)
import System.Environment (getArgs)
import Language.Haskell.Parser
import Language.Haskell.Syntax
import Language.Haskell.Pretty (prettyPrint)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T


main :: IO ()
main = do
    args <- getArgs

    let (input, output) = case args of
                               []         -> (TIO.getContents, TIO.putStr)
                               ["--help"] -> error "Usage: format-hs [FILE]"
                               [fp]       -> (TIO.readFile fp, TIO.writeFile fp)
                               _          -> error "Invalid arguments"

    contents <- input

    case parseModule ("x = " ++ T.unpack contents) of
         ParseOk mod -> output . displayExp $ unpackModule mod
         ParseFailed loc msg -> error $ "Parse error at " ++ show loc ++ ": " ++ msg

-- Extracts the relevant part of the AST
unpackModule :: HsModule -> HsExp
unpackModule mod = res where
    HsModule _ _ _ _ [dec] = mod
    HsPatBind _ _ (HsUnGuardedRhs res) [] = dec

-- Pretty prints the expression
displayExp :: HsExp -> Text
displayExp = T.pack . (++ "\n") . prettyPrint

