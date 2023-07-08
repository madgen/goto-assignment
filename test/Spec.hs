import           Prelude hiding (lines, id, lex)
import           Data.Foldable (traverse_)
import           Data.IORef (newIORef, writeIORef, readIORef)
import           Data.List (isSuffixOf, intercalate)
import           Control.Monad (when)
import           System.Directory (listDirectory, doesFileExist)
import           System.FilePath ((</>))
import           System.Exit (exitFailure)
import           AST (Program, pp)
import           Lexer (lex)
import           Parser (parse)
import qualified Span as Span
import           GoToDef (Mode, tabulate, goToDef)
import           Data.String (lines)
import           Data.Algorithm.Diff (getGroupedDiff)
import           Data.Algorithm.DiffOutput (ppDiff)

main :: IO ()
main = do
  failingRef <- newIORef False
  whileFiles <- findWhileFiles "test/cases"
  (`traverse_` whileFiles)
    $ \whileFile -> do
      contents <- readFile whileFile
      let ast = parse $ lex contents
      (`traverse_` [(minBound :: Mode) .. maxBound])
        $ \mode -> do
          let goToDefTbl = GoToDef.tabulate $ GoToDef.goToDef mode ast
          let actualOutput = showTestOutput mode goToDefTbl
          let expPath = mkExpPath whileFile mode
          mExpOutput <- readExpFile expPath
          case mExpOutput of
            Just expOutput -> when (expOutput /= actualOutput)
              $ do
                writeIORef failingRef True
                putStrLn "Mismatch between expected and actual output."
                putStrLn $ showTestCase whileFile ast
                putStrLn $ "Diff (Expected at " <> expPath <> "):"
                let diff =
                      getGroupedDiff (lines expOutput) (lines actualOutput)
                putStrLn $ ppDiff diff
                replaceExp expPath actualOutput
                putStrLn ""
            Nothing        -> do
              writeIORef failingRef True
              putStrLn "There is no expected file."
              putStrLn $ showTestCase whileFile ast
              putStrLn "Actual:"
              putStrLn actualOutput
              replaceExp expPath actualOutput
              putStrLn ""
  failed <- readIORef failingRef
  when failed exitFailure

mkExpPath :: FilePath -> Mode -> FilePath
mkExpPath whilePath mode = whilePath <> "." <> show mode <> ".exp"

readExpFile :: FilePath -> IO (Maybe String)
readExpFile expPath = do
  exists <- doesFileExist expPath
  if exists
    then Just <$> readFile expPath
    else pure Nothing

findWhileFiles :: FilePath -> IO [FilePath]
findWhileFiles dir = do
  files <- listDirectory dir
  let whileFiles = filter (".while" `isSuffixOf`) files
  pure (map (dir </>) whileFiles)

showTestCase :: FilePath -> Program Span.Span -> String
showTestCase path ast = intercalate
  "\n"
  [ "================================================================================"
  , "AST for " <> path
  , "================================================================================"
  , AST.pp ast]

showTestOutput :: Mode -> [(Span.Span, [Span.Span])] -> String
showTestOutput mode goToDefTbl = intercalate "\n"
  $ [ "================================================================================"
    , "Go-to-Def (" <> show mode <> ")"
    , "================================================================================"]
  <> map (\(id, ids) -> Span.pp id <> " -> " <> Span.pps ids) goToDefTbl

replaceExp :: FilePath -> String -> IO ()
replaceExp expPath expContents = do
  putStrLn "Do you want to replace the .exp file? [Y/n]"
  contents <- getLine
  when (contents == "Y") $ writeFile expPath expContents