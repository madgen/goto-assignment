{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main (main) where

import           Language.LSP.Server
import           Language.LSP.Protocol.Types
import           Language.LSP.Protocol.Message
import           Control.Monad.IO.Class
import qualified System.IO.Strict as SIO
import           Prelude hiding (lex, id)
import           Data.Foldable (traverse_)
import qualified AST
import           Lexer (lex)
import           Parser (parse)
import qualified Index
import qualified Span
import           Target (Target(..))
import           System.Environment (getArgs)
import qualified GoToDef
import           Data.Maybe (fromJust)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["lex"] -> do
      contents <- readProgramSTDIN
      putStrLn "\n\nTokens:"
      print $ lex contents
    ["parse"] -> do
      contents <- readProgramSTDIN
      putStrLn "\n\nPretty printed parse tree:"
      putStrLn $ AST.pp $ parse $ lex contents
    ["goto-def", mode] -> do
      contents <- readProgramSTDIN
      putStrLn "\n\nPretty printed parse tree:"
      let ast = parse $ lex contents
      putStrLn $ AST.pp ast
      putStrLn $ "\n\nGo-to-def (" <> mode <> ") table:"
      traverse_ (\(id, ids) -> putStrLn $ Span.pp id <> " -> " <> Span.pps ids)
        $ GoToDef.tabulate
        $ GoToDef.goToDef (read mode) ast
    ["find-pos", line, column] -> do
      contents <- readProgramSTDIN
      let target = Target { _line = read line, _column = read column }
      let ast = parse $ lex contents
      let index = Index.build ast
      case target `Index.lookup` index of
        Just sp -> putStrLn $ Span.pp sp
        Nothing -> error "couldn't find a span"
    _ -> do
      _exitCode <- runServer serverDef
      error "server died"

readProgramSTDIN :: IO String
readProgramSTDIN = do
  putStrLn "Reading input..."
  SIO.getContents

serverDef :: ServerDefinition ()
serverDef =
  ServerDefinition { onConfigurationChange = const $ const $ Right ()
                   , defaultConfig = ()
                   , doInitialize = \env _req -> pure $ Right env
                   , staticHandlers = handlers
                   , interpretHandler = \env -> Iso (runLspT env) liftIO
                   , options = defaultOptions
                   }
  where
    handlers :: Handlers (LspM ())
    handlers = mconcat
      [ requestHandler SMethod_TextDocumentDefinition
        $ \req responder -> do
          let TRequestMessage
                _
                _
                _
                (DefinitionParams
                   (TextDocumentIdentifier uri)
                   pos
                   _workDone
                   _partialResultToken) = req
          let Position l c = pos
          let path = fromJust $ uriToFilePath uri
          contents <- liftIO $ readFile path
          let target = Target { _line = fromIntegral l + 1
                              , _column = fromIntegral c + 1
                              }
          let ast = parse $ lex contents
          let index = Index.build ast
          let spans = case target `Index.lookup` index of
                Just sp -> do
                  let goToDef = GoToDef.goToDef GoToDef.SSA ast
                  let sps = maybe [] S.toList (sp `M.lookup` goToDef)
                  Location uri . Span.toLSPRange <$> sps
                Nothing -> []
          let response = Definition (InR spans)
          responder (Right $ InL response)]
