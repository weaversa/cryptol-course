import Control.Monad (forM_, when)
import Data.List (partition, unzip)
import Data.Text (Text, pack, splitOn, unlines)
import Data.Text.IO (hGetContents, writeFile)
import System.Environment (getArgs)
import System.FilePath ((<.>))
import System.IO (IOMode (ReadMode, WriteMode), withFile)

import CMarkGFM (NodeType (CODE_BLOCK), CMarkExtension, CMarkOption, Info, Node, Node (Node), commonmarkToNode)
import Text.Regex.TDFA


xCryptolSessionBlocks :: Node -> [Text]
xCryptolSessionBlocks (Node _ nodeType children) =
    case nodeType of
        CODE_BLOCK info text ->
            (if ((pack "Xcryptol session") == info) then [text] else [])
            ++ childBlocks
        otherwise -> childBlocks
  where
    childBlocks = concat (map xCryptolSessionBlocks children)

data SnippetLine =
    Command Text
  | Expected Text Bool

lineSnippet :: Text -> SnippetLine
lineSnippet line =
    if isExpected then Expected line isStdOut else Command (head groups)
  where
    (_, _, _, groups) = line =~ "^[A-Za-z0-9_:]+> ?(.*)$" :: (Text, Text, Text, [Text])
    isExpected = groups == []
    isStdOut = isExpected && (line =~ "^Loading module .*$|^Counterexample$|^Q.E.D.$|^Satisfiable$|^Unsatisfiable$" :: Bool)

blockSnippets :: Text -> ([SnippetLine], [SnippetLine])
blockSnippets block =
    partition isCommand (map lineSnippet (splitOn (pack "\n") block))
  where
    isCommand line =
        case line of
            Command _ -> True
            Expected _ _ -> False


extractSnippets :: [CMarkOption] -> [CMarkExtension] -> FilePath -> IO ()
extractSnippets opts exts path = do
    contents <- withFile path ReadMode hGetContents ;
    let
        markdown = commonmarkToNode opts exts contents
        sessionBlocks = xCryptolSessionBlocks markdown
        hasSessions = sessionBlocks /= []
        (commandBlocks, expectedBlocks) = unzip (map blockSnippets (sessionBlocks))
        commands = concat commandBlocks
        expected = concat expectedBlocks
        stdout = filter isStdOut expected
      in when hasSessions (do
          writeSnippets (path <.> "icry")          commands;
          writeSnippets (path <.> "icry.expected") expected;
          writeSnippets (path <.> "icry.stdout")   stdout
      )
  where
    snippetText snippetLine =
        case snippetLine of
            Command c -> c
            Expected x _ -> x

    isStdOut snippetLine =
        case snippetLine of
            Command _ -> False
            Expected _ b -> b
    
    writeSnippets path = Data.Text.IO.writeFile path . Data.Text.unlines . map snippetText
    
main :: IO ()
main = do
    args <- getArgs
    forM_ args $ \arg -> extractSnippets [] [] arg
