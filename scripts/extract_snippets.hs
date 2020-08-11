import Data.List (partition, unzip)
import Data.Text (Text, pack, splitOn, unlines)
import Data.Text.IO (hGetContents, writeFile)
import System.FilePath ((<.>))
import System.IO (IOMode (ReadMode, WriteMode), withFile)
import Text.Regex.TDFA

import CMarkGFM (NodeType (CODE_BLOCK), CMarkExtension, CMarkOption, Info, Node, Node (Node), commonmarkToNode)


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


readMarkdown :: [CMarkOption] -> [CMarkExtension] -> FilePath -> IO Node
readMarkdown opts exts path = do 
    contents <- (withFile path ReadMode hGetContents)
    return (commonmarkToNode opts exts contents)

goofusMarkdown opts exts path = do
    contents <- withFile path ReadMode hGetContents ;
    let
        markdown = commonmarkToNode opts exts contents
        (commandBlocks, expectedBlocks) = unzip (map blockSnippets (xCryptolSessionBlocks markdown))
        commands = concat commandBlocks
        expected = concat expectedBlocks
        stdout = filter isStdOut expected
      in do
        Data.Text.IO.writeFile (path <.> "icry") (combine commands);
        Data.Text.IO.writeFile (path <.> "icry.expected") (combine expected);
        Data.Text.IO.writeFile (path <.> "icry.stdout") (combine stdout)
  where
    snippetText snippetLine =
        case snippetLine of
            Command c -> c
            Expected x _ -> x

    isStdOut snippetLine =
        case snippetLine of
            Command _ -> False
            Expected _ b -> b
    
    combine = Data.Text.unlines . map snippetText




