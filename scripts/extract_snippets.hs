import Control.Monad (forM_, when)
import Data.List (partition, unzip)
import Data.Text (Text, pack, splitOn, unlines)
import Data.Text.IO (hGetContents, writeFile)
import System.Console.GetOpt (ArgDescr (NoArg, OptArg), ArgOrder(RequireOrder), OptDescr (Option), getOpt, usageInfo)
import System.Environment (getArgs)
import System.Environment.Blank (getProgName)
import System.Exit (ExitCode (ExitSuccess), exitWith)
import System.FilePath (takeDirectory, (</>), (<.>))
import System.IO (IOMode (ReadMode, WriteMode), stderr, withFile)

import CMarkGFM (CMarkExtension, CMarkOption, Info, Node (Node), NodeType (CODE_BLOCK), commonmarkToNode)
import Text.Regex.TDFA ((=~))
import System.Directory (createDirectoryIfMissing)


-- | Extract ```Xcryptol-session code fences from Markdown
xCryptolSessionBlocks :: Node -> [Text]
xCryptolSessionBlocks (Node _ nodeType children) =
    case nodeType of
        CODE_BLOCK info text ->
            (if ((pack "Xcryptol-session") == info) then [text] else [])
            ++ childBlocks
        otherwise -> childBlocks
  where
    childBlocks = concat (map xCryptolSessionBlocks children)

-- | Lines from interactive/batch mode are commands (input) or responses (output)
data SnippetLine =
    -- | Command passed to interpreter, e.g. `:prove myProp` or `:s ascii=on`
    Command Text
    -- | Expected response e.g. status (`Q.E.D.`, `Counterexample`) 
    -- | or evaluation output (`"HELLO"`, `ascii = on`)
    -- | 
    -- | `Bool` field is `True` if response is a test runner status, 
    -- | one of:
    -- | `Q.E.D.` or `Counterexample` (`:prove` response) or
    -- | `Satisfiable` or `Unsatisfiable` (`:sat` response) or
    -- | `Loading ...` (`:m` response)
  | Response Text Bool

-- | Parse a line from an interactive/batch cryptol-session snippet
-- | Lines beginning with alphanumeric characters delimited by `::` 
-- | and followed by `>` with no spaces are probably prompt commands.
lineSnippet :: Text -> SnippetLine
lineSnippet line =
    if isResponse then Response line isStdOut else Command (head groups)
  where
    (_, _, _, groups) = line =~ "^[A-Za-z0-9_:]+> ?(.*)$" :: (Text, Text, Text, [Text])
    isResponse = groups == []
    isStdOut = isResponse && (line =~ "^(Loading .*)|(Counterexample)|(Q\\.E\\.D\\.)|(Satisfiable)|(Unsatisfiable)$" :: Bool)

-- | Partition a snippet block into (Commands, Responses)
-- | 
-- | Commands are for an `.icry` file to be run via `cryptol -b`
-- | Responses are for an `.icry.stdout` file for test runners to 
-- | compare against actual output.
blockSnippets :: Text -> ([SnippetLine], [SnippetLine])
blockSnippets block =
    partition isCommand (map lineSnippet (splitOn (pack "\n") block))
  where
    isCommand line =
        case line of
            Command _ -> True
            Response _ _ -> False

-- | Command Line Interface (CLI) Options
data Options = Options 
    { optVerbose    :: Bool    -- ^ Log stages of snippet extraction to stdOut
    , optOutput     :: String  -- ^ Write extracted .icry and .icry.stdout
    }

-- | Default values for CLI options
defaults :: Options
defaults = Options
    { optVerbose    = False
    , optOutput     = ""
    }

-- | Extract snippets from the specified file `path` using CLI `opts`
extractSnippets :: Options -> FilePath -> IO ()
extractSnippets opts path = do
    when verbose (putStrLn ("Extracting snippets from " ++ path)) ;
    contents <- withFile path ReadMode hGetContents ;
    let
        markdown = commonmarkToNode [] [] contents
        sessionBlocks = xCryptolSessionBlocks markdown
        hasSessions = sessionBlocks /= []
        (commandBlocks, expectedBlocks) = unzip (map blockSnippets sessionBlocks)
        commands = concat commandBlocks
        responses = concat expectedBlocks
        stdout = filter isStdOut responses
      in when hasSessions ( do
          logSnippets "commands"        "icry"          commands ;
          logSnippets "expected output" "icry.expected" responses ;
          logSnippets "batch output"    "icry.stdout"   stdout
      )
  where
    Options { optVerbose = verbose
            , optOutput = output
            } = opts

    snippetText snippetLine =
        case snippetLine of
            Command c -> c
            Response x _ -> x

    isStdOut snippetLine =
        case snippetLine of
            Command _ -> False
            Response _ b -> b

    logSnippets label ext commands = do
        when verbose ( putStrLn ("Writing " ++ label ++ " to " ++ outPathExt ++ " ..." ))
        createDirectoryIfMissing True (takeDirectory outPathExt)
        writeSnippets outPathExt commands
      where
        outPathExt = output </> path <.> ext

    writeSnippets outPathExt =
        Data.Text.IO.writeFile outPathExt . Data.Text.unlines . map snippetText

-- | Process CLI options, exiting if help is requested
options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "r" ["output"]
        (OptArg
            (\arg opt ->
                return opt {
                    optOutput = case arg of
                        Just x -> x
                        Nothing -> optOutput opt
                }
            )
            "DIR")
        "base directory for extracted snippets"

    , Option "v" ["verbose"]
        (NoArg
            (\opt -> return opt { optVerbose = True }))
        "report stages of snippet extraction"

    , Option "h?" ["help"]
        (NoArg
            (\_ -> do
                prg <- getProgName
                putStrLn (usageInfo prg options)
                exitWith ExitSuccess))
        "usage info"
    ]

main :: IO ()
main = do
    args <- getArgs

    let (actions, nonOptions, errors) = getOpt RequireOrder options args

    opts <- foldl (>>=) (return defaults) actions

    let Options { optVerbose = verbose
                , optOutput  = output
                } = opts
    
    prog <- getProgName

    when verbose (putStrLn ("Running " ++ prog ++ " verbosely " ++ " with output to: " ++ output))

    forM_ nonOptions $ \path -> extractSnippets opts path
