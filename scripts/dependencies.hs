{-
 - Given a module in (Literate or normal) Cryptol, output a list of
 - its dependencies (`import`s and `include`s), relative to its path.
 - Must be run from `cryptol-course` root folder:
 -
 -     /workspaces/cryptol-course> ghc scripts/dependencies.hs
 -     ...
 -
 -     /workspaces/cryptol-course> scripts/dependencies scripts/Test/Includerific.md
 -     "crypt.txt" "../crypt.txt" "../../scripts/Test2/MondoMod.cry" "../../scripts/Test/Ambig.md"
 -
 -     /workspaces/cryptol-course> scripts/dependencies labs/CryptoProofs/CryptoProofs.md
 -     ""../../specs/Primitive/Symmetric/Cipher/Block/DES.cry"
 -
 -     /workspaces/cryptol-course> scripts/dependencies labs/LoremIpsum/KLI20.cry
 -     "../../labs/CRC/CRCAnswers.md" "../../labs/KeyWrapping/KeyWrappingAnswers.md" "../../labs/Salsa20/Salsa20Answers.md"
 -}

import Control.Monad (filterM)
import Data.List (nub, unwords)
import Data.Text (splitOn, unpack)
import Data.Text.IO (hGetContents)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.FilePath (joinPath, (<.>), (</>))
import System.IO (IOMode (ReadMode), withFile)

import Cryptol.Parser (defaultConfig, parseModule)
import Cryptol.Parser.AST (Import (Import), Module, PName, TopDecl (Include), iModule, mDecls, mImports, mName, thing)
import Cryptol.Parser.Unlit (guessPreProc, unLit)
import Cryptol.Utils.Ident (modNameChunks)

-- extensions recognized for (Literate or normal) Cryptol, in order of recognition
exts :: [[Char]]
exts = [".cry", ".tex", ".markdown", ".md"]

-- get Cryptol module from `path` to parse
getModule :: FilePath -> IO (Module PName)
getModule path = do
  contents <- (withFile path ReadMode hGetContents)
  case (parseModule defaultConfig (unLit (guessPreProc path) contents)) of
    Right pm -> return pm
    Left err -> error (show err)

-- get list of paths to `import`ed modules (assuming resolution in order of `exts`)
importPaths :: Module name -> IO [FilePath]
importPaths m = mapM resolve (mImports m)
  where
    parents = [".." | _ <- init (modNameChunks (thing (mName m)))]
    resolve imp = do
      existFiles <- filterM doesFileExist [joinPath (modNameChunks (iModule (thing imp))) <.> ext | ext <- exts]
      return (joinPath parents </> head existFiles)

-- get list of paths to `include`d files
includePaths :: Module name -> [FilePath]
includePaths m =
  nub (map getPath (filter isInclude (mDecls m)))
  where
    getPath decl =
      thing (case decl of Include x -> x)

    isInclude decl =
      case decl of
        Include _ -> True
        otherwise -> False

-- CLI that returns dependencies of module at path specified by first argument
main :: IO ()
main = do
  args <- getArgs

  paths <-
    let arg = head args
     in do
          m <- getModule arg
          imps <- do
            imps <- importPaths m
            return (nub imps)
          return ((includePaths m) ++ imps)

  putStrLn (unwords (map show paths))
