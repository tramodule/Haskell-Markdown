import           Assignment           (convertADTHTML, markdownParser)
import           Control.Exception    (IOException, try)
import           Control.Monad        (mapM_, (>>), (>>=))
import           Data.List            (isPrefixOf)
import           Instances            (ParseResult (Error, Result), parse)
import           System.FilePath      (joinPath, replaceExtension,
                                       splitDirectories, splitFileName,
                                       takeFileName, (</>))
import ComprehensiveTests             (runComprehensiveTests, allTests)
import           System.Directory     (createDirectoryIfMissing)
import           System.FilePath.Glob (glob)
import           System.IO            (IOMode (WriteMode), withFile)
import           System.IO.Error      (isDoesNotExistError)
import           System.Process       (StdStream (UseHandle), createProcess,
                                       proc, std_out, waitForProcess)

-- Function to replace a substring with another substring in a string
replaceSubstring :: String -> String -> String -> String
replaceSubstring _ _ [] = []
replaceSubstring from to str@(x : xs)
  | from `isPrefixOf` str = to ++ replaceSubstring from to (drop (length from) str)
  | otherwise = x : replaceSubstring from to xs

processFile :: FilePath -> FilePath -> IO ()
processFile inputFile outputFile  = do
  putStrLn $ "Starting " ++ inputFile
  result <- try (readFile inputFile) :: IO (Either IOException String)
  case result of
    Left e
      | isDoesNotExistError e -> putStrLn $ "File not found: " ++ inputFile
      | otherwise -> putStrLn $ "Error reading file " ++ inputFile ++ ": " ++ show e
    Right contents -> do
      let parsed = parse markdownParser contents
      case parsed of
        Result _ adt -> do
          let prettyOutput = convertADTHTML adt
          writeFile outputFile prettyOutput
          putStrLn $ "Processed: " ++ inputFile
        _ -> putStrLn $ "Invalid ParseResult for file " ++ inputFile

mkFolderIfNotExists :: FilePath -> IO ()
mkFolderIfNotExists = createDirectoryIfMissing True


gitDiffNoIndex :: FilePath -> FilePath -> IO ()
gitDiffNoIndex file1 file2 = do
    mkFolderIfNotExists "./examples/diff_output"
    let outputFile = genOutputName "diff_output" "diff" file1

    withFile outputFile WriteMode $ \handle -> do
      -- Create a process for git diff --no-index
      (_, _, _, processHandle) <- createProcess (proc "git" ["--no-pager", "diff", "--no-index", "--no-ext-diff", file1, file2])
                                                   { std_out = UseHandle handle }


      -- Wait for the process to complete but ignore the exit code
      _ <- waitForProcess processHandle
      return ()

genOutputName :: FilePath -> String -> String -> FilePath
genOutputName outputFolder extension file = replaceExtension (replaceFolderAtIndex file 1 outputFolder) extension

replaceFolderAtIndex :: FilePath -> Int -> String -> FilePath
replaceFolderAtIndex filePath index newFolder = joinPath (replaceAtIndex index newFolder (splitDirectories dirPath)) </> fileName
  where
    -- Split the file path into directory and file components
    dirPath = fst $ splitFileName filePath
    fileName = takeFileName filePath

    -- Replace the folder at the specified index using zipWith
    replaceAtIndex :: Int -> String -> [FilePath] -> [FilePath]
    replaceAtIndex idx newVal = zipWith (\i folder -> if i == idx then newVal else folder) [0..]

checkADTShow :: IO ()
checkADTShow = do
  let markdownStr = "test **bold**"
  putStrLn "Testing ADT's Show instance:"
  putStrLn $ "Input string: " ++ markdownStr
  putStrLn $ case parse markdownParser markdownStr of
    Error _      -> "Parse error"
    Result _ adt -> "Result: " ++ show adt

main :: IO ()
main = do
  checkADTShow

  putStrLn ""
  putStrLn "Testing example inputs:"
  files <- glob "examples/input/*.md"
  let outputFiles = map (genOutputName "output" "html") files
  let expectedOutput = map (genOutputName "expected_output" "html") files

  mapM_ (uncurry processFile) (zip files outputFiles)
  mapM_ (uncurry gitDiffNoIndex) (zip expectedOutput outputFiles)

  putStrLn "\nRunning Comprehensive Tests..."
  allTests