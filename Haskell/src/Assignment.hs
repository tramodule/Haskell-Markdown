
module Assignment (
  markdownParser, 
  peekInput, 
  repTextChar, 
  convertADTHTML, 
  saveHtmlFile, 
  auxConvertADTHTML,
  ADT(..), 
  nestingModifiers, 
  nestedModifiers, 
  manyTill, 
  anyChar, 
  notFollowedByString, 
  followedByString, 
  failParser, 
  notFormattingMarker,
  notEofAndNotNewline, 
  trimFirstEnd, 
  boldCheck, 
  italicCheck, 
  strikeCheck, 
  inlineCode, 
  codeBlock, 
  linkCheck, 
  imageCheck, 
  footNote, 
  footNoteRef, 
  parseHeaderLine, 
  blockQuote, 
  listCheck, 
  unlistCheck, 
  tableHeaderCheck, 
  normalText, 
  parseAnyChar, 
  detectStartEndMod, 
  parseInnerContent, 
  consumeResult, 
  isWrappedByHTML) where


import Control.Applicative (Alternative (many, some, (<|>)), Applicative (liftA2), optional)
import Control.Monad (void, when)
import Data.Maybe (maybeToList)
import Instances (ParseError (..), ParseResult (..), Parser (..), isErrorResult)
import Data.Char (isAlpha, isDigit, isLower, isSpace, isUpper)
import Data.List (intercalate, isInfixOf, isPrefixOf, isSuffixOf)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Debug.Trace (trace)
import Parser
  ( alpha,
    char,
    charTok,
    commaTok,
    digit,
    eof,
    inlineSpace,
    inlineSpace1,
    positiveInt,
    is,
    isNot,
    noneof,
    notEof,
    oneof,
    satisfy,
    sepBy1,
    space,
    spaces,
    spaces1,
    string,
    stringTok,
    tok,
  )

data ADT
  = Bold ADT
  | StartLine ADT ADT
  | Strike ADT
  | Italic ADT
  | Link (ADT, String)
  | Image (String, String, String)
  | FootNote Int
  | Table ([ADT], [[ADT]]) -- (Header, rows)
  | References (Int, String)
  | List [(ADT, ADT)]
  | UList [(ADT, ADT)]
  | Code ADT
  | CodeBlock (String, String)
  | Quote ADT
  | Text String
  | Header Int ADT
  | NewLine ADT ADT
  | HTML ADT ADT
  | Empty
  deriving (Show, Eq)

-- Main function that applies the HTML conversion to a given input string
-- It first parses the input using markdownParser and then handles the result
htmlApply :: String -> String
htmlApply = handleResult . parse markdownParser
  where
    handleResult (Result _ adt) = auxConvertADTHTML adt "Converted HTML" ""
    handleResult (Error err) = "Parse error: " ++ show err

-- Parser function to parse Markdown into an Abstract Data Type (ADT)
-- It trims the input first and then parses it using nestedModifiers
markdownParser :: Parser ADT
markdownParser = Parser $ \input ->
  let modifiedInput = trimFirstEnd $ input
   in parse nestedModifiers modifiedInput

-- Function to get the current time as a formatted string
-- Used to generate a unique filename for saving the HTML file
getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H~%M~%S" <$> getCurrentTime

-- Function to save the HTML content to a file
-- It generates the filename using getTime and saves the HTML in the current directory
saveHtmlFile :: String -> IO ()
saveHtmlFile htmlContent = do
  filePath <- getTime -- Save in the current directory
  writeFile (filePath ++ ".html") htmlContent

-- Helper function to convert the ADT (Abstract Data Type) to HTML format
-- It generates a full HTML document with a specified title and optional CSS
auxConvertADTHTML :: ADT -> String -> String -> String
auxConvertADTHTML adt title css =
  "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n"
    ++ "    <meta charset=\"UTF-8\">\n"
    ++ "    <title>"
    ++ trimFirstEnd title
    ++ "</title>"
    ++ css
    ++ "\n</head>\n\n<body>\n"
    ++ convertADTHTMLHelper 1 adt
    ++ "\n</body>\n\n</html>\n"

--------- HTML Conversion ---------
-- Main function that converts an ADT (Abstract Data Type) to HTML
-- It uses auxConvertADTHTML to create a full HTML document with the ADT content
convertADTHTML :: ADT -> String
convertADTHTML adt = auxConvertADTHTML adt "Test" ""

-- Recursive helper function to convert ADT elements into HTML strings
-- The 'level' parameter is used for indentation, and it processes the ADT recursively
convertADTHTMLHelper :: Int -> ADT -> String
-- Base case: If the ADT is Empty, return an empty string
convertADTHTMLHelper _ Empty = ""

-- Case for handling new lines: Converts the inner data and the continuation to HTML
-- It inserts a newline in the resulting HTML to maintain structure
convertADTHTMLHelper level (NewLine dataInside continue) =
  "\n"
    ++ convertADTHTMLHelper level dataInside
    ++ convertADTHTMLHelper level continue

-- Case for handling the start of a new line of data
-- It checks if the content is already wrapped by HTML elements
-- If it is wrapped, it adds an indented paragraph (<p>) tag, otherwise it just adds the content

convertADTHTMLHelper level (StartLine dataInside continue)
  | dataInside == Empty && continue == Empty = ""  -- Base case for both being empty
  | isWrappedByHTML dataInside =
      indent level
        ++ wrapInP extracted
        ++ convertADTHTMLHelper level continue
  | otherwise =
      indent level
        ++ extracted
        ++ convertADTHTMLHelper level continue
  where
    extracted = convertADTHTMLHelper level dataInside
--------- Recursing the Linked Objects ---------
convertADTHTMLHelper level (HTML first second) =
  convertADTHTMLHelper level first
    ++ convertADTHTMLHelper level second
---------------- String Content ----------------
convertADTHTMLHelper level (Text text) = text
---------------- Bold Content ----------------
convertADTHTMLHelper level (Bold bl) =
  "<strong>"
    ++ convertADTHTMLHelper level bl
    ++ "</strong>"
---------------- Strike Content ----------------
convertADTHTMLHelper level (Strike sk) =
  "<del>"
    ++ convertADTHTMLHelper level sk
    ++ "</del>"
---------------- Italic Content ----------------
convertADTHTMLHelper level (Italic it) =
  "<em>"
    ++ convertADTHTMLHelper level it
    ++ "</em>"
---------------- Code Content ----------------
convertADTHTMLHelper level (Code codes) =
  "<code>"
    ++ convertADTHTMLHelper level codes
    ++ "</code>"
---------------- Link Content ----------------
convertADTHTMLHelper level (Link (textContent, url)) =
  "<a href=\""
    ++ url
    ++ "\">"
    ++ convertADTHTMLHelper level textContent
    ++ "</a>"
---------------- Footnote Content ----------------
convertADTHTMLHelper level (FootNote num) =
  "<sup><a id=\"fn"
    ++ show num
    ++ "ref\" href=\"#fn"
    ++ show num
    ++ "\">"
    ++ show num
    ++ "</a></sup>"
---------------- Image Content --------------------
convertADTHTMLHelper level (Image (altText, url, caption)) =
  "<img src=\""
    ++ url
    ++ "\" alt=\""
    ++ altText
    ++ "\" title=\""
    ++ caption
    ++ "\">"
---------------- Footnote reference Content ------------
convertADTHTMLHelper level (References (num, refText)) =
  "<p id=\"fn"
    ++ show num
    ++ "\">"
    ++ refText
    ++ "</p>"
---------------- Header Content ----------------
convertADTHTMLHelper level (Header hNum text) =
  "<h"
    ++ show hNum
    ++ ">"
    ++ convertADTHTMLHelper level text
    ++ "</h"
    ++ show hNum
    ++ ">"
---------------- Code Block Content ----------------
convertADTHTMLHelper level (CodeBlock (lang, content)) =
    "<pre><code"
    ++ (if null lang then ">" else " class=\"language-" ++ lang ++ "\">")
    ++ content --(intercalate ("\n") (lines content))
    ++ "</code></pre>"
---------------- Table Content ----------------
convertADTHTMLHelper level (Table (header, rows)) =
  "<table>\n"
    ++ indent (level + 1)
    ++ "<tr>\n"
    ++ concatMap (wrapWithTag "th" (level + 1)) header -- Process the header row
    ++ indent (level + 1)
    ++ "</tr>\n"
    ++ concatMap (convertTableRow (level + 1)) rows -- Process all data rows
    ++ indent level
    ++ "</table>"
---------------- List Content ----------------
convertADTHTMLHelper level (List items) =
  "<ol>\n"
    ++ concatMap (convertListItem level) items
    ++ indent level
    ++ "</ol>"
----------- Unordered-List Content -----------
convertADTHTMLHelper level (UList items) =
  "<ul>\n"
    ++ concatMap (convertUnorderedListItem level) items
    ++ indent level
    ++ "</ul>"
---------------- Quote Content ----------------
convertADTHTMLHelper level (Quote quoteContent) =
  "<blockquote>\n"
    ++ convertADTHTMLHelper (level + 1) quoteContent
    ++ "\n"
    ++ indent level
    ++ "</blockquote>"

--------- End of HTML Conversion ---------





--------- HTML Aux Functions ---------
-- Function to generate indentation using a specified level
-- Each level adds 4 spaces of indentation
indent :: Int -> String
indent = (`replicate` ' ') . (* 4)

-- Function to convert a row of table data (ADT elements) into an HTML table row (<tr>)
-- It uses wrapWithTag to wrap each data cell in a <td> tag
convertTableRow :: Int -> [ADT] -> String
convertTableRow level rowData =
  indent level
    ++ "<tr>\n"
    ++ concatMap (wrapWithTag "td" level) rowData
    ++ indent level
    ++ "</tr>\n"

-- Function to wrap ADT content in the specified HTML tag
-- It adds the proper indentation and uses convertADTHTMLHelper to process the content
wrapWithTag :: String -> Int -> ADT -> String
wrapWithTag tag level content =
  indent (level + 1)
    ++ "<"
    ++ tag
    ++ ">"
    ++ convertADTHTMLHelper (level + 2) content
    ++ "</"
    ++ tag
    ++ ">\n"


-- Helper function to convert a list item for unordered list
-- Helper function to convert a single unordered list item to HTML (<li>)
-- It checks if the item has a sublist and processes it recursively if it exists
convertUnorderedListItem :: Int -> (ADT, ADT) -> String
convertUnorderedListItem level (item, sublist) =
  indent (level + 1)
    ++ "<li>"
    ++ convertADTHTMLHelper (level + 1) item
    ++ ( if sublist /= Empty
           then
             "\n"
               ++ indent (level + 2)
               ++ "<ul>\n"
               ++ convertUnorderedListItems (level + 2) sublist
               ++ indent (level + 2)
               ++ "</ul>\n"
               ++ indent (level + 1)
           else ""
       )
    ++ "</li>\n"

-- Helper function to recursively process all unordered list items (converting <li> tags)
-- It handles each item in the unordered list (UList)
convertUnorderedListItems :: Int -> ADT -> String
convertUnorderedListItems level (UList a) = concatMap (convertUnorderedListItem level) a
convertUnorderedListItems _ Empty = ""
convertUnorderedListItems _ _ = ""

-- Helper function to convert a single ordered list item to HTML (<li>)
-- Similar to the unordered list, but wraps the sublist in <ol> instead of <ul>
convertListItem :: Int -> (ADT, ADT) -> String
convertListItem level (item, sublist) =
  indent (level + 1)
    ++ "<li>"
    ++ convertADTHTMLHelper (level + 1) item
    ++ ( if sublist /= Empty
           then
             "\n"
               ++ indent (level + 2)
               ++ "<ol>\n"
               ++ convertListItems (level + 2) sublist
               ++ indent (level + 2)
               ++ "</ol>\n"
               ++ indent (level + 1)
           else ""
       )
    ++ "</li>\n"

-- Helper function to recursively process all ordered list items (converting <li> tags)
-- It handles each item in the ordered list (List)
convertListItems :: Int -> ADT -> String
convertListItems level (List items) = concatMap (convertListItem level) items
convertListItems _ Empty = ""
convertListItems _ _ = ""

-- Helper function to wrap a string of content inside a <p> tag (paragraph)
wrapInP :: String -> String
wrapInP content =
  "<p>"
    ++ content
    ++ "</p>"

-- Function to check if the ADT is already wrapped by an HTML tag
-- Returns True if the ADT is an HTML type or empty, otherwise False
isWrappedByHTML :: ADT -> Bool
isWrappedByHTML (HTML _ _) = True
isWrappedByHTML Empty = True
isWrappedByHTML _ = False
--------- End of HTML Aux Functions ---------






-------------------------------------------------------
-- | The 'manyTill' function runs a parser 'p' zero or more times, stopping when the 'end' parser succeeds.
-- It returns a list of results from running the parser 'p'.
-- This is a useful combinator when you want to collect a sequence of parsed elements up to a terminating condition.
-- 
-- Arguments:
-- p   - The parser to run repeatedly.
-- end - The parser that signals when to stop.
--
-- Returns:
-- A list of parsed elements from 'p', stopping when 'end' succeeds.
manyTill :: Parser a -> Parser end -> Parser [a]
manyTill p end =
  end *> pure []
    <|> ( p >>= \x ->
            manyTill p end >>= \xs ->
              pure (x : xs)
        )

-- | The 'anyChar' parser matches any single character from the input.
-- It succeeds with the character it consumes, or fails if no input is available.
anyChar :: Parser Char
anyChar = satisfy (const True)

-- | The 'notFollowedByString' parser succeeds if the input does not start with the given string 's'.
-- If the input does start with 's', the parser fails with an 'UnexpectedString' error.
-- 
-- Arguments:
-- s - The string that must not be found at the start of the input.
--
-- Returns:
-- The parser succeeds with no result if 's' is not found at the start.
notFollowedByString :: String -> Parser ()
notFollowedByString s = Parser $ \input ->
  if s `isPrefixOf` input
    then Error (UnexpectedString s)
    else Result input ()


-- | The 'followedByString' parser succeeds if the input starts with the given string 's'.
-- It fails if the input does not start with 's', producing an 'UnexpectedString' error.
-- 
-- Arguments:
-- s - The string that must be found at the start of the input.
--
-- Returns:
-- The parser succeeds with no result if 's' is found at the start.
followedByString :: String -> Parser ()
followedByString s = Parser $ \input ->
  if s `isPrefixOf` input
    then Result input ()
    else Error (UnexpectedString s)


-- | The 'peekInput' parser allows you to look at the entire input without consuming any of it.
-- It succeeds with the full input as a result, but leaves the input unchanged for subsequent parsing.
--
-- Returns:
-- The entire input string without consuming it.
peekInput :: Parser String
peekInput = Parser $ \input -> Result input input

-- | The 'failParser' function creates a parser that immediately fails with an 'UnexpectedString' error and the given message.
-- 
-- Arguments:
-- msg - The error message to return when the parser fails.
--
-- Returns:
-- A parser that always fails with the given error message.
failParser :: String -> Parser a
failParser msg = Parser $ \_ -> Error (UnexpectedString msg)


-- | The 'notFormattingMarker' parser checks that none of the given strings appear as prefixes of the input.
-- If any of the strings 'd' are found at the start of the input, it fails with an error message.
-- 
-- Arguments:
-- d - A list of strings that must not appear at the start of the input.
--
-- Returns:
-- The parser succeeds with no result if none of the strings are found.
notFormattingMarker :: [String] -> Parser ()
notFormattingMarker d = do
  input <- peekInput
  if any (`isPrefixOf` input) d
    then failParser "Found formatting marker"
    else pure ()
    
-------------------------------------------------------


-- | The 'nestedModifiers' function is responsible for parsing various Markdown elements, 
--   such as headers, block quotes, code blocks, and more.
--   It uses multiple parsers in sequence to handle different Markdown structures.
nestedModifiers :: Parser ADT
nestedModifiers = do
  -- Attempt to parse the start of various Markdown structures like tables, code blocks, images, footnotes, blockquotes, and headers.
  -- The 'optional' function allows this to either succeed or move on without failing.
  maybeStart <-
    optional
      ( ( inlineSpace -- First, consume any leading inline spaces.
          *> -- Then, attempt to parse different Markdown elements in order of priority.
          ( tableHeaderCheck  -- Check if the input starts with a table header.
              <|> codeBlock   -- Check if it's a code block (using backticks).
              <|> imageCheck  -- Check if it's an image (starts with "![").
              <|> footNoteRef -- Check if it's a footnote reference.
              <|> blockQuote  -- Check if it's a blockquote (starts with ">").
              <|> parseHeaderLine -- Check if it's a header (either hash-based or underlined).
          )
        )
          <|> listCheck -- If none of the above succeed, check for ordered lists (starts with "1.").
          <|> unlistCheck -- Check for unordered lists (starts with "-").
      )

  -- Based on whether we found something in 'maybeStart', handle the parsed result:
  parsedData <- case maybeStart of
    Just parsedSuccess -> return parsedSuccess
    Nothing -> do
      -- Otherwise, continue parsing regular text content until a newline or EOF is encountered.
      content <- many (isNot '\n') -- Parse all characters that are not a newline.
      return $ parseInnerContent content nestingModifiers -- Recursively parse any nested modifiers in the content.

  -- After parsing the main line, attempt to parse any further lines recursively.
  rest <- many $ is '\n' *> nestedModifiers

  -- Combine the parsed content and any following lines using the 'StartLine' constructor, 
  -- with further lines represented as a chain of 'NewLine' elements.
  return $ StartLine parsedData (foldr NewLine Empty rest)


  

-- | 'nestingModifiers' is a parser that handles different inline Markdown elements.
--   It repeatedly applies individual parsers for various formatting options (bold, italic, etc.),
--   collecting them into a list of 'ADT' (Abstract Data Type) elements.
nestingModifiers :: Parser [ADT]
nestingModifiers =
  many -- 'many' indicates that the parsers will be applied zero or more times, collecting the results.
    ( boldCheck        -- Parse for bold text (usually marked by "**").
      <|> italicCheck  -- Parse for italic text (usually marked by "_").
      <|> strikeCheck  -- Parse for strikethrough text (marked by "~~").
      <|> inlineCode   -- Parse inline code (marked by backticks "`").
      <|> footNote     -- Parse for footnotes (e.g., [^1]).
      <|> linkCheck    -- Parse for hyperlinks (e.g., [text](url)).
      <|> normalText   -- Parse for normal text (any text not enclosed by special markers).
      <|> parseAnyChar -- Fallback parser to capture any remaining characters (e.g., punctuation).
    )


--------------------------- Table ----------------------------
-- | The 'tableContentRow' function parses a row of table content in Markdown.
--   It ensures that the row has the correct number of columns and handles nested elements in each cell.
--   The 'columns' parameter is used to ensure that each row has the expected number of columns.
tableContentRow :: Int -> Parser [ADT]
tableContentRow columns = do
  -- Consume a newline character at the beginning of the row (indicating the start of a new row).
  _ <- is '\n'
  -- Consume any leading spaces, followed by a '|' character (indicating the start of the first cell).
  _ <- inlineSpace *> is '|' <* inlineSpace

  -- Parse the content inside each cell of the table.
  -- 'many (noneof "|\n")' parses characters inside a cell until a '|' or newline is encountered.
  -- 'sepBy1' ensures that the content is separated by '|' characters.
  rowContent <-
    (many (noneof "|\n"))
      `sepBy1` (inlineSpace *> is '|' <* inlineSpace <* notEofAndNotNewline)

  _ <- inlineSpace *> is '|' *> inlineSpace *> (followedByString "\n" <|> eof)

  if length rowContent == columns
    then return $
        -- Convert each cell content to an Abstract Data Type (ADT) using 'parseInnerContent'.
        -- 'trimFirstEnd' is used to trim any leading or trailing spaces from the content before parsing.
        (\a -> (parseInnerContent (trimFirstEnd $ a) nestingModifiers))
        <$> rowContent
    else failParser "Invalid column number"


-- | The 'notEofAndNotNewline' parser ensures that the input doesn't contain a newline or reach the end of the file.
--   It is used to prevent parsing table cells that span multiple lines or are incomplete. 
notEofAndNotNewline :: Parser ()
notEofAndNotNewline = Parser $ \input ->
  handleInput input input
  where
    handleInput "" _ = Error UnexpectedEof
    handleInput (c : _) originalInput
      | c == '\n' = Error (UnexpectedChar '\n')
      | otherwise = Result originalInput ()

-- | The 'allSameLengthAndMoreThanThree' function checks whether all strings in the list
--   have a length of at least 3 characters. This is often used for checking table header separators.
--   It ensures that there is a consistent length of at least 3 dashes for all header cells.
allSameLengthAndMoreThanThree :: [String] -> Bool
allSameLengthAndMoreThanThree [] = False -- Handle empty list
allSameLengthAndMoreThanThree (x : xs) = length x >= 3

-- Detecting the Table Header and rest of the table, including separator and at least one row of content
-- Table should at least have one header and separator and at least one row of content - https://edstem.org/au/courses/16784/discussion/2293581
tableHeaderCheck :: Parser ADT
tableHeaderCheck = do
  ---- Header Section ----
  _ <- inlineSpace *> is '|' <* inlineSpace
  headerContent <-
    (many (noneof "|\n"))
      `sepBy1` (inlineSpace *> is '|' <* inlineSpace <* notEofAndNotNewline)
  _ <- inlineSpace *> is '|' *> inlineSpace *> is '\n'

  ---- Separator Section ----
  _ <- inlineSpace *> is '|' <* inlineSpace
  sepCheck <- (some (is '-')) `sepBy1` (inlineSpace *> is '|' <* inlineSpace)
  when (length sepCheck /= length headerContent) (failParser "+- columns")
  when (not $ allSameLengthAndMoreThanThree sepCheck) (failParser "+- columns")
  _ <- inlineSpace *> is '|' *> inlineSpace *> followedByString "\n"

  ---- Row Content Section ----
  columnRowContent <- some (tableContentRow $ length headerContent)
  return $
    Table
      ( (\a -> (parseInnerContent (trimFirstEnd $ a) nestingModifiers))
          <$> headerContent,
        columnRowContent
      )
--------------------------------------------------------------


-- | The 'parseAnyChar' function parses a single character from the input.
--   It returns the parsed character wrapped in the 'Text' ADT (Abstract Data Type).
--   This is a simple parser used as a fallback when no special formatting is detected.
parseAnyChar :: Parser ADT
parseAnyChar =
  anyChar >>= \c ->
    return (Text [c])

-- | The 'normalText' function parses plain text that does not contain any special Markdown formatting markers.
--   It parses multiple characters until it encounters a formatting marker (e.g., bold, italic, code, or link).
--   The parsed text is returned wrapped in the 'Text' ADT.
normalText :: Parser ADT
normalText =
  Text
    <$> some
      ( notFormattingMarker ["**", "_", "~~", "`", "["]
          *> anyChar
      )


--------------------------- List ----------------------------
listCheck :: Parser ADT
listCheck = do
  -- Parse all list items first
  firstData <- parseListItem True
  items <- many (is '\n' *> parseListItem False)
  -- Apply sorting to the entire list after parsing all items
  return $ List ([firstData] <> items)

parseListItem :: Bool -> Parser (ADT, ADT)
parseListItem first = do
  -- Parse the main list item number
  numCheck <- if first
              then is '1' *> pure 1
              else notFollowedBySpace *> notFollowedByString "\n" *> positiveInt

  -- Parse the period
  _ <- is '.'
  -- Parse inline space(s)
  _ <- inlineSpace1
  -- Parse the content line until a newline character
  contentStr <- many (isNot '\n')

  let itemContent = (parseInnerContent contentStr nestingModifiers)

  sublist <-
    ( do
        firstSubItem <- optionalSubList 4 True
        restSubItems <- many (optionalSubList 4 False)
        return (firstSubItem : restSubItems)
    )
      <|> return []

  return (itemContent, if null sublist then Empty else List sublist)


-- Sublist parser (handles indentation of 4 spaces)
optionalSubList :: Int -> Bool -> Parser (ADT, ADT)
optionalSubList leadingSpace first = do
  -- Peek at the next input to check if the next item is a sublist
  input <- peekInput
  if take (leadingSpace + 1) input == ('\n' : replicate leadingSpace ' ') -- Check if the next line starts with 4 spaces (indicating a sublist)
    then do
      -- Consume 4 spaces for sublist indentation
      _ <- is '\n'
      _ <- string (replicate leadingSpace ' ')
      -- Parse sublist item number and content

      numCheck <- if first
            then is '1' *> pure 1
            else notFollowedBySpace *> notFollowedByString "\n" *> positiveInt

      _ <- is '.'
      -- Parse inline space(s)
      _ <- inlineSpace1
      -- Parse the content line until a newline character
      contentStr <- many (isNot '\n')
      nestedSublist <-
        ( do
            firstNestedItem <- (optionalSubList ((+) leadingSpace 4) True)
            restNestedItems <- many (optionalSubList ((+) leadingSpace 4) False)
            return (firstNestedItem : restNestedItems)
        )
          <|> return []

      let sublistContent = (parseInnerContent contentStr nestingModifiers)

      -- Return the sublist as a tuple (number, content, and nested sublist as List or Empty if no sublist)
      return
        ( sublistContent,
          if null nestedSublist
            then Empty
            else List nestedSublist
        )
    else failParser "No sublist found" -- Fail if no sublist is found
--------------------------------------------------------------


----------------------- Unordered-List -----------------------
unlistCheck :: Parser ADT
unlistCheck = do
  -- Parse all list items first
  firstData <- unParseListItem True
  items <- many (is '\n' *> unParseListItem False)
  -- Apply sorting to the entire list after parsing all items
  return $ UList ([firstData] <> items)

unParseListItem :: Bool -> Parser (ADT, ADT)
unParseListItem first = do
  -- Parse the main list item number
  numCheck <- notFollowedBySpace *> notFollowedByString "\n" *> is '-'
  -- Parse inline space(s)
  _ <- inlineSpace1
  -- Parse the content line until a newline character
  contentStr <- many (isNot '\n')
  let itemContent = (parseInnerContent contentStr nestingModifiers)

  -- Recusrively call the subitems function to find the sublists
  sublist <-
    ( do
        firstSubItem <- unOptionalSubList 4 True
        restSubItems <- many (unOptionalSubList 4 False)
        return (firstSubItem : restSubItems)
    )
      <|> return []

  return (itemContent, if null sublist then Empty else UList sublist)

-- Sublist parser (handles indentation of 4 spaces)
unOptionalSubList :: Int -> Bool -> Parser (ADT, ADT)
unOptionalSubList leadingSpace first = do
  -- Peek at the next input to check if the next item is a sublist
  input <- peekInput
  if take (leadingSpace + 1) input == ('\n' : replicate leadingSpace ' ') -- Check if the next line starts with 4 spaces (indicating a sublist)
    then do
      -- Consume 4 spaces for sublist indentation
      _ <- is '\n'
      _ <- string (replicate leadingSpace ' ')
      -- Parse sublist item number and content

      numCheck <- notFollowedBySpace *> notFollowedByString "\n" *> is '-'

      _ <- inlineSpace1
      -- Parse the content line until a newline character
      contentStr <- many (isNot '\n')

      -- Recusrively call the subitems function to find the sublists
      nestedSublist <-
        ( do
            firstNestedItem <- (unOptionalSubList ((+) leadingSpace 4) True)
            restNestedItems <- many (unOptionalSubList ((+) leadingSpace 4) False)
            return (firstNestedItem : restNestedItems)
        )
          <|> return []

      let sublistContent = (parseInnerContent contentStr nestingModifiers)

      -- Return the sublist as a tuple (number, content, and nested sublist as List or Empty if no sublist)
      return
        ( sublistContent,
          if null nestedSublist
            then Empty
            else UList nestedSublist
        )
    else failParser "No sublist found" -- Fail if no sublist is found
--------------------------------------------------------------



-- | The 'inlineCode' parser handles inline code blocks wrapped in single backticks (`).
--   It parses the content between the backticks, allowing for some nested formatting inside.
--   The result is wrapped in the 'Code' ADT (Abstract Data Type).
inlineCode :: Parser ADT
inlineCode =
  -- Inline code with single backticks
  ( Code
      <$> detectStartEndMod
        "`"
        '`'
        1           -- Number of backticks to detect.
        True        -- Disallow newlines within the inline code block.
        ( many      -- Parse the content inside the inline code block, allowing nested elements.
            ( boldCheck       -- Allow bold formatting inside inline code.
                <|> italicCheck -- Allow italic formatting inside inline code.
                <|> strikeCheck -- Allow strike-through formatting inside inline code.
                <|> footNote    -- Allow footnotes inside inline code.
                <|> linkCheck   -- Allow links inside inline code.
                <|> normalText  -- Parse any normal text inside inline code.
                <|> parseAnyChar -- Fallback to parse any character.
            )
        )
  )

-- | The 'boldCheck' parser handles bold text wrapped in double asterisks (**).
--   It parses the content between the double asterisks, allowing for some nested formatting inside.
--   The result is wrapped in the 'Bold' ADT.
boldCheck :: Parser ADT
boldCheck =
  Bold
    <$> detectStartEndMod
      "**"
      '*'
      2            -- Number of asterisks to detect.
      True         -- Disallow newlines within the bold text.
      ( many       -- Parse the content inside the bold block, allowing nested elements.
          ( italicCheck      -- Allow italic formatting inside bold text.
              <|> strikeCheck  -- Allow strike-through formatting inside bold text.
              <|> inlineCode   -- Allow inline code formatting inside bold text.
              <|> footNote     -- Allow footnotes inside bold text.
              <|> linkCheck    -- Allow links inside bold text.
              <|> normalText   -- Parse any normal text inside bold text.
              <|> parseAnyChar -- Fallback to parse any character.
          )
      )

-- | The 'italicCheck' parser handles italic text wrapped in single underscores (_).
--   It parses the content between the underscores, allowing for some nested formatting inside.
--   The result is wrapped in the 'Italic' ADT.
italicCheck :: Parser ADT
italicCheck =
  Italic
    <$> detectStartEndMod
      "_"
      '_'
      1            -- Number of underscores to detect.
      True         -- Disallow newlines within the italic text.
      ( many       -- Parse the content inside the italic block, allowing nested elements.
          ( boldCheck        -- Allow bold formatting inside italic text.
              <|> strikeCheck  -- Allow strike-through formatting inside italic text.
              <|> inlineCode   -- Allow inline code formatting inside italic text.
              <|> footNote     -- Allow footnotes inside italic text.
              <|> linkCheck    -- Allow links inside italic text.
              <|> normalText   -- Parse any normal text inside italic text.
              <|> parseAnyChar -- Fallback to parse any character.
          )
      )

-- | The 'strikeCheck' parser handles strike-through text wrapped in double tildes (~~).
--   It parses the content between the tildes, allowing for some nested formatting inside.
--   The result is wrapped in the 'Strike' ADT.
strikeCheck :: Parser ADT
strikeCheck =
  Strike
    <$> detectStartEndMod
      "~~"
      '~'
      2            -- Number of tildes to detect.
      True         -- Disallow newlines within the strike-through text.
      ( many       -- Parse the content inside the strike-through block, allowing nested elements.
          ( boldCheck        -- Allow bold formatting inside strike-through text.
              <|> italicCheck  -- Allow italic formatting inside strike-through text.
              <|> inlineCode   -- Allow inline code formatting inside strike-through text.
              <|> footNote     -- Allow footnotes inside strike-through text.
              <|> linkCheck    -- Allow links inside strike-through text.
              <|> normalText   -- Parse any normal text inside strike-through text.
              <|> parseAnyChar -- Fallback to parse any character.
          )
      )



constList :: [String]
constList =
  [ "1c",
    "abnf",
    "accesslog",
    "actionscript",
    "ada",
    "apache",
    "applescript",
    "arduino",
    "asciidoc",
    "asm6502",
    "aspnet",
    "autohotkey",
    "autoit",
    "bash",
    "basic",
    "batch",
    "bbcode",
    "brainfuck",
    "c",
    "clojure",
    "cmake",
    "cobol",
    "coffeescript",
    "commonlisp",
    "cpp",
    "crystal",
    "csharp",
    "css",
    "d",
    "dart",
    "diff",
    "django",
    "dockerfile",
    "elixir",
    "erlang",
    "fsharp",
    "glsl",
    "go",
    "graphql",
    "groovy",
    "haskell",
    "haxe",
    "http",
    "ini",
    "java",
    "javascript",
    "json",
    "julia",
    "kotlin",
    "latex",
    "less",
    "lisp",
    "lua",
    "makefile",
    "markdown",
    "matlab",
    "nginx",
    "nim",
    "nix",
    "objectivec",
    "ocaml",
    "pascal",
    "perl",
    "php",
    "plaintext",
    "powershell",
    "prolog",
    "properties",
    "protobuf",
    "python",
    "r",
    "ruby",
    "rust",
    "scala",
    "scheme",
    "scss",
    "shell",
    "sql",
    "swift",
    "typescript",
    "vbnet",
    "verilog",
    "vhdl",
    "vim",
    "vue",
    "xml",
    "xquery",
    "yaml"
  ]

-- | The 'trimFirstEnd' function removes leading and trailing whitespace from a given string.
--   It first removes spaces from the beginning, then reverses the string, removes spaces again,
--   and finally reverses it back to its original order without leading or trailing spaces.
trimFirstEnd :: String -> String
trimFirstEnd = 
  reverse               -- Reverse the string to handle trailing spaces.
    . dropWhile isSpace -- Drop spaces from the end of the string.
    . reverse           -- Reverse the string back to handle leading spaces.
    . dropWhile isSpace -- Drop spaces from the beginning of the string.


------------------------------------------------------------------------
-- | The 'codeBlock' parser handles multi-line code blocks in Markdown-like syntax.
--   These code blocks are enclosed by triple backticks (```).
--   It allows for an optional language identifier after the opening backticks,
--   and then captures all the text content between the opening and closing backticks.


codeBlock :: Parser ADT
codeBlock = do
  -- Parse the opening triple backticks (```), followed by optional spaces.
  _ <- string "```" *> inlineSpace
  
  -- Parse the optional language identifier (e.g., "python", "javascript").
  -- It checks that the identifier is not empty and belongs to a predefined list of valid languages.
  lang <- many (noneof "\n ")
  when (length lang > 0 && lang `notElem` constList) (failParser "Inalid lang")
  _ <- inlineSpace
  _ <- is '\n'

  -- Parse the content of the code block, stopping when a closing triple backticks is found.
  datas <-
    ( manyTill
        anyChar
        ( is '\n'
            -- *> inlineSpace
            *> string "```"
            -- *> inlineSpace
            *> (followedByString "\n" <|> eof)
        )
    )
    <|> ( -- Handle the edge case where the code block is empty.
          -- inlineSpace
          string "```" *> pure "" 
          -- <* inlineSpace
          <* (followedByString "\n" <|> eof)
        )
  return $ CodeBlock $ (lang, datas)
------------------------------------------------------------------------


--------------------------- Header ----------------------------
parseHeaderLine :: Parser ADT
parseHeaderLine = do
  headerHash <|> parseHeaderLineMatch

parseHeaderLineMatch :: Parser ADT
parseHeaderLineMatch = do
  header <- many (isNot '\n') -- Parse the header content until a newline
  -- when (length header <= 0) (failParser "heading is empty")
  _ <- is '\n' *> inlineSpace
  headType <-
    (is '=' *> is '=' *> many (is '=') *> pure 1) -- If line contains '=', return 1 as header type
      <|> (is '-' *> is '-' *> many (is '-') *> pure 2) -- If line contains '-', return 2 as header type
  _ <- inlineSpace *> (followedByString "\n" <|> eof)

  return $ Header headType ((parseInnerContent header nestingModifiers))

headerHash :: Parser ADT
headerHash = do
  hashes <- some (is '#') -- Parse the hash symbols
  _ <- inlineSpace1
  headerText <- many (isNot '\n') -- Parse the header content
  -- when (length headerText <= 0) (failParser "heading is empty")
  if length hashes > 6 -- Check if there are more than 6 hashes
    then failParser "Incorrect amount" -- Return HTML structure with Text for hashes and header content
    else return $ 
         Header (length hashes) (parseInnerContent headerText nestingModifiers) -- Otherwise, return a Header ADT
---------------------------------------------------------------


--------------------------- Quote ----------------------------
quoteString :: Parser String
quoteString = is '\"' *> many (noneof "\"\n") <* is '\"'

-- | The ADT (Abstract Data Type) representing parsed elements.
-- Assuming ADT is defined elsewhere, for example:
-- data ADT = StartLine ADT ADT | NewLine | Empty | ... 

-- | Parser for gathering block quotes in the format:
-- > This is a quote line 1
-- > This is a quote line 2
-- ...
quoteGather :: Parser ADT
quoteGather = do
  firstQuoteLine <- do
    _ <- inlineSpace
    _ <- is '>' *> inlineSpace
    inThisline <- many (isNot '\n')
    return $ parseInnerContent inThisline nestingModifiers
  -- Parse any subsequent lines that are part of the same block quote
  continue <- many $ (is '\n' *> inlineSpace *> followedByString ">") *> quoteGather
  -- Construct the ADT representing the block quote with all its lines
  return $ StartLine firstQuoteLine (foldr NewLine Empty continue)

blockQuote :: Parser ADT
blockQuote = do
  -- Parse the first line of the block quote
  gatheredData <- quoteGather
  -- Convert the parsed lines into ADT as a Quote with NewLine separators
  return $ Quote $ gatheredData
--------------------------------------------------------------


--------------------------- Image ----------------------------
imageCheck :: Parser ADT
imageCheck = do
  -- Ensure there is no whitespace inside the brackets
  _ <- is '!'
  _ <- is '[' *> notFollowedByString "]"
  linkText <- many (noneof "]\n") -- Parse the link text inside the brackets (no spaces)
  _ <- is ']' *> inlineSpace
  -- Ensure there is no whitespace inside the parentheses
  _ <- is '(' *> inlineSpace
  url <- (many (noneof "\n\t\r\f\v ") <* inlineSpace1) -- Parse the URL (until a closing parenthesis)
  caption <- quoteString <* inlineSpace <* is ')'

  _ <- inlineSpace *> (followedByString "\n" <|> eof)
  return $ Image (linkText, url, caption)
--------------------------------------------------------------


--------------------------- Link ----------------------------
linkCheck :: Parser ADT
linkCheck = do
  -- Ensure there is no whitespace inside the brackets
  _ <- is '[' *> notFollowedByString "]"
  linkText <- manyTill (isNot '\n') ( 
                                      is ']' 
                                      *> inlineSpace 
                                      *> is '(' 
                                      *> notFollowedByString ")"
                                    ) -- Parse the link text inside the brackets (no spaces)
  leadingSpace <- inlineSpace
  url <- many (noneof ") \t\r\f\v") -- Parse the URL (until a closing parenthesis)
  trailingSpace <- inlineSpace
  _ <-  is ')'
  return $ Link (
                  (parseInnerContent linkText nestingModifiers), 
                  leadingSpace <> url <> trailingSpace
                )
-------------------------------------------------------------

notFollowedBySpace :: Parser ()
notFollowedBySpace = notFormattingMarker ["\t", "\r", "\f", "\v", " "]



------------------------------ FOOTNOTE ------------------------------

-- | Parser for footnotes in the format [^number]
footNote :: Parser ADT
footNote =
  is '[' >>= \_ ->  -- Parse the opening '[' character.
    is '^' >>= \_ ->  -- Parse the '^' character indicating the start of a footnote.
      notFollowedBySpace >>= \_ ->  -- Ensure that there is no space immediately after '^'.
        notFollowedByString "\n" >>= \_ ->  -- Ensure that the footnote is not followed by a newline.
          positiveInt >>= \n ->  -- Parse a positive integer representing the footnote number and bind it to 'n'.
            is ']' >>= \_ ->  -- Parse the closing ']' character.
              (notFollowedByString ":" <|> eof) >>= \_ ->  -- Ensure that after ']' there is not a ':' or that we've reached the end of the file.
                return $ FootNote n  -- Construct and return the FootNote ADT with the parsed number 'n'.


-- | Parser for footnotes reference in the format [^number]: <text>*
footNoteRef :: Parser ADT
footNoteRef = do
  _ <- is '['
  _ <- is '^' *> notFollowedBySpace *> notFollowedByString "\n"
  num <- positiveInt -- Capture the footnote number
  _ <- is ']'
  _ <- is ':' *> inlineSpace -- Expect a colon
  content <- many (isNot '\n') -- Parse the text content, which may have formatting
  return $ References (num, content) -- combineParsedContent (parseInnerContent content nestingModifiers))
----------------------------------------------------------------------






-------------------------------------------------------
consumeResult :: Parser a -> Int -> Parser a
consumeResult parser drNum = Parser $ \input ->
  case parse parser input of
    Result rem result -> Result (drop drNum rem) (result) -- Discard remaining input by setting it to an empty string
    Error err -> Error err -- If there's an error, propagate it


-- | The detectStartEndMod Parser
-- This parser detects and parses content enclosed by specified start and end markers.
-- It handles nested formatting, ensures proper termination, and optionally checks for newlines within the content.
detectStartEndMod :: String -> Char -> Int -> Bool -> Parser [ADT] -> Parser ADT
detectStartEndMod startMod charMod num nLineCheck adtParser =
  ( do
      -- Parse the opening marker (e.g., "**" for bold)
      _ <- string startMod
      emptyMod <-
        (followedByString startMod 
         *> string startMod <* notFollowedByString [charMod])
          <|> (pure "")

      -- followedByString startMod`**: Look ahead to ensure that another `startMod` follows without consuming it.
      -- *> string startMod: If the above condition is true, consume another `startMod`.
      -- <* notFollowedByString [charMod]`**: Ensure that the second `startMod` is not immediately followed by `charMod`.
      -- <|> pure "": If the above parsing fails, default `emptyMod` to an empty string.

      -- Parse everything until the closing marker, but treat it as potential nested content
      content <- many (notFormattingMarker [startMod] *> anyChar)
      if ('\n' `elem` content) && nLineCheck
        then failParser "Newline detected within content"
        else pure ()

      innerContent <-
        ( do
            strikeRes <- repTextChar charMod num
            return (emptyMod <> content <> strikeRes)
        )
          <|> (string startMod *> return (emptyMod <> content))
      -- After successfully detecting the closing marker, parse the content recursively
      return $ (parseInnerContent innerContent adtParser)
  )


-- Function to parse the inner content recursively using `nestedMod`
-- This will ensure any inner formatting is detected
parseInnerContent :: String -> Parser [ADT] -> ADT
parseInnerContent content adtsParser =
  let parsed = parse (foldr HTML Empty <$> adtsParser) content
  in handleParsed parsed
  where
    -- Very self explanator, unwrapping the context
    handleParsed (Result _ adt) = adt
    handleParsed (Error _) = Text content



-- | The repTextChar Parser
-- This parser consumes a sequence of characters, stopping when it encounters
-- exactly 'num' consecutive occurrences of the character 'a'. It returns the
-- list of consumed characters excluding the terminating 'a's.
repTextChar :: Char -> Int -> Parser [Char]
repTextChar a num =
  consumeResult
    ( some $
        ( do
            -- Peek at the current input to see what characters are ahead.
            input <- peekInput
            -- Count how many consecutive occurrences of the character 'a' are present.
            let deli = takeWhile (== a) input
            let numDeli = length deli
            -- If we find exactly 'num' occurrences of 'a', terminate the parsing process.
            if numDeli == num -- If exactly `num` `a` characters are found, end parsing
              then failParser "End"
              else anyChar -- Otherwise, consume and return the current character.
        )
    )
    num -- Otherwise, parse the character
-------------------------------------------------------
