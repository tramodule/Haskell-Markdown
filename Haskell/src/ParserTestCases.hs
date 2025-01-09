module ParserTestCases (main) where

import Assignment (ADT(..), markdownParser, nestedModifiers, nestingModifiers, manyTill, anyChar, notFollowedByString, followedByString, peekInput, failParser, notFormattingMarker, notEofAndNotNewline, trimFirstEnd, boldCheck, italicCheck, strikeCheck, inlineCode, codeBlock, linkCheck, imageCheck, footNote, footNoteRef, parseHeaderLine, blockQuote, listCheck, unlistCheck, tableHeaderCheck, normalText, parseAnyChar, detectStartEndMod, parseInnerContent, repTextChar, consumeResult, isWrappedByHTML)
import Control.Applicative (Alternative (..))
import Control.Monad (when)
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Data.Time.Clock (UTCTime (..))
import Instances (ParseError (..), ParseResult (..), Parser (..))
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
import System.Exit (exitFailure)

main :: IO ()
main = do
  putStrLn "Running tests..."
  results <- sequence [
      testManyTill,
      testAnyChar,
      testNotFollowedByString,
      testFollowedByString,
      testPeekInput,
      testFailParser,
      testNotFormattingMarker,
      testNotEofAndNotNewline,
      testTrimFirstEnd,
      testMarkdownParser,
      testNestedModifiers,
      testNestingModifiers,
      testBoldCheck,
      testItalicCheck,
      testStrikeCheck,
      testInlineCode,
      testCodeBlock,
      testLinkCheck,
      testImageCheck,
      testFootNote,
      testFootNoteRef,
      testParseHeaderLine,
      testBlockQuote,
      testListCheck,
      testUnlistCheck,
      testTableHeaderCheck,
      testNormalText,
      testParseAnyChar,
      testDetectStartEndMod,
      testParseInnerContent,
      testRepTextChar,
      testConsumeResult,
      testIsWrappedByHTML,
      testHeadingCheck,
      testBlockCodeCheck,
      testParseReferenceLink,
      testInvalidInputs,
      testEdgeCases
    ]
  if and results
    then putStrLn "\ESC[32mAll tests passed!\ESC[0m"
    else do
      putStrLn "\ESC[31mSome tests failed.\ESC[0m"
      exitFailure

-- Helper function to check if parsing was successful
isParseResult :: ParseResult a -> Bool
isParseResult (Result _ _) = True
isParseResult _ = False

-- Helper function to check if parsing resulted in an error
isErrorResult :: ParseResult a -> Bool
isErrorResult (Error _) = True
isErrorResult _ = False

-- Helper function to extract the result from ParseResult
getResult :: ParseResult a -> a
getResult (Result _ a) = a
getResult _ = error "No result"

-- Helper function for assertions with colors and symbols
assert :: Bool -> String -> IO Bool
assert condition message = do
  if condition
    then do
      putStrLn $ "\ESC[32m[✓] " ++ message ++ "\ESC[0m"
      return True
    else do
      putStrLn $ "\ESC[31m[✗] " ++ message ++ "\ESC[0m"
      return False

-- Test cases for manyTill
testManyTill :: IO Bool
testManyTill = do
  let parser = manyTill (is 'a') (is '!')
  let result = parse parser "aaaa!"
  test1 <- assert (result == Result "" "aaaa") "testManyTill: parsing 'aaaa!'"
  return test1

-- Test cases for anyChar
testAnyChar :: IO Bool
testAnyChar = do
  let result1 = parse anyChar "a"
  let result2 = parse anyChar ""
  test1 <- assert (result1 == Result "" 'a') "testAnyChar: parsing 'a'"
  test2 <- assert (isErrorResult result2) "testAnyChar: empty input failed as expected"
  return (test1 && test2)

-- Test cases for notFollowedByString
testNotFollowedByString :: IO Bool
testNotFollowedByString = do
  let parser = notFollowedByString "test"
  let result1 = parse parser "hello"
  let result2 = parse parser "test123"
  test1 <- assert (result1 == Result "hello" ()) "testNotFollowedByString: 'hello'"
  test2 <- assert (isErrorResult result2) "testNotFollowedByString: 'test123' failed as expected"
  return (test1 && test2)

-- Test cases for followedByString
testFollowedByString :: IO Bool
testFollowedByString = do
  let parser = followedByString "hello"
  let result1 = parse parser "hello world"
  let result2 = parse parser "world"
  test1 <- assert (result1 == Result "hello world" ()) "testFollowedByString: 'hello world'"
  test2 <- assert (isErrorResult result2) "testFollowedByString: 'world' failed as expected"
  return (test1 && test2)

-- Test cases for peekInput
testPeekInput :: IO Bool
testPeekInput = do
  let result1 = parse peekInput "hello"
  let result2 = parse peekInput ""
  test1 <- assert (result1 == Result "hello" "hello") "testPeekInput: 'hello'"
  test2 <- assert (result2 == Result "" "") "testPeekInput: empty input"
  return (test1 && test2)

-- Test cases for failParser
testFailParser :: IO Bool
testFailParser = do
  let parser = failParser "Error"
  let result = parse parser "input"
  test1 <- assert (isErrorResult result) "testFailParser: 'input' failed as expected"
  return test1

-- Test cases for notFormattingMarker
testNotFormattingMarker :: IO Bool
testNotFormattingMarker = do
  let parser = notFormattingMarker ["**", "_", "~~"]
  let result1 = parse parser "hello"
  let result2 = parse parser "**bold**"
  test1 <- assert (result1 == Result "hello" ()) "testNotFormattingMarker: 'hello'"
  test2 <- assert (isErrorResult result2) "testNotFormattingMarker: '**bold**' failed as expected"
  return (test1 && test2)

-- Test cases for notEofAndNotNewline
testNotEofAndNotNewline :: IO Bool
testNotEofAndNotNewline = do
  let result1 = parse notEofAndNotNewline "text"
  let result2 = parse notEofAndNotNewline "\n"
  let result3 = parse notEofAndNotNewline ""
  test1 <- assert (result1 == Result "text" ()) "testNotEofAndNotNewline: 'text'"
  test2 <- assert (isErrorResult result2) "testNotEofAndNotNewline: newline failed as expected"
  test3 <- assert (isErrorResult result3) "testNotEofAndNotNewline: empty input failed as expected"
  return (test1 && test2 && test3)

-- Test cases for trimFirstEnd
testTrimFirstEnd :: IO Bool
testTrimFirstEnd = do
  let result1 = trimFirstEnd "  hello world  " == "hello world"
  let result2 = trimFirstEnd "" == ""
  let result3 = trimFirstEnd "    " == ""
  test1 <- assert result1 "testTrimFirstEnd: '  hello world  '"
  test2 <- assert result2 "testTrimFirstEnd: empty input"
  test3 <- assert result3 "testTrimFirstEnd: spaces only"
  return (test1 && test2 && test3)

-- Test cases for markdownParser
testMarkdownParser :: IO Bool
testMarkdownParser = do
  let input = "This is a simple paragraph."
  let expected = Result "" (StartLine (HTML (Text "This is a simple paragraph.") Empty) Empty)
  let result = parse markdownParser input
  test1 <- assert (result == expected) "testMarkdownParser: simple text"
  return test1

-- Test cases for nestedModifiers
testNestedModifiers :: IO Bool
testNestedModifiers = do
  let input = "**bold _italic_** `code`"
  let result = parse nestedModifiers input
  test1 <- assert (isParseResult result) "testNestedModifiers: mixed markdown elements"
  return test1

-- Test cases for nestingModifiers
testNestingModifiers :: IO Bool
testNestingModifiers = do
  let input = "**bold** and _italic_"
  let result = parse nestingModifiers input
  test1 <- assert (isParseResult result) "testNestingModifiers: '**bold** and _italic_'"
  return test1

-- Test cases for boldCheck
testBoldCheck :: IO Bool
testBoldCheck = do
  let input = "**bold text**"
  let expected = Result "" (Bold (HTML (Text "bold text") Empty))
  let result = parse boldCheck input
  test1 <- assert (result == expected) "testBoldCheck: '**bold text**'"
  return test1

-- Test cases for italicCheck
testItalicCheck :: IO Bool
testItalicCheck = do
  let input = "_italic text_"
  let expected = Result "" (Italic (HTML (Text "italic text") Empty))
  let result = parse italicCheck input
  test1 <- assert (result == expected) "testItalicCheck: '_italic text_'"
  return test1

-- Test cases for strikeCheck
testStrikeCheck :: IO Bool
testStrikeCheck = do
  let input = "~~strikethrough~~"
  let expected = Result "" (Strike (HTML (Text "strikethrough") Empty))
  let result = parse strikeCheck input
  test1 <- assert (result == expected) "testStrikeCheck: '~~strikethrough~~'"
  return test1

-- Test cases for inlineCode
testInlineCode :: IO Bool
testInlineCode = do
  let input = "`code`"
  let expected = Result "" (Code (HTML (Text "code") Empty))
  let result = parse inlineCode input
  test1 <- assert (result == expected) "testInlineCode: '`code`'"
  return test1

-- Test cases for codeBlock
testCodeBlock :: IO Bool
testCodeBlock = do
  let input = "```\ncode block content\n```"
  let expected = Result "" (CodeBlock ("","code block content"))
  let result = parse codeBlock input
  test1 <- assert (result == expected) "testCodeBlock: code block without language"
  return test1

-- Test cases for linkCheck
testLinkCheck :: IO Bool
testLinkCheck = do
  let input = "[Example](https://example.com)"
  let expected = Result "" (Link (HTML (Text "Example") Empty, "https://example.com"))
  let result = parse linkCheck input
  test1 <- assert (result == expected) "testLinkCheck: '[Example](https://example.com)'"
  return test1

-- Test cases for imageCheck
testImageCheck :: IO Bool
testImageCheck = do
  let input = "![Alt Text](https://example.com/image.png \"Image Title\")"
  let expected = Result "" (Image ("Alt Text", "https://example.com/image.png", "Image Title"))
  let result = parse imageCheck input
  test1 <- assert (result == expected) "testImageCheck: image with title"
  return test1

-- Test cases for footNote
testFootNote :: IO Bool
testFootNote = do
  let input = "[^1]"
  let expected = Result "" (FootNote 1)
  let result = parse footNote input
  test1 <- assert (result == expected) "testFootNote: '[^1]'"
  return test1

-- Test cases for footNoteRef
testFootNoteRef :: IO Bool
testFootNoteRef = do
  let input = "[^1]: This is a footnote reference."
  let expected = Result "" (References (1, "This is a footnote reference."))
  let result = parse footNoteRef input
  test1 <- assert (result == expected) "testFootNoteRef: footnote definition"
  return test1

-- Test cases for parseHeaderLine
testParseHeaderLine :: IO Bool
testParseHeaderLine = do
  let input = "# Header 1"
  let expected = Result "" (Header 1 (HTML (Text "Header 1") Empty))
  let result = parse parseHeaderLine input
  test1 <- assert (result == expected) "testParseHeaderLine: '# Header 1'"
  return test1

-- Test cases for blockQuote
testBlockQuote :: IO Bool
testBlockQuote = do
  let input = "> This is a block quote."
  let result = parse blockQuote input
  test1 <- assert (isParseResult result) "testBlockQuote: simple block quote"
  return test1

-- Test cases for listCheck
testListCheck :: IO Bool
testListCheck = do
  let input = "1. First item\n2. Second item"
  let result = parse listCheck input
  test1 <- assert (isParseResult result) "testListCheck: ordered list"
  return test1

-- Test cases for unlistCheck
testUnlistCheck :: IO Bool
testUnlistCheck = do
  let input = "- First item\n- Second item"
  let result = parse unlistCheck input
  test1 <- assert (isParseResult result) "testUnlistCheck: unordered list"
  return test1

-- Test cases for tableHeaderCheck
testTableHeaderCheck :: IO Bool
testTableHeaderCheck = do
  let input = "| Header1 | Header2 |\n| ------- | ------- |\n| Cell1   | Cell2   |"
  let result = parse tableHeaderCheck input
  test1 <- assert (isParseResult result) "testTableHeaderCheck: valid table"
  return test1

-- Test cases for normalText
testNormalText :: IO Bool
testNormalText = do
  let input = "Just some normal text."
  let expected = Result "" (Text "Just some normal text.")
  let result = parse normalText input
  test1 <- assert (result == expected) "testNormalText: normal text"
  return test1

-- Test cases for parseAnyChar
testParseAnyChar :: IO Bool
testParseAnyChar = do
  let input = "a"
  let expected = Result "" (Text "a")
  let result = parse parseAnyChar input
  test1 <- assert (result == expected) "testParseAnyChar: 'a'"
  return test1

-- Test cases for detectStartEndMod
testDetectStartEndMod :: IO Bool
testDetectStartEndMod = do
  let parser = detectStartEndMod "**" '*' 2 True (many normalText)
  let input = "**bold**"
  let result = parse parser input
  test1 <- assert (isParseResult result) "testDetectStartEndMod: '**bold**'"
  return test1

-- Test cases for parseInnerContent
testParseInnerContent :: IO Bool
testParseInnerContent = do
  let content = "This is **bold _and italic_** text."
  let adt = parseInnerContent content nestingModifiers
  let test1 = case adt of
        HTML (Text "This is ") (HTML (Bold boldContent) (HTML (Text " text.") Empty)) ->
          case boldContent of
            HTML (Text "bold ") (HTML (Italic (HTML (Text "and italic") Empty)) Empty) -> True
            _ -> False
        _ -> False
  assert test1 "testParseInnerContent: nested formatting"

-- Test cases for repTextChar
testRepTextChar :: IO Bool
testRepTextChar = do
  let parser = (repTextChar '*' 2)
  let input = "text**"
  let result = parse parser input
  test1 <- assert (result == Result "" "text") "testRepTextChar: 'text**'"
  return test1

-- Test cases for consumeResult
testConsumeResult :: IO Bool
testConsumeResult = do
  let parser = consumeResult (string "hello") 2
  let input = "hello world"
  let expected = Result "orld" "hello"
  let result = parse parser input
  test1 <- assert (result == expected) "testConsumeResult: 'hello world'"
  return test1

-- Test cases for isWrappedByHTML
testIsWrappedByHTML :: IO Bool
testIsWrappedByHTML = do
  let test1 = isWrappedByHTML (HTML Empty Empty)
  let test2 = not (isWrappedByHTML (Text "content"))
  let test3 = isWrappedByHTML Empty
  let condition = test1 && test2 && test3
  assert condition "testIsWrappedByHTML: various inputs"

-- Test cases for headingCheck (Assuming you have a function to parse headings)
testHeadingCheck :: IO Bool
testHeadingCheck = do
  let input1 = "### Heading Level 3"
  let expected1 = Result "" (Header 3 (HTML (Text "Heading Level 3") Empty))
  let result1 = parse parseHeaderLine input1
  test1 <- assert (result1 == expected1) "testHeadingCheck: '### Heading Level 3'"

  let input2 = "####### Invalid Heading"
  let result2 = parse parseHeaderLine input2
  test2 <- assert (isErrorResult result2) "testHeadingCheck: '####### Invalid Heading' failed as expected"

  return (test1 && test2)

-- Test cases for blockCodeCheck (Assuming you have a function to parse code blocks with language)
testBlockCodeCheck :: IO Bool
testBlockCodeCheck = do
  let input = "```haskell\nmain = putStrLn \"Hello, World!\"\n```"
  let expected = Result "" (CodeBlock ("haskell", "main = putStrLn \"Hello, World!\""))
  let result = parse codeBlock input
  test1 <- assert (result == expected) "testBlockCodeCheck: code block with language"
  return test1

-- Test cases for parseReferenceLink (Assuming you have a function to parse reference links)
testParseReferenceLink :: IO Bool
testParseReferenceLink = do
  let input = "[Example](1)"
  let expected = Result "" (Link (HTML (Text "Example") Empty, "1"))
  let result = parse linkCheck input
  test1 <- assert (result == expected) "testParseReferenceLink: '[Example](1)'"

  let input2 = "[Invalid][NotExist]"
  let result2 = parse linkCheck input2
  test2 <- assert (isErrorResult result2) "testParseReferenceLink: '[Invalid][NotExist]' failed as expected"


  return (test1 && test2)

-- Test cases for invalid inputs
testInvalidInputs :: IO Bool
testInvalidInputs = do
  -- Invalid bold syntax
  let input1 = "**bold text"
  let result1 = parse boldCheck input1
  test1 <- assert (isErrorResult result1) "testInvalidInputs: incomplete bold syntax failed as expected"

  -- Invalid link syntax
  let input2 = "[Example](invalid url)"
  let result2 = parse linkCheck input2
  test2 <- assert (isErrorResult result2) "testInvalidInputs: invalid link syntax failed as expected"

  return (test1 && test2)

-- Test cases for edge cases
testEdgeCases :: IO Bool
testEdgeCases = do
  -- Empty input for parsers
  let result1 = parse markdownParser ""
  test1 <- assert (result1 == Result "" (StartLine Empty Empty)) "testEdgeCases: empty input for markdownParser"

  -- Whitespace input
  let result2 = parse markdownParser "   "
  test2 <- assert (result2 == Result "" (StartLine Empty Empty)) "testEdgeCases: whitespace input for markdownParser"

  -- Nested formatting edge case
  let input3 = "***bold and italic***"
  let result3 = parse nestedModifiers input3
  test3 <- assert (isParseResult result3) "testEdgeCases: triple asterisks for bold and italic"

  -- Overlapping modifiers
  let input4 = "**bold _italic** text_"
  let result4 = parse nestedModifiers input4
  test4 <- assert (result4 == Result "" (StartLine (HTML (Bold (HTML (Text "bold ") (HTML (Text "_") (HTML (Text "italic") Empty)))) (HTML (Text " text") (HTML (Text "_") Empty))) Empty)) "testEdgeCases: overlapping modifiers failed as expected"

  return (test1 && test2 && test3 && test4)
