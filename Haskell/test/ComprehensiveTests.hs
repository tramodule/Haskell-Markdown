{-# LANGUAGE OverloadedStrings #-}

module ComprehensiveTests (runComprehensiveTests, allTests) where

import Assignment (ADT(..), markdownParser, convertADTHTML)
import Instances (ParseResult(..), parse)
import Parser   (noneof)
import Test.QuickCheck
import Data.List (intercalate, isInfixOf, isPrefixOf, isSuffixOf)
import Data.Char (isAlphaNum, isSpace)
import Control.Monad (replicateM)
import System.Exit (exitFailure)
import Control.Monad (unless)
import Debug.Trace (trace)
import Text.Regex.TDFA ((=~))

-- | Generators for different markdown elements

-- Generator for plain text including special characters
genPlainText :: Gen String
genPlainText = listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ".,!?;{}[('-"

-- Generator for bold text
genBoldText :: Gen String
genBoldText = do
  content <- genPlainText
  return $ "**" ++ content ++ "**"

-- Generator for italic text
genItalicText :: Gen String
genItalicText = do
  content <- genPlainText
  return $ "_" ++ content ++ "_"

-- Generator for strikethrough text
genStrikeText :: Gen String
genStrikeText = do
  content <- genPlainText
  return $ "~~" ++ content ++ "~~"

genInlineCode :: Gen String
genInlineCode = do
  content <- listOf1 $ elements $ filter (`notElem` ['`', '|']) ['\32'..'\126']  -- Printable ASCII characters excluding backtick
  return $ "`" ++ content ++ "`"


-- Generator for headers
genHeader :: Gen String
genHeader = do
  level <- choose (1, 6) :: Gen Int
  content <- genPlainText
  return $ replicate level '#' ++ " " ++ content

-- Generator for links
genLink :: Gen String
genLink = do
  text <- genPlainText
  url <- genURL
  title <- genMaybe genPlainText
  return $ "[" ++ text ++ "](" ++ url ++ ")"

-- Generator for images
genImage :: Gen String
genImage = do
  altText <- genPlainText
  url <- genURL
  title <- genMaybe genPlainText
  let titlePart = maybe "\"Default Title\"" (\t -> " \"" ++ t ++ "\"") title
  return $ "![" ++ altText ++ "](" ++ url ++ " " ++ titlePart ++ ")"

-- Generator for URLs
genURL :: Gen String
genURL = do
  protocol <- elements ["http://", "https://"]
  domain <- elements ["example.com", "test.org", "google.com", "localhost", "127.0.0.1"]
  path <- listOf $ elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "/-.~"
  return $ protocol ++ domain ++ "/" ++ path

-- Generator for code blocks
genCodeBlock :: Gen String
genCodeBlock = do
  language <- elements ["haskell", "python", "javascript", "", "markdown"]
  code <- listOf $ elements $ ['\32'..'\126'] -- Printable ASCII characters
  return $ "```" ++ language ++ "\n" ++ code ++ "\n```"

-- Generator for unordered lists with nested items
genUnorderedList :: Gen String
genUnorderedList = do
  items <- listOf1 genListItem
  return $ unlines items

genListItem :: Gen String
genListItem = do
  prefix <- elements ["- "]
  content <- genPlainText
  subItems <- genMaybe $ listOf1 genSubListItem
  let subList = maybe "" (('\n':) . unlines) subItems
  return $ prefix ++ content ++ subList

genSubListItem :: Gen String
genSubListItem = do
  prefix <- elements ["    - "]
  content <- genPlainText
  return $ prefix ++ content

genOrderedList :: Gen String
genOrderedList = do
  numItems <- choose (1, 5)  -- Choose the number of list items
  items <- vectorOf numItems genOrderedListItem -- Generate the list items
  return $ unlines items

genOrderedListItem :: Gen String
genOrderedListItem = do
  content <- genPlainText
  subItems <- genMaybe genOrderedSubList  -- Generate sublist or Nothing
  let subList = maybe "" ("\n" ++) subItems -- Append sublist if it exists
  return $ "1. " ++ content ++ subList -- Always start main list at "1"

genOrderedSubList :: Gen String
genOrderedSubList = do
  numItems <- choose (1, 5)  -- Choose the number of sublist items
  items <- vectorOf numItems genPlainText -- Generate the sublist items
  return $ unlines $ zipWith (\i item -> show i ++ ". " ++ item) [1..] items -- Always start sublist at "1"


-- Generator for blockquotes with nested elements
genBlockQuote :: Gen String
genBlockQuote = do
  content <- genPlainText
  nested <- genMaybe genBlockQuote
  let nestedContent = maybe "" ("\n> " ++) nested
  return $ "> " ++ content ++ nestedContent

-- Generator for tables with varying sizes
genTable :: Gen String
genTable = do
  numCols <- choose (1, 5) :: Gen Int
  numRows <- choose (1, 5) :: Gen Int
  headers <- vectorOf numCols genPlainText
  alignments <- vectorOf numCols genAlignment
  rows <- vectorOf numRows $ vectorOf numCols genPlainText
  let headerLine = "| " ++ intercalate " | " headers ++ " |"
      separator = "| " ++ intercalate " | " alignments ++ " |"
      rowLines = map (\row -> "| " ++ intercalate " | " row ++ " |") rows
  return $ unlines $ [headerLine, separator] ++ rowLines

genAlignment :: Gen String
genAlignment = elements ["---"]

-- Generator for footnotes with a limited identifier length
genFootnote :: Gen String
genFootnote = do
  identifierLength <- choose (1, 7)  -- Limit identifier length to 1 to 3 digits
  identifier <- replicateM identifierLength $ elements ['1'..'9']  -- Generate a string of digits
  content <- genPlainText
  return $ "[^" ++ identifier ++ "]: " ++ content

-- Generator for footnote references with a limited identifier length
genFootnoteRef :: Gen String
genFootnoteRef = do
  identifierLength <- choose (1, 7)  -- Limit identifier length to 1 to 3 digits
  identifier <- replicateM identifierLength $ elements ['1'..'9']
  return $ "Here is a footnote reference[^" ++ identifier ++ "]."


-- Helper function to generate Maybe values
genMaybe :: Gen a -> Gen (Maybe a)
genMaybe gen = frequency [(1, return Nothing), (3, Just <$> gen)]

-- Generator for paragraphs
genParagraph :: Gen String
genParagraph = do
  sentences <- listOf1 genSentence
  return $ unwords sentences

genSentence :: Gen String
genSentence = do
  words <- listOf1 genWord
  return $ unwords words ++ "."

genWord :: Gen String
genWord = listOf1 $ elements $ ['a'..'z'] ++ ['A'..'Z']

-- Generator for combined markdown elements
genCombinedMarkdown :: Gen String
genCombinedMarkdown = do
  elementsList <- listOf1 $ elements
    [ genBoldText
    , genItalicText
    , genStrikeText
    , genInlineCode
    , genHeader
    , genLink
    , genImage
    , genCodeBlock
    , genUnorderedList
    , genOrderedList
    , genBlockQuote
    , genTable
    , genFootnote
    , genFootnoteRef
    , genParagraph
    ]
  contents <- sequence elementsList
  return $ intercalate "\n\n" contents

-- | Properties to test

-- Original properties

-- Property: Parsing bold text results in <strong> tags in HTML
prop_parseBoldText :: Property
prop_parseBoldText = forAll genBoldText $ \input ->
  case parse markdownParser input of
    Result _ adt ->
      let html = convertADTHTML adt
      in counterexample ("HTML output: " ++ html) $
           property ("<strong>" `isInfixOf` html && "</strong>" `isInfixOf` html)
    Error _ -> counterexample ("Failed to parse bold text: " ++ input) $ property False

-- Property: Parsing italic text results in <em> tags in HTML
prop_parseItalicText :: Property
prop_parseItalicText = forAll genItalicText $ \input ->
  case parse markdownParser input of
    Result _ adt ->
      let html = convertADTHTML adt
      in counterexample ("HTML output: " ++ html) $
           property ("<em>" `isInfixOf` html && "</em>" `isInfixOf` html)
    Error _ -> counterexample ("Failed to parse italic text: " ++ input) $ property False

-- Property: Parsing strikethrough text results in <del> tags in HTML
prop_parseStrikeText :: Property
prop_parseStrikeText = forAll genStrikeText $ \input ->
  case parse markdownParser input of
    Result _ adt ->
      let html = convertADTHTML adt
      in counterexample ("HTML output: " ++ html) $
           property ("<del>" `isInfixOf` html && "</del>" `isInfixOf` html)
    Error _ -> counterexample ("Failed to parse strikethrough text: " ++ input) $ property False

-- Property: Parsing headers results in corresponding <h1> to <h6> tags
prop_parseHeaders :: Property
prop_parseHeaders = forAll genHeader $ \input ->
  case parse markdownParser input of
    Result _ adt ->
      let html = convertADTHTML adt
          -- Determine the expected header level based on the number of '#' characters
          headerLevel = length (takeWhile (== '#') input)
          expectedTag = "<h" ++ show headerLevel ++ ">"
          closingTag = "</h" ++ show headerLevel ++ ">"
          hasCorrectHeaderTag = expectedTag `isInfixOf` html && closingTag `isInfixOf` html
      in counterexample ("HTML output: " ++ html) $ property hasCorrectHeaderTag
    Error _ -> counterexample ("Failed to parse header: " ++ input) $ property False


-- Property: Parsing links results in <a href=""> tags in HTML
prop_parseLinks :: Property
prop_parseLinks = forAll genLink $ \input ->
  case parse markdownParser input of
    Result _ adt ->
      let html = convertADTHTML adt
      in counterexample ("HTML output: " ++ html) $
           property ("<a href=" `isInfixOf` html)
    Error _ -> counterexample ("Failed to parse link: " ++ input) $ property False

-- Property: Parsing images results in <img src="" alt=""> tags in HTML
prop_parseImages :: Property
prop_parseImages = forAll genImage $ \input ->
  case parse markdownParser input of
    Result _ adt ->
      let html :: String
          html = convertADTHTML(adt)
          
          -- Regex pattern to match <img src="...">
          imgSrcPattern :: String
          imgSrcPattern = "<img src=\"[^\"]+\""

          -- Check if <img src="..."> is present in the HTML
          hasImgSrc :: Bool
          hasImgSrc = html =~ imgSrcPattern
          
      in counterexample ("HTML output: " ++ html) $
           property hasImgSrc
    Error _ -> counterexample ("Failed to parse image: " ++ input) $ property False



-- Property: Parsing code blocks results in <pre><code> tags
prop_parseCodeBlocks :: Property
prop_parseCodeBlocks = forAll genCodeBlock $ \input ->
  case parse markdownParser input of
    Result _ adt ->
      let html = convertADTHTML adt
      in counterexample ("HTML output: " ++ html) $
           property ("</code></pre>" `isInfixOf` html)
    Error _ -> counterexample ("Failed to parse code block: " ++ input) $ property False

-- Property: Parsing unordered lists results in <ul><li> tags
prop_parseUnorderedLists :: Property
prop_parseUnorderedLists = forAll genUnorderedList $ \input ->
  case parse markdownParser input of
    Result _ adt ->
      let html = convertADTHTML adt
      in counterexample ("HTML output: " ++ html) $
           property ("<ul>" `isInfixOf` html && "<li>" `isInfixOf` html)
    Error _ -> counterexample ("Failed to parse unordered list: " ++ input) $ property False

-- Property: Parsing ordered lists results in <ol><li> tags
prop_parseOrderedLists :: Property
prop_parseOrderedLists = forAll genOrderedList $ \input ->
  case parse markdownParser input of
    Result _ adt ->
      let html = convertADTHTML adt
      in counterexample ("HTML output: " ++ html) $
           property ("<ol>" `isInfixOf` html && "<li>" `isInfixOf` html)
    Error _ -> counterexample ("Failed to parse ordered list: " ++ input) $ property False

-- Property: Parsing blockquotes results in <blockquote> tags
prop_parseBlockQuotes :: Property
prop_parseBlockQuotes = forAll genBlockQuote $ \input ->
  case parse markdownParser input of
    Result _ adt ->
      let html = convertADTHTML adt
      in counterexample ("HTML output: " ++ html) $
           property ("<blockquote>" `isInfixOf` html)
    Error _ -> counterexample ("Failed to parse blockquote: " ++ input) $ property False

-- Property: Parsing tables results in <table><tr><td> tags
prop_parseTables :: Property
prop_parseTables = forAll genTable $ \input ->
  case parse markdownParser input of
    Result _ adt ->
      let html :: String
          html = convertADTHTML adt

          tablePattern :: String
          tablePattern = "<table>"

          hasTable :: Bool
          hasTable = html =~ tablePattern

      in counterexample ("HTML output: " ++ html) $
           property hasTable
    Error _ -> counterexample ("Failed to parse table: " ++ input) $ property False


-- Property: Parsing footnotes results in appropriate footnote references and/or citation
prop_parseFootnotes :: Property
prop_parseFootnotes = forAll genFootnote $ \input ->
  case parse markdownParser input of
    Result _ adt ->
      let html :: String
          html = convertADTHTML adt
          
          -- Regex patterns to match footnote definition and reference
          defPattern :: String
          defPattern = "<p id=\"fn[^\"]+\">"
          
          refPattern :: String
          refPattern = "<sup><a href=\"#fn[^\"]+\">"
          
          -- Check if the footnote definition and/or reference are present
          hasFootnoteDef :: Bool
          hasFootnoteDef = html =~ defPattern
          
          hasFootnoteRef :: Bool
          hasFootnoteRef = html =~ refPattern
          
      in counterexample ("HTML output: " ++ html) $
           property (hasFootnoteDef || hasFootnoteRef)
           
    Error _ -> counterexample ("Failed to parse footnote: " ++ input) $ property False


-- Property: Parsing inline code results in <code> tags
prop_parseInlineCode :: Property
prop_parseInlineCode = forAll genInlineCode $ \input ->
  case parse markdownParser input of
    Result _ adt ->
      let html = convertADTHTML adt
          -- Ensure the <code> tag is present somewhere in the HTML output
          hasCodeTag = "<code>" `isInfixOf` html
      in counterexample ("HTML output: " ++ html) $
           property hasCodeTag
    Error _ -> counterexample ("Failed to parse inline code: " ++ input) $ property False

-- Property: Round-trip property for plain text
prop_roundTripPlainText :: Property
prop_roundTripPlainText = forAll genPlainText $ \input ->
  case parse markdownParser input of
    Result _ adt -> 
      let html = convertADTHTML adt
      in counterexample ("HTML output: " ++ html) $
           property (input `isInfixOf` html)
    Error _ -> counterexample ("Failed to parse plain text: " ++ input) $ property False

-- Property: The parser does not crash on arbitrary input
prop_parserDoesNotCrash :: String -> Property
prop_parserDoesNotCrash input =
  property $
    case parse markdownParser input of
      Result _ _ -> True
      Error _    -> True


-- Property: Parsing combined markdown elements
prop_parseCombinedMarkdown :: Property
prop_parseCombinedMarkdown = forAll genCombinedMarkdown $ \input ->
    case parse markdownParser input of
      Result _ _ -> property True
      Error err  -> counterexample ("Failed to parse combined markdown: " ++ input ++ "\nError: " ++ show err) $ property False


-- Property: Parser correctly handles empty input
prop_parseEmptyInput :: Property
prop_parseEmptyInput =
  case parse markdownParser "" of
    Result _ adt ->
      let html = convertADTHTML adt
      in counterexample ("HTML output: " ++ html) $
           property (null html || not ("<p></p>"  `isInfixOf` html))
    Error _ -> counterexample "Failed to parse empty input" $ property False

-- Property: Parser does not produce unescaped HTML tags in output
prop_noUnescapedHTMLTags :: Property
prop_noUnescapedHTMLTags = forAll genPlainText $ \input ->
  let disallowedTags = ["<script>", "<iframe>", "<object>", "<embed>", "<style>"]
      output = case parse markdownParser input of
        Result _ adt -> convertADTHTML adt
        Error _      -> ""
  in counterexample ("HTML output: " ++ output) $
       property $ not $ any (`isInfixOf` output) disallowedTags


-- | Main function to run all tests

-- Generator for nested Bold within Italic
genBoldWithinItalic :: Gen String
genBoldWithinItalic = do
  bold <- genBoldText
  return $ "_" ++ bold ++ "_"

-- Generator for nested Italic within Bold
genItalicWithinBold :: Gen String
genItalicWithinBold = do
  italic <- genItalicText
  return $ "**" ++ italic ++ "**"

-- Generator for Bold within Link
genBoldWithinLink :: Gen String
genBoldWithinLink = do
  bold <- genBoldText
  url <- genURL
  return $ "[" ++ bold ++ "](" ++ url ++ ")"

-- Generator for Italic within Link
genItalicWithinLink :: Gen String
genItalicWithinLink = do
  italic <- genItalicText
  url <- genURL
  return $ "[" ++ italic ++ "](" ++ url ++ ")"

-- Generator for Link within Bold
genLinkWithinBold :: Gen String
genLinkWithinBold = do
  link <- genLink
  return $ "**" ++ link ++ "**"

-- Generator for Link within Italic
genLinkWithinItalic :: Gen String
genLinkWithinItalic = do
  link <- genLink
  return $ "_" ++ link ++ "_"

-- Generator for nested Ordered and Unordered Lists
genNestedLists :: Gen String
genNestedLists = oneof [genOrderedWithinUnordered, genUnorderedWithinOrdered]

genOrderedWithinUnordered :: Gen String
genOrderedWithinUnordered = do
  outerList <- genUnorderedList
  innerList <- genOrderedList
  return $ outerList ++ "\n    " ++ innerList

genUnorderedWithinOrdered :: Gen String
genUnorderedWithinOrdered = do
  outerList <- genOrderedList
  innerList <- genUnorderedList
  return $ outerList ++ "\n    " ++ innerList

-- Generator for Blockquotes containing various nested elements
genBlockquoteWithNestedElements :: Gen String
genBlockquoteWithNestedElements = do
  nestedElement <- oneof
    [ genBoldText
    , genItalicText
    , genLink
    , genUnorderedList
    , genOrderedList
    , genHeader
    , genCodeBlock
    , genBlockQuote
    ]
  return $ "> " ++ nestedElement

-- Generator for Tables with nested lists and formatting
genTableWithNestedElements :: Gen String
genTableWithNestedElements = do
  numCols <- choose (1, 3) :: Gen Int
  numRows <- choose (1, 3) :: Gen Int
  headers <- vectorOf numCols genPlainText
  alignments <- vectorOf numCols genAlignment
  rows <- vectorOf numRows $ vectorOf numCols genNestedTableCell
  let headerLine = "| " ++ intercalate " | " headers ++ " |"
      separator = "| " ++ intercalate " | " alignments ++ " |"
      rowLines = map (\row -> "| " ++ intercalate " | " row ++ " |") rows
  return $ unlines $ [headerLine, separator] ++ rowLines

-- Generator for table cells that may contain nested lists or formatting
genNestedTableCell :: Gen String
genNestedTableCell = oneof
  [ genBoldText
  , genItalicText
  , genLink
  , genInlineCode
  ]

-- Generator for nested Footnotes
genFootnoteWithinBold :: Gen String
genFootnoteWithinBold = do
  footnote <- genFootnote
  return $ "**" ++ footnote ++ "**"

genFootnoteWithinItalic :: Gen String
genFootnoteWithinItalic = do
  footnote <- genFootnote
  return $ "_" ++ footnote ++ "_"

-- Generator for deeply nested structures (e.g., Bold within Italic within Link within Blockquote)
genDeeplyNested :: Gen String
genDeeplyNested = do
  -- Generate a blockquote containing nested elements
  level1 <- genBlockquoteWithNestedElements
  
  -- Generate italic text within bold
  level2 <- genItalicWithinBold
  
  -- Generate a link within italic text
  level3 <- genLinkWithinItalic
  
  -- Generate bold text within a link
  level4 <- genBoldWithinLink
  
  -- Combine all levels into a deeply nested Markdown string
  -- Example nesting: > **_[**_link_text_**]**
  return $ "> **_" ++ "[**_" ++ level3 ++ "_**]" ++ "**"

-- | Properties to test nesting

-- Property: Bold within Italic results in properly nested <strong> and <em> tags
prop_nestedBoldWithinItalic :: Property
prop_nestedBoldWithinItalic = forAll genBoldWithinItalic $ \input ->
  case parse markdownParser input of
    Result _ adt ->
      let html = convertADTHTML adt
      in counterexample ("HTML output: " ++ html) $
           property ("<em>" `isInfixOf` html && "<strong>" `isInfixOf` html)
    Error _ -> counterexample ("Failed to parse nested Bold within Italic: " ++ input) $ property False

-- Property: Italic within Bold results in properly nested <strong> and <em> tags
prop_nestedItalicWithinBold :: Property
prop_nestedItalicWithinBold = forAll genItalicWithinBold $ \input ->
  case parse markdownParser input of
    Result _ adt ->
      let html = convertADTHTML adt
      in counterexample ("HTML output: " ++ html) $
           property ("<strong>" `isInfixOf` html && "<em>" `isInfixOf` html)
    Error _ -> counterexample ("Failed to parse nested Italic within Bold: " ++ input) $ property False

-- Property: Bold within Link results in <a> containing <strong> tags
prop_nestedBoldWithinLink :: Property
prop_nestedBoldWithinLink = forAll genBoldWithinLink $ \input ->
  case parse markdownParser input of
    Result _ adt ->
      let html = convertADTHTML adt
      in counterexample ("HTML output: " ++ html) $
           property ("<a href=" `isInfixOf` html && "<strong>" `isInfixOf` html)
    Error _ -> counterexample ("Failed to parse nested Bold within Link: " ++ input) $ property False

-- Property: Italic within Link results in <a> containing <em> tags
prop_nestedItalicWithinLink :: Property
prop_nestedItalicWithinLink = forAll genItalicWithinLink $ \input ->
  case parse markdownParser input of
    Result _ adt ->
      let html = convertADTHTML adt
      in counterexample ("HTML output: " ++ html) $
           property ("<a href=" `isInfixOf` html && "<em>" `isInfixOf` html)
    Error _ -> counterexample ("Failed to parse nested Italic within Link: " ++ input) $ property False

-- Property: Link within Bold results in <strong> containing <a> tags
prop_nestedLinkWithinBold :: Property
prop_nestedLinkWithinBold = forAll genLinkWithinBold $ \input ->
  case parse markdownParser input of
    Result _ adt ->
      let html = convertADTHTML adt
      in counterexample ("HTML output: " ++ html) $
           property ("<strong>" `isInfixOf` html && "<a href=" `isInfixOf` html)
    Error _ -> counterexample ("Failed to parse nested Link within Bold: " ++ input) $ property False

-- Property: Link within Italic results in <em> containing <a> tags
prop_nestedLinkWithinItalic :: Property
prop_nestedLinkWithinItalic = forAll genLinkWithinItalic $ \input ->
  case parse markdownParser input of
    Result _ adt ->
      let html = convertADTHTML adt
      in counterexample ("HTML output: " ++ html) $
           property ("<em>" `isInfixOf` html && "<a href=" `isInfixOf` html)
    Error _ -> counterexample ("Failed to parse nested Link within Italic: " ++ input) $ property False

-- Property: Nested Ordered and Unordered Lists are parsed correctly
prop_nestedLists :: Property
prop_nestedLists = forAll genNestedLists $ \input ->
  case parse markdownParser input of
    Result _ adt ->
      let html = convertADTHTML adt
      in counterexample ("HTML output: " ++ html) $
           property (("<ul>" `isInfixOf` html || "<ol>" `isInfixOf` html) &&
                      ("<li>" `isInfixOf` html))
    Error _ -> counterexample ("Failed to parse nested lists: " ++ input) $ property False

-- Property: Blockquotes containing nested elements are parsed correctly
prop_nestedBlockquotes :: Property
prop_nestedBlockquotes = forAll genBlockquoteWithNestedElements $ \input ->
  case parse markdownParser input of
    Result _ adt ->
      let html = convertADTHTML adt
      in counterexample ("HTML output: " ++ html) $
           property ("<blockquote>" `isInfixOf` html)
    Error _ -> counterexample ("Failed to parse nested Blockquote: " ++ input) $ property False

-- Property: Tables with nested lists and formatting are parsed correctly
prop_nestedTables :: Property
prop_nestedTables = forAll genTableWithNestedElements $ \input ->
  case parse markdownParser input of
    Result _ adt ->
      let html = convertADTHTML adt
          hasTable = "<table>" `isInfixOf` html
          hasListOrFormatted = any (`isInfixOf` html) ["<strong>", "<em>", "<a href=", "<code>"]
      in counterexample ("HTML output: " ++ html) $
           property (hasTable && hasListOrFormatted)
    Error _ -> counterexample ("Failed to parse nested Tables: " ++ input) $ property False

-- Property: Footnotes within Bold are parsed correctly
prop_nestedFootnoteWithinBold :: Property
prop_nestedFootnoteWithinBold = forAll genFootnoteWithinBold $ \input ->
  case parse markdownParser input of
    Result _ adt ->
      let html = convertADTHTML adt
      in counterexample ("HTML output: " ++ html) $
           property (("<strong>" `isInfixOf` html) && ("<sup><a" `isInfixOf` html))
    Error _ -> counterexample ("Failed to parse nested Footnote within Bold: " ++ input) $ property False

-- Property: Footnotes within Italic are parsed correctly
prop_nestedFootnoteWithinItalic :: Property
prop_nestedFootnoteWithinItalic = forAll genFootnoteWithinItalic $ \input ->
  case parse markdownParser input of
    Result _ adt ->
      let html = convertADTHTML adt
      in counterexample ("HTML output: " ++ html) $
           property (("<em>" `isInfixOf` html) && ("<sup><a" `isInfixOf` html))
    Error _ -> counterexample ("Failed to parse nested Footnote within Italic: " ++ input) $ property False


-- Property: Deeply nested structures are parsed correctly
prop_deeplyNested :: Property
prop_deeplyNested = forAll genDeeplyNested $ \input ->
  case parse markdownParser input of
    Result _ adt ->
      let html = convertADTHTML adt
          -- Check for nested <blockquote>, <strong>, <em>, <a> tags
          hasBlockquote = "<blockquote>" `isInfixOf` html
          hasStrong = "<strong>" `isInfixOf` html
          hasEm = "<em>" `isInfixOf` html
          hasLink = "<a href=" `isInfixOf` html
      in counterexample ("HTML output: " ++ html) $
           property (hasBlockquote && hasStrong && hasEm && hasLink)
    Error err -> counterexample ("Failed to parse deeply nested structure: " ++ input ++ "\nError: " ++ show err) $ property False

-- | Main function to run all tests, including nesting tests

runComprehensiveTests :: IO ()
runComprehensiveTests = do
  putStrLn "Running comprehensive markdown parser tests..."
  let args = stdArgs { maxSuccess = 1000 } 
      runProp prop = quickCheckWithResult args prop
  results <- sequence
    [ runProp prop_parseBoldText
    , runProp prop_parseItalicText
    , runProp prop_parseStrikeText
    , runProp prop_parseHeaders
    , runProp prop_parseLinks
    , runProp prop_parseImages
    , runProp prop_parseCodeBlocks
    , runProp prop_parseUnorderedLists
    , runProp prop_parseOrderedLists
    , runProp prop_parseBlockQuotes
    , runProp prop_parseTables
    , runProp prop_parseFootnotes
    , runProp prop_parseInlineCode
    , runProp prop_roundTripPlainText
    , runProp prop_parserDoesNotCrash
    , runProp prop_parseCombinedMarkdown
    , runProp prop_parseEmptyInput
    , runProp prop_noUnescapedHTMLTags
    ]
  let success = all isSuccess results
  unless success exitFailure
  where
    isSuccess (Success{}) = True
    isSuccess _           = False

runNestingTests :: IO ()
runNestingTests = do
  putStrLn "Running nesting-specific markdown parser tests..."
  let args = stdArgs { maxSuccess = 1000 } 
      runProp prop = quickCheckWithResult args prop
  results <- sequence
    [ runProp prop_nestedBoldWithinItalic
    , runProp prop_nestedItalicWithinBold
    , runProp prop_nestedBoldWithinLink
    , runProp prop_nestedItalicWithinLink
    , runProp prop_nestedLinkWithinBold
    , runProp prop_nestedLinkWithinItalic
    , runProp prop_nestedLists
    , runProp prop_nestedBlockquotes
    , runProp prop_nestedTables
    , runProp prop_deeplyNested
    ]
  let success = all isSuccess results
  unless success exitFailure
  where
    isSuccess (Success{}) = True
    isSuccess _           = False


allTests :: IO ()
allTests = do
  runComprehensiveTests
  runNestingTests
