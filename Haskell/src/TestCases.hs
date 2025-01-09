module TestCases where

import           Assignment (convertADTHTML, markdownParser)
import           Instances  (ParseResult (Error, Result), parse)
import           Data.List  (intercalate)
import           Data.Char  (isSpace)

-- Data type to represent a test case without expectedADT
data TestCase = TestCase
  { testName       :: String
  , testInput      :: String
  , expectedOutput :: String
  }

-- Helper function to trim whitespace from both ends
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

-- Function to set the terminal color using ANSI escape codes
setColor :: String -> IO ()
setColor color = putStr color

resetColor :: IO ()
resetColor = putStr "\ESC[0m"

-- Function to run a single test case
runTestCase :: TestCase -> IO Bool
runTestCase (TestCase name input expectedHtml) = do
  putStrLn $ "Running test: " ++ name
  passed <- case parse markdownParser input of
    Error err -> do
      setColor "\ESC[31m" -- Set color to red
      putStrLn $ "Parser error: " ++ show err
      resetColor
      return False
    Result _ adt -> do
      let actualHtml = trim $ convertADTHTML adt
      if actualHtml == trim expectedHtml
        then do
          setColor "\ESC[32m" -- Set color to green
          putStrLn "Test Passed"
          resetColor
          return True
        else do
          setColor "\ESC[31m" -- Set color to red
          putStrLn "Test Failed"
          putStrLn "Expected HTML:"
          putStrLn expectedHtml
          putStrLn "Actual HTML:"
          putStrLn actualHtml
          resetColor
          return False
  putStrLn "" -- Print an empty line for formatting
  return passed

-- Function to run all test cases and summarize results
runTestCases :: [TestCase] -> IO ()
runTestCases tests = do
  results <- mapM runTestCase tests
  let passed = length $ filter id results
  let total = length tests
  putStrLn ""
  putStrLn $ "Test Summary: " ++ show passed ++ " out of " ++ show total ++ " tests passed."
  if passed == total
    then setColor "\ESC[32m" -- Green if all tests passed
    else setColor "\ESC[31m" -- Red if some tests failed
  putStrLn $ if passed == total then "All tests passed!" else "Some tests failed!"
  resetColor

-- Define your test cases here
testCases :: [TestCase]
testCases =
  [ TestCase
        { testName = "Test Bold Text"
        , testInput = "**bold text**"
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <p><strong>bold text</strong></p>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Test Italic Text"
        , testInput = "_italic text_"
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <p><em>italic text</em></p>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Test Nested Modifiers (Not Supported)"
        , testInput = "**_bold and italic_**"
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <p><strong><em>bold and italic</em></strong></p>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Test Link with Modifiers in Text"
        , testInput = "[click **here**](https://example.com)"
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <p><a href=\"https://example.com\">click <strong>here</strong></a></p>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Test Header Level 1"
        , testInput = "# Header 1"
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <h1>Header 1</h1>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Test Code Block with Language"
        , testInput = "```haskell\nmain = putStrLn \"Hello, World!\"\n```"
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <pre><code class=\"language-haskell\">main = putStrLn \"Hello, World!\"</code></pre>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Test Ordered List"
        , testInput = "1. First Item\n2. Second Item\n3. Third Item"
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <ol>\n        <li>First Item</li>\n        <li>Second Item</li>\n        <li>Third Item</li>\n    </ol>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Edge Case: **** * ****"
        , testInput = "**** * ****"
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <p><strong>** * **</strong></p>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Super Nested List"
        , testInput = "1. a\n    1. a\n        1. a\n            1. a\n                1. a\n                    1. a\n                        1. a\n                            1. a\n                                1. a"
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <ol>\n        <li>a\n            <ol>\n                <li>a\n                    <ol>\n                        <li>a\n                            <ol>\n                                <li>a\n                                    <ol>\n                                        <li>a\n                                            <ol>\n                                                <li>a\n                                                    <ol>\n                                                        <li>a\n                                                            <ol>\n                                                                <li>a\n                                                                    <ol>\n                                                                        <li>a</li>\n                                                                    </ol>\n                                                                </li>\n                                                            </ol>\n                                                        </li>\n                                                    </ol>\n                                                </li>\n                                            </ol>\n                                        </li>\n                                    </ol>\n                                </li>\n                            </ol>\n                        </li>\n                    </ol>\n                </li>\n            </ol>\n        </li>\n    </ol>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Test Inline Code"
        , testInput = "Here is some code: `print(\"Hello World\")`"
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <p>Here is some code: <code>print(\"Hello World\")</code></p>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Test Code Block"
        , testInput = "```python\nprint(\"Hello World\")\n```"
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <pre><code class=\"language-python\">print(\"Hello World\")</code></pre>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Test Unordered List"
        , testInput = "1. Item 1\n2. Item 2\n    1. Subitem 2.1\n    4. Subitem 2.2\n5. Item 3"
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <ol>\n        <li>Item 1</li>\n        <li>Item 2\n            <ol>\n                <li>Subitem 2.1</li>\n                <li>Subitem 2.2</li>\n            </ol>\n        </li>\n        <li>Item 3</li>\n    </ol>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Test Ordered List"
        , testInput = "1. First Item\n2. Second Item\n3. Third Item"
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <ol>\n        <li>First Item</li>\n        <li>Second Item</li>\n        <li>Third Item</li>\n    </ol>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Test Blockquote"
        , testInput = "> This is a quote"
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <blockquote>\n        <p>This is a quote</p>\n    </blockquote>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Test Header Levels"
        , testInput = "# Header 1\n## Header 2\n### Header 3"
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <h1>Header 1</h1>\n    <h2>Header 2</h2>\n    <h3>Header 3</h3>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Test Table"
        , testInput = "| Header 1 | Header 2 |\n| --- | --- |\n| Cell 1 | Cell 2 |\n| Cell 3 | Cell 4 |"
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <table>\n        <tr>\n            <th>Header 1</th>\n            <th>Header 2</th>\n        </tr>\n        <tr>\n            <td>Cell 1</td>\n            <td>Cell 2</td>\n        </tr>\n        <tr>\n            <td>Cell 3</td>\n            <td>Cell 4</td>\n        </tr>\n    </table>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Edge Case: Multiple Modifiers"
        , testInput = "**~~_Bold, Italic, and Strikethrough_~~**"
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <p><strong><del><em>Bold, Italic, and Strikethrough</em></del></strong></p>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Edge Case: Empty Bold Text"
        , testInput = "****"
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <p>****</p>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Test Footnote"
        , testInput = "Here is a sentence with a footnote.[^1]\n\n[^1]: This is the footnote."
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <p>Here is a sentence with a footnote.<sup><a id=\"fn1ref\" href=\"#fn1\">1</a></sup></p>\n    <p></p>\n    <p id=\"fn1\">This is the footnote.</p>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Test Horizontal Rule"
        , testInput = "---"
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <p>---</p>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Test Strikethrough Text"
        , testInput = "~~strikethrough text~~"
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <p><del>strikethrough text</del></p>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Test Link"
        , testInput = "[click here](https://example.com)"
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <p><a href=\"https://example.com\">click here</a></p>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Test Inline Code"
        , testInput = "This is some `inline code` in a sentence."
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <p>This is some <code>inline code</code> in a sentence.</p>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Test Footnote in Text"
        , testInput = "Here is a footnote reference[^2] in the text."
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <p>Here is a footnote reference<sup><a id=\"fn2ref\" href=\"#fn2\">2</a></sup> in the text.</p>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Test Image with Caption"
        , testInput = "![An image](https://example.com/image.jpg \"Image Caption\")"
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <img src=\"https://example.com/image.jpg\" alt=\"An image\" title=\"Image Caption\">\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Test Image with Leading Whitespace"
        , testInput = "   ![Alt Text](URL \"Caption Text\")"
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <img src=\"URL\" alt=\"Alt Text\" title=\"Caption Text\">\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Test Footnote Reference"
        , testInput = "[^1]: This is the footnote text."
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <p id=\"fn1\">This is the footnote text.</p>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Test Footnote Reference with Leading Whitespace"
        , testInput = "    [^3]: Footnote with leading spaces."
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <p id=\"fn3\">Footnote with leading spaces.</p>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Test Free Text with Modifiers"
        , testInput = "This is some **bold** and _italic_ text with no special formatting."
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <p>This is some <strong>bold</strong> and <em>italic</em> text with no special formatting.</p>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Test Heading Level 3"
        , testInput = "### Heading Level 3"
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <h3>Heading Level 3</h3>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Test Heading Level 1 Alternative Syntax"
        , testInput = "Heading Level 1\n====="
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <h1>Heading Level 1</h1>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Test Heading with Modifiers"
        , testInput = "#### This is a heading with **bold** text"
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <h4>This is a heading with <strong>bold</strong> text</h4>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Test Blockquote with Leading Whitespace"
        , testInput = "   > This is a blockquote with leading spaces."
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <blockquote>\n        <p>This is a blockquote with leading spaces.</p>\n    </blockquote>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Test Blockquote with Modifiers"
        , testInput = "> **Bold** and _italic_ text in a blockquote."
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <blockquote>\n        <p><strong>Bold</strong> and <em>italic</em> text in a blockquote.</p>\n    </blockquote>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Test Code Block without Language"
        , testInput = "```\nprint(\"Hello, World!\")\n```"
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <pre><code>print(\"Hello, World!\")</code></pre>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Test Code Block with Leading Whitespace"
        , testInput = "    ```python\n    print(\"Indented code block\")\n    ```"
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <p><code>`</code>python</p>\n    <p>    print(\"Indented code block\")</p>\n    <p>    <code>`</code></p>\n</body>\n\n</html>\n"
        }
    , TestCase
        { testName = "Test Ordered List"
        , testInput = "1. First Item\n2. Second Item\n3. Third Item"
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <ol>\n        <li>First Item</li>\n        <li>Second Item</li>\n        <li>Third Item</li>\n    </ol>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Test Ordered List with Sublists"
        , testInput = "1. Main Item\n    1. Sub Item 1\n    2. Sub Item 2\n2. Next Main Item"
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <ol>\n        <li>Main Item\n            <ol>\n                <li>Sub Item 1</li>\n                <li>Sub Item 2</li>\n            </ol>\n        </li>\n        <li>Next Main Item</li>\n    </ol>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Test Ordered List with Modifiers"
        , testInput = "1. **Bold Item**\n2. _Italic Item_\n3. Item with [Link](https://example.com)"
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <ol>\n        <li><strong>Bold Item</strong></li>\n        <li><em>Italic Item</em></li>\n        <li>Item with <a href=\"https://example.com\">Link</a></li>\n    </ol>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Test Ordered List Starting with Wrong Number"
        , testInput = "2. Should Not Start Here\n3. Next Item"
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <p>2. Should Not Start Here</p>\n    <p>3. Next Item</p>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Test Ordered List with Incorrect Indentation"
        , testInput = "1. Item\n 1. Not a Sublist Item"
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <ol>\n        <li>Item</li>\n    </ol>\n    <p> 1. Not a Sublist Item</p>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Test Table with Modifiers"
        , testInput = "| **Header 1** | _Header 2_ |\n| ---------- | ---------- |\n| **Bold**     | _Italic_   |\n| [Link](url)  | `Code`     |"
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <table>\n        <tr>\n            <th><strong>Header 1</strong></th>\n            <th><em>Header 2</em></th>\n        </tr>\n        <tr>\n            <td><strong>Bold</strong></td>\n            <td><em>Italic</em></td>\n        </tr>\n        <tr>\n            <td><a href=\"url\">Link</a></td>\n            <td><code>Code</code></td>\n        </tr>\n    </table>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Test Table with Leading Whitespace"
        , testInput = "   | Column 1 | Column 2 |\n   | -------- | -------- |\n   | Data 1   | Data 2   |"
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <table>\n        <tr>\n            <th>Column 1</th>\n            <th>Column 2</th>\n        </tr>\n        <tr>\n            <td>Data 1</td>\n            <td>Data 2</td>\n        </tr>\n    </table>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Test Table with Inconsistent Columns"
        , testInput = "| Col 1 | Col 2 |\n| ------ | ----- |\n| Data 1 |\n| Data 2 | Data 3 |"
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <p>| Col 1 | Col 2 |</p>\n    <p>| ------ | ----- |</p>\n    <p>| Data 1 |</p>\n    <p>| Data 2 | Data 3 |</p>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Test Heading with Leading Whitespace"
        , testInput = "   ## Heading with Spaces"
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <h2>Heading with Spaces</h2>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Test Invalid Heading"
        , testInput = "No hash symbol\n#No space after hash"
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <p>No hash symbol</p>\n    <p>#No space after hash</p>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Test Multiple Footnote References"
        , testInput = "[^2]: Second footnote.\n[^1]: First footnote."
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <p id=\"fn2\">Second footnote.</p>\n    <p id=\"fn1\">First footnote.</p>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Test Free Text with Newlines"
        , testInput = "This is some free text.\n\nIt has multiple paragraphs."
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <p>This is some free text.</p>\n    <p></p>\n    <p>It has multiple paragraphs.</p>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Empty Line"
        , testInput = ""
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Edge Case: **~~**~~\n**hi*hi**\n**hi_hi**\n**hi**hi**"
        , testInput = "**~~**~~\n**hi*hi**\n**hi_hi**\n**hi**hi**"
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <p><strong>~~</strong>~~</p>\n    <p><strong>hi*hi</strong></p>\n    <p><strong>hi_hi</strong></p>\n    <p><strong>hi</strong>hi**</p>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Edge Case: Empty Link"
        , testInput = "[](something)"
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <p>[](something)</p>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Edge Case: Empty Link"
        , testInput = "[]()"
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <p>[]()</p>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Edge Case: Empty Image"
        , testInput = "![](something \"caption\")"
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <p>![](something \"caption\")</p>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Edge Case: Invalid Image"
        , testInput = "![Hi](something \"caption\")l"
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <p>![Hi](something \"caption\")l</p>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Edge Case: Valid Image"
        , testInput = "![Hi](something \"caption\")"
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <img src=\"something\" alt=\"Hi\" title=\"caption\">\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Unordered List"
        , testInput = "- a\n- b\n    - c\n    - d"
        , expectedOutput = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <ul>\n        <li>a</li>\n        <li>b\n            <ul>\n                <li>c</li>\n                <li>d</li>\n            </ul>\n        </li>\n    </ul>\n</body>\n\n</html>"
        }
    , TestCase
        { testName = "Single Item List"
        , testInput = "1. a"
        , expectedOutput =  "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    <meta charset=\"UTF-8\">\n    <title>Test</title>\n</head>\n\n<body>\n    <ol>\n        <li>a</li>\n    </ol>\n</body>\n\n</html>\n"}
    ]


main :: IO ()
main = runTestCases testCases
