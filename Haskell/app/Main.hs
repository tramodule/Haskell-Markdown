{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- import Assignment (markdownParser)

import           Assignment              (markdownParser, saveHtmlFile, auxConvertADTHTML)
import           Data.Aeson              (object, (.=))
import           Data.Aeson.Key          (fromString)
import           Data.Text.Lazy          (Text, pack, unpack)
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Instances               (ParseResult (Result), parse)
import           Web.Scotty              (ActionM, body, json, post, scotty, header)
import           Data.Char               (isSpace)
import           Data.Maybe              (fromMaybe)
import           Control.Monad.IO.Class  (liftIO)

getResult :: ParseResult a -> String -> String -> (a -> String -> String -> String) -> String
getResult (Result _ a) title css f = f (a) (title) (css)
getResult _ _ _ _         = ""

-- Magic code to convert key, value pairs to JSON to send back to the server
jsonResponse :: [(String, String)] -> ActionM ()
jsonResponse pairs =
  json $ object [fromString key .= ((pack value) :: Text) | (key, value) <- pairs]

trimFirstEnd :: String -> String
trimFirstEnd = reverse . dropWhile isSpace . reverse . dropWhile isSpace


parseSaveState :: Maybe Text -> Bool
parseSaveState maybeSaveState =
  case fmap unpack maybeSaveState of
    Just "true"  -> True
    Just "1"     -> True
    Just "false" -> False
    Just "0"     -> False
    _            -> False 


cssStyle :: String
cssStyle = "<style>\n" ++
    "h3, h4, h5, h6 { color: #2d2d2d; margin-top: 20px; margin-bottom: 10px; }\n" ++
    "h2 { font-size: 1.5em; color: #444444; margin-bottom: 10px; }\n" ++
    "h1 { font-size: 2em; color: #444444; margin-bottom: 10px; }\n" ++
    "p { margin-bottom: 15px; color: #444444; }\n" ++
    "code { background-color: #f1f1f1; padding: 2px 4px; border-radius: 4px; font-size: 90%; color: #c7254e; }\n" ++
    "pre { background-color: #f1f1f1; padding: 10px; border-radius: 4px; overflow: auto; }\n" ++
    "a { color: #007acc; text-decoration: none; }\n" ++
    "a:hover { text-decoration: underline; }\n" ++
    "a:visited { color: #7a5ea8; }\n" ++
    "blockquote { border-left: 4px solid #cccccc; margin: 20px 0; padding: 15px 20px; background-color: #f9f9f9; color: #555555; border-radius: 8px; }\n" ++
    "table { width: 100%; border-collapse: collapse; margin-bottom: 20px; overflow: hidden; border-radius: 8px; box-shadow: 0 0 10px rgba(0, 0, 0, 0.1); }\n" ++
    "th, td { padding: 12px 15px; text-align: left; border-bottom: 1px solid #dddddd; }\n" ++
    "thead { background-color: #f2f2f2; }\n" ++
    "tr:hover { background-color: #f9f9f9; }\n" ++
    "tr:last-child td { border-bottom: none; }\n" ++
    "ol { counter-reset: cupcake; padding-left: 32px; }\n" ++
    "ol li { counter-increment: cupcake; }\n" ++
    "ol li::marker { content: counters(cupcake, '.') ' '; color: hotpink; font-weight: bold; font-family: cursive; }\n" ++
    "footer { margin-top: 20px; padding: 10px; background-color: #2d2d2d; color: #ffffff; text-align: center; font-size: 14px; box-shadow: 0 -4px 8px rgba(0, 0, 0, 0.1); }\n"
    ++ "\n</style>\n"


main :: IO ()
main = scotty 3000 $ do 
  post "/api/convertMD" $ do
    requestBody <- body
    -- Convert the raw request body from ByteString to Text

    saveState <- header "Save-State"
    style <- header "Style-Css"
    titlePage <- header "Title-Page"
    let saveStateBool = parseSaveState saveState
        styleCss = parseSaveState style

    let requestBodyText = decodeUtf8 requestBody
        -- Convert the Text to String
        str = unpack requestBodyText
        title = fromMaybe "" (fmap unpack titlePage)
        -- Parse the Markdown string using 'markdownParser' and apply 'convertAllHTML'
        converted_html = getResult (parse markdownParser str) (title) ("") (auxConvertADTHTML)
    
    liftIO $ do
        if saveStateBool
          then saveHtmlFile $ getResult (parse markdownParser str) (title) (if styleCss then cssStyle else "") (auxConvertADTHTML)
          else return ()
    -- Respond with the converted HTML as JSON
    jsonResponse [("html", converted_html)]
