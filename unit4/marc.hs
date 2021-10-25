{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Maybe

import Common
import Lookup

data Book = Book { bookAuthor :: Text
                 , bookTitle :: Text }
  deriving Show

main :: IO ()
main = do
  marcData <- B.readFile "sample.mrc"
  let processed = take 50 $ processRecords marcData
  print $ length processed
  T.writeFile "books.html" $ booksToHtml processed

processRecords :: ByteString -> [Book]
processRecords =
  mapMaybe lookupBook . allRecords
  where
    allRecords :: ByteString -> [MarcRecordRaw]
    allRecords marcStream =
      if B.null marcStream
      then []
      else MarcRecord next : allRecords rest
      where
        (next, rest) = B.splitAt recordLength marcStream
        recordLength = rawToInt . B.take 5 $ marcStream

    lookupBook :: MarcRecordRaw -> Maybe Book
    lookupBook record = do
      title <- lookupValue "245" 'a' record
      author <- lookupValue "100" 'a' record
      return $ Book title author

booksToHtml :: [Book] -> Text
booksToHtml books = mconcat [ "<html>\n"
                            , "<head><title>books</title>"
                            , "<meta charset='utf-8'/>"
                            , "</head>\n"
                            , "<body>\n"
                            , booksHtml books
                            , "\n</body>\n"
                            , "</html>"]
  where
    booksHtml = mconcat . map bookToHtml

    bookToHtml :: Book -> Text
    bookToHtml book = mconcat ["<p>\n"
                              ,titleInTags
                              ,authorInTags
                              ,"</p>\n"]
      where
        titleInTags = mconcat["<strong>",bookTitle book,"</strong>\n"]
        authorInTags = mconcat["<em>",bookAuthor book,"</em>\n"]
