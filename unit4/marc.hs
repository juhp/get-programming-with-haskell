{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.IO as T
import Data.Tuple.Extra
import Data.Maybe

data Book = Book { bookAuthor :: Text
                 , bookTitle :: Text }
  deriving Show

newtype MarcRecordRaw = MarcRecord {unRecord :: ByteString}

main :: IO ()
main = do
  marcData <- B.readFile "sample.mrc"
  let processed = processRecords 50 marcData
  print $ length processed
  T.writeFile "books.html" $ booksToHtml processed
  where
    processRecords :: Int -> ByteString -> [Book]
    processRecords n =
      mapMaybe maybeBook . take n . map (lookupTitle &&& lookupAuthor) . allRecords

    allRecords :: ByteString -> [MarcRecordRaw]
    allRecords marcStream =
      if B.null marcStream
      then []
      else MarcRecord next : allRecords rest
      where
        (next, rest) = B.splitAt recordLength marcStream
        recordLength = rawToInt . B.take 5 $ marcStream

    lookupTitle :: MarcRecordRaw -> Maybe Text
    lookupTitle =
      let titleTag = "245"
          titleSubfield = 'a'
      in lookupValue titleTag titleSubfield

    lookupAuthor :: MarcRecordRaw -> Maybe Text
    lookupAuthor =
      let authorTag = "100"
          authorSubfield = 'a'
      in lookupValue authorTag authorSubfield

    maybeBook :: (Maybe Text, Maybe Text) -> Maybe Book
    maybeBook (mtitle, mauthor) = do
      title <- mtitle
      author <- mauthor
      return $ Book title author

lookupValue :: Text -> Char -> MarcRecordRaw -> Maybe Text
lookupValue aTag subfield record =
  lookupSubfield entryMetadata
  where
    entryMetadata = lookupFieldMetadata aTag record

    lookupSubfield :: Maybe FieldMetadata -> Maybe Text
    lookupSubfield Nothing = Nothing
    lookupSubfield (Just fieldMetadata) =
      if null results
      then Nothing
      else Just ((T.drop 1 . head) results)
      where
        rawField = getTextField record fieldMetadata

        fieldDelimiter :: Char
        fieldDelimiter = toEnum 31

        subfields = T.split (== fieldDelimiter) rawField

        results = filter ((== subfield) . T.head) subfields

getTextField :: MarcRecordRaw -> FieldMetadata -> Text
getTextField record fieldMetadata = E.decodeUtf8 byteStringValue
  where
    baseAddress = getBaseAddress record
    baseRecord = B.drop baseAddress $ unRecord record
    baseAtEntry = B.drop (fieldStart fieldMetadata) baseRecord
    byteStringValue = B.take (fieldLength fieldMetadata) baseAtEntry

type Html = Text

booksToHtml :: [Book] -> Html
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

    bookToHtml :: Book -> Html
    bookToHtml book = mconcat ["<p>\n"
                              ,titleInTags
                              ,authorInTags
                              ,"</p>\n"]
      where
        titleInTags = mconcat["<strong>",bookTitle book,"</strong>\n"]
        authorInTags = mconcat["<em>",bookAuthor book,"</em>\n"]

rawToInt :: ByteString -> Int
rawToInt = read . T.unpack . E.decodeUtf8

getBaseAddress :: MarcRecordRaw -> Int
getBaseAddress (MarcRecord record) =
  rawToInt . B.take 5 . B.drop 12 $ record

lookupFieldMetadata :: Text -> MarcRecordRaw -> Maybe FieldMetadata
lookupFieldMetadata aTag record =
  if null results
  then Nothing
  else Just (head results)
  where
    metadata = (map makeFieldMetadata . splitDirectory . getDirectory) record
    results = filter ((== aTag) . tag) metadata

getDirectory :: MarcRecordRaw -> ByteString
getDirectory record =
  B.take (getDirectoryLength record) . B.drop leaderLength . unRecord $ record
  where
    leaderLength :: Int
    leaderLength = 24

    getDirectoryLength :: MarcRecordRaw -> Int
    getDirectoryLength leader =
      getBaseAddress leader - (leaderLength + 1)

newtype MarcDirectoryEntryRaw = MarcDirectoryEntry ByteString

splitDirectory :: ByteString -> [MarcDirectoryEntryRaw]
splitDirectory directory =
  if B.null directory
  then []
  else MarcDirectoryEntry nextEntry : splitDirectory restEntries
  where
    (nextEntry, restEntries) =
      let dirEntryLength = 12
      in B.splitAt dirEntryLength directory

data FieldMetadata =
  FieldMetadata { tag :: Text
                , fieldLength :: Int
                , fieldStart :: Int }
  deriving Show

makeFieldMetadata :: MarcDirectoryEntryRaw -> FieldMetadata
makeFieldMetadata (MarcDirectoryEntry entry) =
  FieldMetadata textTag theLength theStart
  where
    (theTag,rest) = B.splitAt 3 entry
    textTag = E.decodeUtf8 theTag
    (rawLength,rawStart) = B.splitAt 4 rest
    theLength = rawToInt rawLength
    theStart = rawToInt rawStart
