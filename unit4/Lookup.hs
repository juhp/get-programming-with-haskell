module Lookup (
  lookupValue,
  MarcRecordRaw(..)
  )
where

import qualified Data.ByteString as B
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E

import Common
import Directory

data FieldMetadata =
  FieldMetadata { fieldTag :: Text
                , fieldLength :: Int
                , fieldStart :: Int }
  deriving Show

lookupValue :: Text -> Char -> MarcRecordRaw -> Maybe Text
lookupValue tag subfield record =
  lookupSubfield entryMetadata
  where
    entryMetadata :: Maybe FieldMetadata
    entryMetadata =
      if null results
      then Nothing
      else Just (head results)
      where
        results = filter ((== tag) . fieldTag) (metadataList record)

        metadataList :: MarcRecordRaw -> [FieldMetadata]
        metadataList = map makeFieldMetadata . marcDirectory

        makeFieldMetadata :: MarcDirectoryEntryRaw -> FieldMetadata
        makeFieldMetadata (MarcDirectoryEntry entry) =
          FieldMetadata textTag theLength theStart
          where
            (theTag,rest) = B.splitAt 3 entry
            textTag = E.decodeUtf8 theTag
            (rawLength,rawStart) = B.splitAt 4 rest
            theLength = rawToInt rawLength
            theStart = rawToInt rawStart

    lookupSubfield :: Maybe FieldMetadata -> Maybe Text
    lookupSubfield Nothing = Nothing
    lookupSubfield (Just fieldMetadata) =
      case find ((== subfield) . T.head) subfields of
        Nothing -> Nothing
        -- field terminator: Record separator '\RS' = chr 30 = 0x1e
        Just result -> Just . T.takeWhile (/= '\^^') . T.drop 1 $ result
      where
        -- delimiter: Unit separator '\US' = chr 31 = 0x1f
        subfields = T.split (== '\^_') rawField

        rawField :: Text
        rawField = E.decodeUtf8 byteStringValue
          where
            baseAddress = getBaseAddress record
            baseRecord = B.drop baseAddress $ unRecord record
            baseAtEntry = B.drop (fieldStart fieldMetadata) baseRecord
            byteStringValue = B.take (fieldLength fieldMetadata) baseAtEntry
