module Directory (
  getBaseAddress,
  marcDirectory,
  MarcRecordRaw(..),
  MarcDirectoryEntryRaw(..)
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Common

newtype MarcRecordRaw = MarcRecord {unRecord :: ByteString}

marcDirectory :: MarcRecordRaw -> [MarcDirectoryEntryRaw]
marcDirectory = splitDirectory . getDirectory

getDirectory :: MarcRecordRaw -> ByteString
getDirectory record =
  B.take (getDirectoryLength record) . B.drop leaderLength . unRecord $ record
  where
    leaderLength :: Int
    leaderLength = 24

    getDirectoryLength :: MarcRecordRaw -> Int
    getDirectoryLength leader =
      getBaseAddress leader - (leaderLength + 1)

getBaseAddress :: MarcRecordRaw -> Int
getBaseAddress (MarcRecord record) =
  rawToInt . B.take 5 . B.drop 12 $ record

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
