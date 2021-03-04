{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Metadata.Validate.Wallet
  ( MetadataValidationError(..)
  , prettyPrintMetadataValidationError
  , validateFileContents
  , pFileName
  , metadataJSONMaxSize
  ) where

import Data.Int (Int64)
import Data.Char (isPrint)
import           Prelude hiding (log)
import           Data.Void (Void)
import           Control.Monad.Except (MonadError, throwError)
import           Data.Text (Text)
import           Colog                         (pattern D, pattern E, pattern I, Message, WithLog, log)
import Cardano.Slotting.Slot
    ( SlotNo (..) )
import Cardano.Metadata.GoguenRegistry
    ( GoguenRegistryEntry (..)
    , PartialGoguenRegistryEntry
    , parseRegistryEntry
    , validateEntry
    )
import Cardano.Metadata.Types ( Subject(Subject))
import System.FilePath.Posix (takeBaseName)

import qualified Text.Megaparsec               as P
import qualified Text.Megaparsec.Char          as P
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson

type Parser = P.Parsec Void Text

pFileName :: Parser Text
pFileName = do
  (fileName :: Text) <- P.takeWhile1P (Just "printable Unicode character") (\c -> isPrint c && c /= '.')

  let len = T.length fileName
  if len < 1 || len > 256
    then fail $ "Expected 1-256 chars but found " <> show len <> "."
    else pure ()

  _fileSuffix <- P.string ".json"
  P.eof
  pure fileName

data MetadataValidationError
  = MetadataBadFileName FilePath (P.ParseErrorBundle Text Void)
  -- ^ The metadata's file name failed to validate
  | MetadataTooLarge Int64 Int64
  -- ^ The metadata is too large (actual, limit).
  | MetadataFailedToParseJSON String String
  -- ^ Failed to parse a JSON value from the file contents (contents, err)
  | MetadataFailedToParseRegistryEntry Aeson.Value String
  -- ^ Failed to parse a metadata entry from the file contents (json value, err)
  | MetadataFailedToValidate PartialGoguenRegistryEntry Text
  -- ^ Failed to validate the metadata entry (json value, err)
  | MetadataMissingSubject PartialGoguenRegistryEntry
  -- ^ Metadata entry is missing a subject (entry)
  | MetadataSubjectFileNameMismatch Subject FilePath 
  -- ^ Metadata entry subject does not match filename (subject, filename)

prettyPrintMetadataValidationError :: MetadataValidationError -> Text
prettyPrintMetadataValidationError (MetadataBadFileName fp err) =
  "Failed to parse file name '" <> T.pack fp <> "', error was '" <> T.pack (P.errorBundlePretty err) <> "'."
prettyPrintMetadataValidationError (MetadataTooLarge actual limit) =
  "File size in bytes (" <> T.pack (show actual) <> " bytes) greater than maximum size of " <> T.pack (show limit) <> " bytes."
prettyPrintMetadataValidationError (MetadataFailedToParseJSON contents err) =
  "Failed to parse valid JSON from: '" <> T.pack contents <> "', error was: " <> T.pack err <> "."
prettyPrintMetadataValidationError (MetadataFailedToParseRegistryEntry val err) =
  "Failed to parse wallet metadata entry from JSON: '" <> T.pack (show val) <> "', error was: " <> T.pack err <> "."
prettyPrintMetadataValidationError (MetadataFailedToValidate val err) =
  "Failed to validate wallet metadata entry '" <> T.pack (show val) <> "', error was: '" <> err <> "'."
prettyPrintMetadataValidationError (MetadataMissingSubject entry) =
  "Wallet metadata entry missing subject '" <> T.pack (show entry) <> "'."
prettyPrintMetadataValidationError (MetadataSubjectFileNameMismatch (Subject subj) fileName) =
  "Wallet Metadata subject '" <> subj <> "', does not match file name '" <> T.pack fileName <> "'."

validateFileContents
  :: ( WithLog env Message m
     , MonadError MetadataValidationError m
     )
  => FilePath
  -> String
  -> SlotNo
  -> m ()
validateFileContents filePath contents slotNo = do
  log I $ "Validating file " <> T.pack filePath
  log D $ "File contents: " <> T.pack contents

  case (P.runParser pFileName filePath (T.pack filePath)) of
    Left e -> do
      logAndThrowError $ MetadataBadFileName filePath e
    Right _ -> do
      let contentLength = fromIntegral $ length contents

      if contentLength > metadataJSONMaxSize
        then do
          logAndThrowError $ MetadataTooLarge contentLength metadataJSONMaxSize
        else
          log I ("Good file size ( " <> T.pack (show contentLength) <> " < " <> T.pack (show metadataJSONMaxSize) <> " bytes)")

      -- Must use encodeUtf8 otherwise Aeson will fail on decoding Utf characters
      case Aeson.eitherDecode (TL.encodeUtf8 . TL.pack $ contents) of
        Left e     -> do
          logAndThrowError $ MetadataFailedToParseJSON contents e
        Right json -> do
          case Aeson.parseEither parseRegistryEntry json of
            Left e -> do
              logAndThrowError $ MetadataFailedToParseRegistryEntry json e
            Right entry -> do
              log I $ T.pack $ "Successfully decoded entry: " <> show entry
              log I $ "Validating attestation signatures and content..."
    
              case validateEntry slotNo entry of
                Left e -> do
                  logAndThrowError $ MetadataFailedToValidate entry e
                Right () ->
                  case _goguenRegistryEntry_subject entry of
                    Nothing             -> do
                      logAndThrowError $ MetadataMissingSubject entry
                    Just (Subject subj) -> do
                      let baseName = T.pack $ takeBaseName $ filePath
                      if baseName /= subj
                        then do
                          logAndThrowError $ MetadataSubjectFileNameMismatch (Subject subj) filePath
                        else do
                          log I "File valid!"

  where
    logAndThrowError :: (WithLog env Message m, MonadError MetadataValidationError m) => MetadataValidationError -> m ()
    logAndThrowError err = do
      log E $ prettyPrintMetadataValidationError err
      throwError err

-- | Maximum size in bytes of a metadata entry.
metadataJSONMaxSize :: Int64
metadataJSONMaxSize = 380000
