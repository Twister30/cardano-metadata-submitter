{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Config where

import Prelude

import Cardano.Slotting.Slot
    ( SlotNo (..) )
import qualified Options.Applicative as OA
import Data.Void (Void)
import Data.Foldable (asum)
import Control.Applicative ((<|>), optional, empty)
import Data.Text (Text)
import Cardano.Metadata.Types
    ( Subject (..)
    , WellKnownProperty (..)
    , emptyAttested
    )
import Data.List
    ( isSuffixOf )
import Cardano.Metadata.GoguenRegistry
    ( GoguenRegistryEntry (..)
    , PartialGoguenRegistryEntry
    )
import           Colog                         (Severity, pattern D, pattern I, pattern W, pattern E)

import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Text.Megaparsec               as P
import qualified Text.Megaparsec.Char          as P
import qualified Text.Megaparsec.Char.Lexer    as P
import qualified Data.Bifunctor as Bifunctor

type Parser = P.Parsec Void Text

data SlotNoPreference
  = MainnetTip
  | TestnetTip
  | CustomSlot SlotNo
  deriving (Eq, Show)

data DraftStatus
    = DraftStatusDraft
    | DraftStatusFinal
    deriving Show

data EntryOperation
    = EntryOperationInitialize
    | EntryOperationRevise
    deriving Show

data AttestationField
    = AttestationFieldName
    | AttestationFieldDescription
    | AttestationFieldLogo
    | AttestationFieldUrl
    | AttestationFieldUnit
    | AttestationFieldTicker
    deriving (Show, Eq, Ord)

data FileInfo = FileInfo
    { _FileInfoSubject :: Subject
    , _FileInfoEntryOperation :: EntryOperation
    , _FileInfoDraftStatus :: DraftStatus
    }
  deriving Show

data EntryUpdateArguments = EntryUpdateArguments
    { _EntryUpdateArgumentsFileInfo :: FileInfo
    , _EntryUpdateArgumentsAttestationKeyFilename :: Maybe String
    , _EntryUpdateArgumentsAttestationFields :: [AttestationField]
    , _EntryUpdateArgumentsRegistryEntry :: PartialGoguenRegistryEntry
    , _EntryUpdateLogoFilename :: Maybe String
    , _EntryUpdatePolicyFilenameOrCBOR :: Maybe String
    }
    deriving Show

data Arguments
    = ArgumentsEntryUpdate EntryUpdateArguments
    | ArgumentsValidate FilePath SlotNoPreference Severity
    deriving Show

argumentParser :: Maybe Subject -> OA.Parser Arguments
argumentParser defaultSubject =
  OA.hsubparser 
    ( OA.command "update" (OA.info (ArgumentsEntryUpdate <$> entryUpdateArgumentParser defaultSubject) mempty)
   <> OA.command "validate" (OA.info (ArgumentsValidate <$> pFile <*> pSlotNoPreference <*> pLogSeverity) mempty)
    )

  where
    pFile :: OA.Parser FilePath
    pFile = OA.strArgument (OA.metavar "FILE" <> OA.help "File to validate")

entryUpdateArgumentParser :: Maybe Subject -> OA.Parser EntryUpdateArguments
entryUpdateArgumentParser defaultSubject = EntryUpdateArguments
    <$> fileInfoArgumentParser
    <*> optional (OA.strOption (OA.long "attest-keyfile" <> OA.short 'a' <> OA.metavar "ATTESTATION_KEY_FILE"))
    <*> attestationFieldNamesParser
    <*> goguenRegistryEntryParser
    <*> logoFilenameParser
    <*> policyParser
  where
    attestationFieldNamesParser :: OA.Parser [AttestationField]
    attestationFieldNamesParser = asum
        [ OA.flag' [AttestationFieldName] $ OA.long "attest-name" <> OA.short 'N'
        , OA.flag' [AttestationFieldDescription] $ OA.long "attest-description" <> OA.short 'D'
        , OA.flag' [AttestationFieldLogo] $ OA.long "attest-logo" <> OA.short 'L'
        , OA.flag' [AttestationFieldUrl] $ OA.long "attest-url" <> OA.short 'H'
        , OA.flag' [AttestationFieldUnit] $ OA.long "attest-unit" <> OA.short 'U'
        , OA.flag' [AttestationFieldTicker] $ OA.long "attest-ticker" <> OA.short 'T'
        , pure
            [ AttestationFieldName
            , AttestationFieldDescription
            , AttestationFieldLogo
            , AttestationFieldUrl
            , AttestationFieldUnit
            , AttestationFieldTicker
            ]
       ]

    fileInfoArgumentParser :: OA.Parser FileInfo
    fileInfoArgumentParser = FileInfo
        <$> (trimSubject <$> OA.strArgument (OA.metavar "SUBJECT") <|> defaultSubjectParser)
        <*> OA.flag EntryOperationRevise EntryOperationInitialize (OA.long "init" <> OA.short 'i')
        <*> OA.flag DraftStatusDraft DraftStatusFinal (OA.long "finalize" <> OA.short 'f')

    defaultSubjectParser =
        maybe empty pure defaultSubject

    trimSubject :: String -> Subject
    trimSubject subj
        | jsonSuffix `isSuffixOf` subj =
            Subject $ T.pack $ take (length subj - length jsonSuffix) subj
        | jsonDraftSuffix `isSuffixOf` subj =
            Subject $ T.pack $ take (length subj - length jsonDraftSuffix) subj
        | otherwise =
            Subject $ T.pack subj

    logoFilenameParser :: OA.Parser (Maybe String)
    logoFilenameParser = optional $ OA.strOption (OA.long "logo" <> OA.short 'l' <> OA.metavar "LOGO.png")

    policyParser :: OA.Parser (Maybe String)
    policyParser = optional (OA.strOption (OA.long "policy" <> OA.short 'p' <> OA.metavar "POLICY"))

    goguenRegistryEntryParser :: OA.Parser (PartialGoguenRegistryEntry)
    goguenRegistryEntryParser = GoguenRegistryEntry Nothing Nothing
        <$> optional (emptyAttested <$> wellKnownOption (OA.long "name" <> OA.short 'n' <> OA.metavar "NAME"))
        <*> optional (emptyAttested <$> wellKnownOption (OA.long "description" <> OA.short 'd' <> OA.metavar "DESCRIPTION"))
        <*> pure Nothing -- logo
        <*> optional (emptyAttested <$> wellKnownOption (OA.long "url" <> OA.short 'h' <> OA.metavar "URL"))
        <*> optional (emptyAttested <$> wellKnownOption (OA.long "unit" <> OA.short 'u' <> OA.metavar "UNIT"))
        <*> optional (emptyAttested <$> wellKnownOption (OA.long "ticker" <> OA.short 't' <> OA.metavar "TICKER"))

pSlotNoPreference :: OA.Parser SlotNoPreference
pSlotNoPreference = 
  OA.option (readerFromParser pSlotPref)
    ( OA.long "slot-no"
    <> OA.help "Slot number to use to validate wallet metadata scripts (mainnet | testnet | <WORD64>)"
    )

pSlotPref :: Parser SlotNoPreference
pSlotPref = asum [ MainnetTip <$ P.string "mainnet"
                 , TestnetTip <$ P.string "testnet"
                 , CustomSlot <$> P.decimal
                 , pure MainnetTip
                 ]

readerFromParser :: Parser a -> OA.ReadM a
readerFromParser p =
  OA.eitherReader
    (  Bifunctor.first P.errorBundlePretty
     . P.runParser (p <* P.eof) "cmd-line-args"
     . T.pack
    )

pLogSeverity :: OA.Parser Colog.Severity
pLogSeverity = pDebug <|> pInfo <|> pWarning <|> pError <|> pure I
  where
    pDebug =
      OA.flag' D
        (  OA.long "debug"
        <> OA.help "Print debug, info, warning, and error messages"
        )
    pInfo =
      OA.flag' I
        (  OA.long "info"
        <> OA.help "Print info, warning, and error messages"
        )
    pWarning =
      OA.flag' W
        (  OA.long "warning"
        <> OA.help "Print warning, and error messages"
        )
    pError =
      OA.flag' E
        (  OA.long "error"
        <> OA.help "Print error messages only"
        )

canonicalFilename :: FileInfo -> String
canonicalFilename = (<> jsonSuffix) . T.unpack . unSubject . _FileInfoSubject

jsonSuffix, draftSuffix, jsonDraftSuffix :: String
jsonSuffix = ".json"
draftSuffix = ".draft"
jsonDraftSuffix = jsonSuffix <> draftSuffix

draftFilename :: FileInfo -> String
draftFilename fi = canonicalFilename fi <> draftSuffix

wellKnownOption
    :: forall p. WellKnownProperty p
    => OA.Mod OA.OptionFields p
    -> OA.Parser p
wellKnownOption =
    OA.option wellKnownReader
  where
    wellKnownReader :: OA.ReadM p
    wellKnownReader = OA.eitherReader $
        Aeson.parseEither parseWellKnown . Aeson.toJSON
