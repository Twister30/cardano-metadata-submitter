{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Cardano.Metadata.Types
  ( Subject (..)
  , hashSubject
  , Property (..)
  , hashProperty
  , PropertyValue (..)
  , propertyValueFromString
  , propertyValueToString
  , propertyValueToJson
  , hashPropertyValue
  , AttestationSignature (..)
  , makeAttestationSignature
  , HashesForAttestation (..)
  , hashesForAttestation
  , attestationDigest
  , Attested (..)
  , isAttestedBy
  , verifyAttested
  , WellKnownProperty (..)
  , WellKnown (..)
  , parseWellKnown'
  , withWellKnown
  , Name (..)
  , Description (..)
  , Preimage (..)
  , OwnershipSignature (..)
  , HashesForOwnership (..)
  , WithOwnership (..)
  ) where

import Cardano.Prelude

import Control.Category (id)
import Control.Monad.Fail
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A hiding (parseEither)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Cardano.Crypto.DSIGN
import Cardano.Crypto.Hash

import qualified AesonHelpers as A

newtype Subject = Subject { unSubject :: Text }
  deriving Show

hashSubject :: Subject -> Hash Blake2b_256 Subject
hashSubject = hashWith (T.encodeUtf8 . unSubject)

newtype Property = Property { unProperty :: Text }
  deriving (Show, Eq, Ord)

hashProperty :: Property -> Hash Blake2b_256 Property
hashProperty = hashWith (T.encodeUtf8 . unProperty)

-- | Metadata property values must be parseable as JSON, but they are utf-8 encoded strings.
data PropertyValue = PropertyValue
  { _propertyValue_raw :: Text
  , _propertyValue_parse :: A.Value
  }
 deriving Show

-- | Smart constructor guards creation of 'PropertyValue's so that we can
-- verify that they are valid JSON. 'Left' signals an aeson error.
propertyValueFromString :: Text -> Either Text PropertyValue
propertyValueFromString t = bimap T.pack (PropertyValue t) $
  A.eitherDecodeStrict @A.Value (T.encodeUtf8 t)

propertyValueToString :: PropertyValue -> Text
propertyValueToString = _propertyValue_raw

propertyValueToJson :: PropertyValue -> A.Value
propertyValueToJson = _propertyValue_parse

hashPropertyValue :: PropertyValue -> Hash Blake2b_256 PropertyValue
hashPropertyValue = hashWith (T.encodeUtf8 . propertyValueToString)

-- | An 'AttestationSignature is a pair of a public key and a signature
-- that can be verified with that public key of a message derived from
-- the subject, property name, and property value being attested to.
--
-- In particular, the message is hash(hash(subject) + hash (property_name) + hash(property_value))
data AttestationSignature = AttestationSignature
  { _attestationSignature_publicKey :: VerKeyDSIGN Ed25519DSIGN
  , _attestationSignature_signature :: SigDSIGN Ed25519DSIGN
  } deriving Show

makeAttestationSignature
  :: SignKeyDSIGN Ed25519DSIGN
  -> HashesForAttestation
  -> AttestationSignature
makeAttestationSignature signingKey hfa = AttestationSignature
  { _attestationSignature_publicKey = deriveVerKeyDSIGN signingKey
  , _attestationSignature_signature = signDSIGN ()
    (hashToBytes $ attestationDigest hfa)
    signingKey
  }

-- | Hashes required to produce a message for attestation purposes
data HashesForAttestation = HashesForAttestation
  { _hashesForAttestation_subject :: Hash Blake2b_256 Subject
  , _hashesForAttestation_property :: Hash Blake2b_256 Property
  , _hashesForAttestation_value :: Hash Blake2b_256 PropertyValue
  }

hashesForAttestation
 :: Subject
 -> Property
 -> PropertyValue
 -> HashesForAttestation
hashesForAttestation s p v = HashesForAttestation
  { _hashesForAttestation_subject = hashSubject s
  , _hashesForAttestation_property = hashProperty p
  , _hashesForAttestation_value = hashPropertyValue v
  }

attestationDigest
  :: HashesForAttestation
  -> Hash Blake2b_256 HashesForAttestation
attestationDigest hfa = castHash $ hashWith id $ mconcat
  [ hashToBytes $ _hashesForAttestation_subject hfa
  , hashToBytes $ _hashesForAttestation_property hfa
  , hashToBytes $ _hashesForAttestation_value hfa
  ]

-- | Metadata entries can be provided along with annotated signatures
-- attesting to the validity of those entry values.
data Attested a = Attested
  { _attested_signatures :: [AttestationSignature]
  , _attested_property :: a
  } deriving (Functor, Show)

isAttestedBy
  :: HashesForAttestation
  -> AttestationSignature
  -> Either Text ()
isAttestedBy hashes sig = bimap T.pack id $ verifyDSIGN ()
  (_attestationSignature_publicKey sig)
  (hashToBytes (attestationDigest hashes))
  (_attestationSignature_signature sig)

verifyAttested
  :: Attested HashesForAttestation
  -> Either [AttestationSignature] ()
verifyAttested attested =
  let (invalids, _) = partitionEithers $ flip fmap (_attested_signatures attested) $ \sig ->
         bimap (const sig) id $ isAttestedBy (_attested_property attested) sig
   in case invalids of
        [] -> Right ()
        _ -> Left invalids

class WellKnownProperty p where
  wellKnownPropertyName :: f p -> Property
  parseWellKnown :: PropertyValue -> A.Parser p

data WellKnown p where
  WellKnown :: WellKnownProperty p =>
    { _wellKnown_raw :: PropertyValue
    , _wellKnown_structured :: p
    } -> WellKnown p

deriving instance Show p => Show (WellKnown p)

parseWellKnown'
  :: WellKnownProperty p
  => A.Value
  -> A.Parser (WellKnown p)
parseWellKnown' =
  let parse t = case propertyValueFromString t of
        Left err -> fail (T.unpack err)
        Right v -> WellKnown v <$> parseWellKnown v
   in A.withText "property value" parse

withWellKnown
  :: WellKnownProperty p
  => (Property -> PropertyValue -> x)
  -> WellKnown p
  -> x
withWellKnown f p = f (wellKnownPropertyName p) (_wellKnown_raw p)

-- | "name" is a well-known property whose value must be a string
newtype Name = Name { unName :: Text }
  deriving Show

instance WellKnownProperty Name where
  wellKnownPropertyName _ = Property "name"
  parseWellKnown = A.withText "name" (pure . Name) . propertyValueToJson

-- | "description" is a well-known property whose value must be a string
newtype Description = Description { unDescription :: Text }
  deriving Show

instance WellKnownProperty Description where
  wellKnownPropertyName _ = Property "description"
  parseWellKnown = A.withText "description" (pure . Description) . propertyValueToJson

-- | "preimage" is a well-known property whose value must be a record two
-- fields: "hashFn" which names the hash function used to derive the metadata
-- subject and "preimage" which is the preimage of the hash subject under that
-- hash function.
--
-- NB: Metadata subjects are frequently, but not always, hashes
data Preimage = Preimage
  { _preimage_preimage :: Text
  , _preimage_hashFn :: Text
  } deriving Show

instance WellKnownProperty Preimage where
  wellKnownPropertyName _ = Property "preimage"
  parseWellKnown =
    let jsonParser = A.withObject "preimage" $ \o -> do
          hashFn <- A.prependFailure "preimage " $
            o A..: "hashFn"
          preimage <- A.prependFailure "preimage " $
            o A..: "preimage"
          A.noOtherFields "preimage" o ["hashFn", "preimage"]
          pure $ Preimage
            { _preimage_hashFn = hashFn
            , _preimage_preimage = preimage
            }
     in jsonParser . propertyValueToJson

-- | The goguen-metadata-registry determines ownership by signing entries
-- with Ed25519.
data OwnershipSignature = OwnershipSignature
  { _OwnershipSignature_publicKey :: VerKeyDSIGN Ed25519DSIGN
  , _OwnershipSignature_signature :: SigDSIGN Ed25519DSIGN
  } deriving Show

-- | Ownership signatures are hash( hash(subject)
-- + hash(property_name_1) + hash(property_value_1)
-- + hash(attestation_sig_1_1) + ... + hash(attestation_signature_1_k1)
-- + ... + hash(property_name_n) + hash(property_value_n)
-- + hash(attestation_sig_n_1) + ... + hash(attestation_sig_n_kn)
-- )
--
-- where properties are lexicographically ordered by name and signatures are
-- lexicographically ordered by their attesting key. That's what the 'Map's are for.
data HashesForOwnership = HashesForOwnership
  { _hashesForOwnership_subject :: Hash Blake2b_256 Subject
  , _hashesForOwnership_properties ::
    Map Property
      ( Hash Blake2b_256 Property
      , Hash Blake2b_256 PropertyValue
      , Map (VerKeyDSIGN Ed25519DSIGN)
          (Hash Blake2b_256 (SignedDSIGN Ed25519DSIGN (Hash Blake2b_256 HashesForAttestation)))
      )
  } deriving Show

data WithOwnership a = WithOwnership
  { _withOwnership_owner :: OwnershipSignature
  , _withOwnership_value :: a
  } deriving Show

