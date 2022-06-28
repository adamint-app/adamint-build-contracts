{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Cardano.Plutus.TokenMinter (nonFungibleMint, fungibleMint) where
import           Codec.Serialise
import           Cardano.Api.Shelley      (PlutusScript (..), PlutusScriptV1)
import Control.Lens (view)
import Control.Monad (void, when)
import Control.Monad.Error.Lens (throwing)
import Data.ByteString.Hash (blake2b)
import Data.Default (Default (def))
import Data.List.NonEmpty (nonEmpty, NonEmpty((:|)))
import Data.Map qualified as Map
import Data.Map (Map)
import Data.Text qualified as T
import Data.Void (Void)
import Data.Coerce (coerce)
import Data.ByteString.Lazy qualified as LB
import qualified Data.ByteString.Short    as SBS
import Ledger (Address, POSIXTime, POSIXTimeRange, Validator(..), getTxId)
--import Ledger.Address (PaymentPubKeyHash (unPaymentPubKeyHash), pubKeyAddress, pubKeyHashAddress)
import Ledger.Ada qualified as Ada
import Ledger.Constraints (TxConstraints, mustBeSignedBy, mustPayToTheScript, mustValidateIn)
import Ledger.Constraints qualified as Constraints
import Ledger.Constraints.OffChain ()
import Ledger.Contexts (ScriptContext (..), TxInfo (..))
import Ledger.Contexts qualified as Validation
import Ledger.Interval qualified as Interval
import Ledger.Scripts (dataHash, mkMintingPolicyScript, unMintingPolicyScript)
import Ledger.TimeSlot qualified as TimeSlot
import Ledger.Tx qualified as Tx
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value (Value)
import Ledger.Value qualified as Value
-- import Playground.Contract
-- import Plutus.Contract
-- import Plutus.Contract.Test
-- import Plutus.Contract.Typed.Tx qualified as Typed
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup (..), fold)
import PlutusTx.AssocMap qualified as PMap
import PlutusTx.Builtins
import Prelude as Haskell (Monoid(..), Semigroup (..), show, foldl1, fromIntegral, toInteger, undefined, error)
-- import Schema (ToSchema)
import Unsafe.Coerce

-- == Fungible token generator
-- When creating a token user receives control NFT and non-zero qty of required token
-- To mint additional tokens, user should posess any qty of the same token and control NFT
-- To burn tokens, user should posess qty to burn, and a control NFT
-- User can burn NFT
-- User can send NFT, effectively giving control over token to other user
-- == NFT generator
-- When creating a token user receives only control NFT
-- User can burn NFT

-- ===

-- {-# INLINABLE takeExactMap #-}
-- takeExactMap :: Integer -> Map k a -> Maybe (Map k a)
-- takeExactMap n m = let h = Map.take (fromIntegral n) m in if toInteger (Map.size h) == n then Just h else Nothing

-- {-# INLINABLE first #-}
-- first :: (a -> a') -> (a, b) -> (a', b)
-- first f (a, b) = (f a, b)

--{-# INLINABLE fromJust #-}
-- fromJust Nothing  = fix id -- traceError "Maybe.fromJust: Nothing" -- yuck
--     where
--         fix :: (a -> a) -> a
--         fix f = let x = f x in x
-- fromJust (Just x) = x

-- https://github.com/input-output-hk/plutus/issues/3657#issuecomment-954841099
-- Convert from an integer to its text representation. Example: 123 => "123"
-- {-# INLINEABLE integerToBS #-}
-- integerToBS :: Integer -> BuiltinByteString
-- integerToBS x
--   -- 45 is ASCII code for '-'
--   | x < 0 = consByteString 45 $ integerToBS (negate x)
--   -- x is single-digit
--   | x `quotient` 10 == 0 = digitToBS x
--   | otherwise = integerToBS (x `quotient` 10) `appendByteString` digitToBS (x `remainder` 10)
--   where
--     digitToBS :: Integer -> BuiltinByteString
--     -- 48 is ASCII code for '0'
--     digitToBS d = consByteString (d + 48) emptyByteString

-- {-# INLINEABLE integerToRawBS #-}
-- integerToRawBS :: Integer -> Integer -> BuiltinByteString
-- integerToRawBS base x
--   | x < 0 = consByteString 0 $ integerToRawBS base (negate x)
--   -- x is single-digit
--   | x `quotient` base == 0 = digitToBS x
--   | otherwise = integerToRawBS base (x `quotient` base) `appendByteString` digitToBS (x `remainder` base)
--   where
--     digitToBS :: Integer -> BuiltinByteString
--     digitToBS d = consByteString d emptyByteString

-- foldrByteString :: (Integer -> b -> b) -> b -> BuiltinByteString -> b
-- foldrByteString f start bytes = loop (length bytes)
--     where
--         loop n = let m = n - 1 in f (indexByteString bytes m) (if m == 0 then start else loop m)

-- foldrByteString :: (Integer -> b -> b) -> b -> BuiltinByteString -> b
-- foldrByteString f start bytes = loop 0
--     where
--         l = lengthOfByteString bytes
--         loop n = let m = n + 1 in f (indexByteString bytes n) (if m < l then loop m else start)

-- encodeHex :: BuiltinByteString -> BuiltinByteString
-- encodeHex = foldrByteString (consHex . hexOf) emptyByteString
--     where
--         consHex (a, b) = consByteString a . consByteString b
--         hexOf n = let (d, m) = n `divMod` 16 in (hexSymbol d, hexSymbol m)
--         hexSymbol :: Integer -> Integer
--         hexSymbol b = if b < 10 then b + 48 else b + 87

{-# INLINABLE outRefToHash #-}
outRefToHash :: Tx.TxOutRef -> BuiltinByteString
outRefToHash Tx.TxOutRef {..} =
   -- sha3_256 $ getTxId txOutRefId `appendByteString` integerToRawBS 256 txOutRefIdx
   consByteString txOutRefIdx $ sliceByteString 0 31 $ getTxId txOutRefId

{-# INLINABLE getOwnTokens #-}
getOwnTokens ctx = concat . fmap PMap.toList . PMap.lookup (Validation.ownCurrencySymbol ctx) . Value.getValue

{-# INLINABLE consumedUtxoForName #-}
consumedUtxoForName tokenName info = (\i -> outRefToHash (Validation.txInInfoOutRef i) == Value.unTokenName tokenName) `any` txInfoInputs info

-- ===

{-# INLINABLE mkNonFungiblePolicy #-}
-- This policy has no arguments, and checks if there is an input UTxO for each forged NFT with corresponding currency symbol
mkNonFungiblePolicy :: () -> ScriptContext -> Bool
mkNonFungiblePolicy () ctx@ScriptContext { scriptContextTxInfo = info } =
    and . map mintAllowed . getOwnTokens ctx $ txInfoMint info
    where
        mintAllowed (tokenName, tokenQty)
          | tokenQty == 1 = consumedUtxoForName tokenName info
          | otherwise = tokenQty == -1

{-# INLINABLE nonFungiblePolicy #-}
nonFungiblePolicy :: Scripts.MintingPolicy
nonFungiblePolicy = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy mkNonFungiblePolicy ||])

{-# INLINABLE nonFungibleSymbol #-}
nonFungibleSymbol :: Value.CurrencySymbol
nonFungibleSymbol = Validation.scriptCurrencySymbol nonFungiblePolicy

-- ===

{-# INLINABLE mkFungiblePolicy #-}
mkFungiblePolicy :: Value.CurrencySymbol -> () -> ScriptContext -> Bool
mkFungiblePolicy nftSymbol () ctx@ScriptContext { scriptContextTxInfo = info } =
    and . map mintAllowed . getOwnTokens ctx $ txInfoMint info
    where
        mintAllowed (tokenName, _) = (hasSameToken && hasControlNft) || consumedUtxo
            where
                consumedUtxo = consumedUtxoForName tokenName info
                spent = Validation.valueSpent info
                hasSameToken = Value.valueOf spent (Validation.ownCurrencySymbol ctx) tokenName > 0
                hasControlNft = Value.valueOf spent nftSymbol tokenName == 1

fungiblePolicy :: Value.CurrencySymbol -> Scripts.MintingPolicy
fungiblePolicy nftSymbol = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkFungiblePolicy ||])
    `PlutusTx.applyCode` PlutusTx.liftCode nftSymbol

fungibleSymbol :: Value.CurrencySymbol -> Value.CurrencySymbol
fungibleSymbol = Validation.scriptCurrencySymbol . fungiblePolicy

-- ===

-- data MintFtParams = MintFtParams
--     {
--         mintFtName :: BuiltinByteString
--       , mintFtControl :: Bool
--       , mintFtQty :: Integer
--     }
--     deriving stock Generic
--     deriving anyclass (ToJSON, FromJSON, ToSchema)

-- newtype MintNftParams = MintNftParams { mintNftQty :: Integer }
--     deriving stock Generic
--     deriving anyclass (ToJSON, FromJSON, ToSchema)

-- newtype BurnNftParams = BurnNftParams { burnNftNames :: [BuiltinByteString] }
--     deriving stock Generic
--     deriving anyclass (ToJSON, FromJSON, ToSchema)

-- type GeneratorSchema =
--         Endpoint "mintFt"  MintFtParams
--     .\/ Endpoint "mintNft" MintNftParams
--     .\/ Endpoint "burnNft" BurnNftParams
-- mkSchemaDefinitions ''GeneratorSchema

-- mintFt :: MintFtParams -> Contract w GeneratorSchema T.Text ()
-- mintFt MintFtParams {..} = do
--     (tokenName, lookups', tx') <- case mintFtName of
--         "" -> do
--             (tokenNames, lookups', tx') <- (if mintFtControl then mintNfts else return) =<< spendNftUtxos 1
--             tokenName <- case tokenNames of
--                 [tokenName] -> return tokenName
--                 _ -> throwing _ContractError "prepareNfts error"
--             return (tokenName, lookups', tx')
--         tokenName -> do
--             let
--                 controlNft = Value.singleton nonFungibleSymbol (TokenName tokenName) 1
--                 tx' = Constraints.mustSpendAtLeast controlNft
--             return (tokenName, Haskell.mempty, tx')
--     let val     = Value.singleton (fungibleSymbol nonFungibleSymbol) (TokenName tokenName) mintFtQty
--         lookups = Constraints.mintingPolicy (fungiblePolicy nonFungibleSymbol) <> lookups'
--         tx      = Constraints.mustMintValue val <> tx'
--     void $ submitTxConstraintsWith @Void lookups tx

-- mintNft :: MintNftParams -> Contract w GeneratorSchema T.Text ()
-- mintNft MintNftParams {..} = do
--     (_, lookups, tx) <- mintNfts =<< spendNftUtxos mintNftQty
--     void $ submitTxConstraintsWith @Void lookups tx

-- burnNft :: BurnNftParams -> Contract w GeneratorSchema T.Text ()
-- burnNft BurnNftParams {..} = do
--     (lookups, tx) <- burnNfts burnNftNames
--     void $ submitTxConstraintsWith @Void lookups tx

-- {-# INLINABLE spendNftUtxos #-}
-- spendNftUtxos mintNftQty = do
--     ownKey <- ownPaymentPubKeyHash
--     utxos <- utxosAt (pubKeyHashAddress ownKey Nothing)
--     usedUtxos <- maybe (throwing _ContractError "not enough utxos") return (takeExactMap mintNftQty utxos)
--     let
--         usedOrefs = Map.keys usedUtxos
--         usedNames = fmap outRefToHash usedOrefs
--         lookups = Constraints.unspentOutputs usedUtxos
--         tx = foldl1 (<>) $ map Constraints.mustSpendPubKeyOutput usedOrefs
--     return (usedNames, lookups, tx)

-- {-# INLINABLE mintNfts #-}
-- mintNfts (usedNames, lookups', tx') = do
--     let
--         val = foldl1 (<>) $ (\name -> Value.singleton nonFungibleSymbol (TokenName name) 1) <$> usedNames
--         lookups = Constraints.mintingPolicy nonFungiblePolicy
--         tx = Constraints.mustMintValue val
--     return (usedNames, lookups <> lookups', tx <> tx')

-- {-# INLINABLE burnNfts #-}
-- burnNfts nfts = do
--     ownKey <- ownPaymentPubKeyHash
--     utxos <- utxosAt (pubKeyHashAddress ownKey Nothing)
--     let
--         vals = foldl1 (<>) $ (\name -> Value.singleton nonFungibleSymbol (TokenName name) (-1)) <$> nfts
--         lookups = Constraints.mintingPolicy nonFungiblePolicy <> Constraints.unspentOutputs utxos
--         tx = Constraints.mustMintValue vals
--     return (lookups, tx)

-- mintFtR :: Promise () GeneratorSchema T.Text ()
-- mintFtR = endpoint @"mintFt" mintFt

-- mintNftR :: Promise () GeneratorSchema T.Text ()
-- mintNftR = endpoint @"mintNft" mintNft

-- burnNftR :: Promise () GeneratorSchema T.Text ()
-- burnNftR = endpoint @"burnNft" burnNft

-- endpoints :: Contract () GeneratorSchema T.Text ()
-- endpoints = selectList [mintFtR, mintNftR, burnNftR]

-- $(mkKnownCurrencies [])

-- ===

mintingPolicyAsCbor :: Scripts.MintingPolicy -> LB.ByteString
mintingPolicyAsCbor = serialise . Validator . unMintingPolicyScript

toSerialized :: LB.ByteString -> PlutusScript PlutusScriptV1
toSerialized = PlutusScriptSerialised . SBS.toShort . LB.toStrict

nonFungibleMint :: PlutusScript PlutusScriptV1
nonFungibleMint = toSerialized $ mintingPolicyAsCbor nonFungiblePolicy

fungibleMint :: PlutusScript PlutusScriptV1
fungibleMint = toSerialized $ mintingPolicyAsCbor (fungiblePolicy nonFungibleSymbol)