{-# LANGUAGE LambdaCase #-}

import Cardano.Api                         hiding (TxId)
import Data.String                         (IsString (..))
import Ledger
import Ledger.Bytes                        (getLedgerBytes)
import System.Environment                  (getArgs)

import Cardano.Plutus.TokenMinter

printResult :: String -> Either (FileError ()) () -> IO ()
printResult fileName = \case
    Left err -> print $ displayError err
    Right () -> putStrLn $ "wrote policy to file " ++ fileName

main :: IO ()
main = do
    let
        nftPolicyFile  = "scripts/nft-mint-policy.plutus"
        ftPolicyFile   = "scripts/ft-mint-policy.plutus"
    printResult nftPolicyFile =<< writeFileTextEnvelope nftPolicyFile Nothing nonFungibleMint
    printResult ftPolicyFile =<< writeFileTextEnvelope ftPolicyFile Nothing fungibleMint