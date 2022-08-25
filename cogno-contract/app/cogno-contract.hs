import Prelude
import Cardano.Api
import CognoContract ( cognoContractScript )

main :: IO ()
main = do
  result <- writeFileTextEnvelope "cogno-contract.plutus" Nothing cognoContractScript
  case result of
    Left err -> print $ displayError err
    Right () -> return ()
