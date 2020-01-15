import Test.Hspec
--import Test.Hspec.SmallCheck

import qualified EVM.Bytecode as EVM

round_trip :: EVM.Bytecode -> Expectation
round_trip bc =
  bc `shouldBe` (EVM.decode $ EVM.encode bc)

main :: IO ()
main = hspec $ do
  describe "Parser & Emitter" $ do
    it "round-trip" $ do
      --property round_trip
      round_trip []
      round_trip [EVM.STOP]
      round_trip [EVM.STOP, EVM.ADD]
