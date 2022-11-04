import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "DummyTest" $ do
        it "succeeds" $ do
            1 `shouldBe` 1
