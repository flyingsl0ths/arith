import qualified Arith.Syntax.Utils as ASU
import Test.Hspec


main :: IO ()
main = hspec $ do
  describe "Arith.Syntax.Utils.peek" $ do
    it "returns the head of a non-empty list" $ do
      ASU.peek [1] `shouldBe` Just 1

  describe "Arith.Syntax.Utils.peekNext" $ do
    it "returns the second element of a non-empty list" $ do
      ASU.peekNext [1, 2, 3] `shouldBe` Just 2

  describe "Arith.Syntax.Utils.spanCount" $ do
    it "returns empty results and a count of zero when given an empty list" $ do
      ASU.spanCount [] id `shouldBe` ([], [], 0)

    it "returns ([], [], 0) when the predicate doesn't hold and length(list) == 1" $ do
      ASU.spanCount [1] even `shouldBe` ([], [], 0)

    it "returns ([c], [], 1) when the predicate holds and length(list) == 1" $ do
      ASU.spanCount [1] even `shouldBe` ([], [], 0)

    it "returns ([n1..N], [n(N + 1)..M], N) until the predicate holds and length(list) > 1" $ do
      ASU.spanCount [2, 4, 6, 8, 11, 13, 15] even `shouldBe` ([2, 4, 6, 8], [11, 13, 15], 4)
      ASU.spanCount [2, 4, 9, 8, 11, 13, 15] even `shouldBe` ([2, 4], [9, 8, 11, 13, 15], 2)
      ASU.spanCount "Hel-lo World" (/= '-') `shouldBe` ("Hel", "-lo World", 3)
