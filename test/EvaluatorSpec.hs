module EvaluatorSpec where
import Test.Hspec
import Song
import Syntax
import Commands.Sort
import Commands.Limit
import qualified Evaluators.SortEvaluator as S
import qualified Evaluators.LimitEvaluator as L

-- a few fake songs to test against
testSongs :: [Song]
testSongs =
  [ Song "/fake/get-lucky.mp3"    "Get Lucky"    "Daft Punk" "RAM"      "Funk"       2013 354
  , Song "/fake/around-world.mp3" "Around World" "Daft Punk" "Homework" "Electronic" 1997 429
  , Song "/fake/shiki.mp3"        "Shiki no Uta" "MINMI"     "Single"   "J-Pop"      2005 214
  ]

evaluatorTests :: Spec
evaluatorTests = describe "Evaluators" $ do
  it "sorts by year ascending" $
    map year (S.applySort (SortExpr Year Asc) testSongs)
    `shouldBe` [1997, 2005, 2013]

  it "limits by count" $
    length (L.applyLimit (LimitExpr (Just 2) Nothing Nothing) testSongs)
    `shouldBe` 2
