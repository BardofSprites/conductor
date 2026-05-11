module ParserSpec where
import Test.Hspec
import Text.Megaparsec
import Command
import Commands.Sort (SortExpr(..), SortOrder(..))
import Commands.Limit (LimitExpr(..))
import Syntax (Field(..))

parserTests :: Spec
parserTests = describe "Command parser" $ do
  it "parses a sort command" $
    parse commandExprParser "" "sort(field=\"artist\", order=\"asc\")"
    `shouldBe`
    Right (CommandLeaf (SortCmd (SortExpr Artist Asc)))

  it "parses a limit command" $
    parse commandExprParser "" "limit(count=10)"
    `shouldBe`
    Right (CommandLeaf (LimitCmd (LimitExpr (Just 10) Nothing Nothing)))

  it "parses chained commands with &&" $
    parse commandExprParser "" "sort(field=\"year\", order=\"asc\") && limit(count=5)"
    `shouldBe`
    Right (CommandAnd
            (CommandLeaf (SortCmd (SortExpr Year Asc)))
            (CommandLeaf (LimitCmd (LimitExpr (Just 5) Nothing Nothing))))
