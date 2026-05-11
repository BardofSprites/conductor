import Test.Hspec
import ParserSpec
import EvaluatorSpec

main :: IO ()
main = hspec $ do
  parserTests
  evaluatorTests
