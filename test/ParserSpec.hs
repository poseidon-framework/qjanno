module ParserSpec (spec) where

import           Data.Either
import qualified Data.Map      as Map
import           Test.Hspec    (Spec, describe, it, shouldBe, shouldSatisfy)

import           Qjanno.Parser

spec :: Spec
spec = do
  replaceTableNamesSpec
  roughlyExtractTableNamesSpec
  replaceBackTableNamesSpec
  extractTableNamesSpec

replaceTableNamesSpec :: Spec
replaceTableNamesSpec =
  describe "replaceTableNames" $ do

    it "should replace the file names with table names" $ do
      let query = "SELECT * FROM ./table0 WHERE c0 > 0"
      let expected = ("SELECT * FROM table0 WHERE c0 > 0",
                      Map.fromList [("./table0", "table0")])
      replaceTableNames query `shouldBe` expected

    it "should replace multiple file names with table names" $ do
      let query = "SELECT * FROM ./src/table0.csv\nJOIN /tmp/table1.csv\tON c1 = c2 WHERE c0 > 0"
      let expected = ("SELECT * FROM srctable0\nJOIN tmptable1\tON c1 = c2 WHERE c0 > 0",
                      Map.fromList [("./src/table0.csv", "srctable0"),("/tmp/table1.csv", "tmptable1")])
      replaceTableNames query `shouldBe` expected

    it "should replace only the table names" $ do
      let query = "SELECT * FROM ./table0 WHERE c0 LIKE '%foo ./table0 bar%'"
      let expected = ("SELECT * FROM table0 WHERE c0 LIKE '%foo ./table0 bar%'",
                      Map.fromList [("./table0", "table0")])
      replaceTableNames query `shouldBe` expected

    it "should take care of multi-byte file names correctly" $ do
      let query = "SELECT * FROM ./テスト"
      let expected = ("SELECT * FROM テスト",
                      Map.fromList [("./テスト", "テスト")])
      replaceTableNames query `shouldBe` expected

    it "should not replace inside single quotes" $ do
      let query = "SELECT * FROM table0.csv WHERE c0 LIKE '%foo FROM table1.csv bar%'"
      let expected = ("SELECT * FROM table0 WHERE c0 LIKE '%foo FROM table1.csv bar%'",
                      Map.fromList [("table0.csv", "table0")])
      replaceTableNames query `shouldBe` expected

    it "should not replace inside double quotes" $ do
      let query = "SELECT * FROM table0.csv WHERE c0 LIKE \"%foo FROM table0.csv bar%\""
      let expected = ("SELECT * FROM table0 WHERE c0 LIKE \"%foo FROM table0.csv bar%\"",
                      Map.fromList [("table0.csv", "table0")])
      replaceTableNames query `shouldBe` expected

    it "should replace the file name containing spaces" $ do
      let query = "SELECT * FROM `foo/bar baz qux/quux.csv`"
      let expected = ("SELECT * FROM foobarbazquxquux",
                      Map.fromList [("`foo/bar baz qux/quux.csv`", "foobarbazquxquux")])
      replaceTableNames query `shouldBe` expected

roughlyExtractTableNamesSpec :: Spec
roughlyExtractTableNamesSpec =
  describe "roughlyExtractTableNames" $ do

    it "should roughly extract table name" $ do
      roughlyExtractTableNames "SELECT * FROM table0 WHERE c0 > 0" `shouldBe` [ "table0" ]
      roughlyExtractTableNames "select * from table0 where c0 > 0" `shouldBe` [ "table0" ]

    it "should roughly extract multiple table names" $ do
      roughlyExtractTableNames "SELECT * FROM table0 JOIN table1 ON c1 = c2 WHERE c0 > 0" `shouldBe` [ "table0", "table1" ]
      roughlyExtractTableNames "select * from table0 join table1 on c1 = c2 where c0 > 0" `shouldBe` [ "table0", "table1" ]

    it "should roughly extract table names but ignore inside quotes" $ do
      roughlyExtractTableNames "SELECT * FROM table0 JOIN table1 ON c1 = c2 WHERE c3 LIKE 'A FROM table3 '" `shouldBe` [ "table0", "table1" ]
      roughlyExtractTableNames "select * from table0 join table1 on c1 = c2 where c3 like 'a from table3 '" `shouldBe` [ "table0", "table1" ]

    it "should roughly extract quoted table names" $ do
      roughlyExtractTableNames "SELECT * FROM `src/table 0 .csv` JOIN '/tmp/table 1.csv'" `shouldBe` [ "`src/table 0 .csv`", "'/tmp/table 1.csv'" ]

replaceBackTableNamesSpec :: Spec
replaceBackTableNamesSpec =
  describe "replaceBackTableNames" $ do

    it "should replace back table names to the file names" $ do
      let query = "SELECT * FROM ./table0 WHERE c0 > 0"
      let (query', tableMap) = replaceTableNames query
      replaceBackTableNames tableMap query' `shouldBe` query

    it "should replace back multiple table names with the file names" $ do
      let query = "SELECT * FROM ./src/table0.csv\nJOIN /tmp/table1.csv\tON c1 = c2 WHERE c0 > 0"
      let (query', tableMap) = replaceTableNames query
      replaceBackTableNames tableMap query' `shouldBe` query

    it "should replace back only the table names" $ do
      let query = "SELECT * FROM ./table0 WHERE c0 LIKE '%foo d8dc7ec0 bar%'"
      let (query', tableMap) = replaceTableNames query
      replaceBackTableNames tableMap query' `shouldBe` query

    it "should replace back the table name to the file name containing spaces" $ do
      let query = "SELECT * FROM `foo/bar baz qux/quux.csv`"
      let (query', tableMap) = replaceTableNames query
      replaceBackTableNames tableMap query' `shouldBe` query

extractTableNamesSpec :: Spec
extractTableNamesSpec =
  describe "extractTableNames" $ do

    it "should extract table name" $ do
      extractTableNames "SELECT * FROM table0 WHERE c0 > 0" "" `shouldBe` Right [ "table0" ]
      extractTableNames "select * from table0 where c0 > 0" "" `shouldBe` Right [ "table0" ]

    it "should extract multiple table names" $ do
      extractTableNames "SELECT * FROM table0 JOIN table1 ON c1 = c2 WHERE c0 > 0" "" `shouldBe` Right [ "table0", "table1" ]
      extractTableNames "select * from table0 join table1 on c1 = c2 where c0 > 0" "" `shouldBe` Right [ "table0", "table1" ]

    it "should return a parse error" $ do
      extractTableNames "SELECT ** FROM table0 WHERE c0 > 0" "" `shouldSatisfy` isLeft
      extractTableNames "SELECT * FROM table0 WHERE > 0" "" `shouldSatisfy` isLeft
