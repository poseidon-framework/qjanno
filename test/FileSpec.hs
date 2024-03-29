module FileSpec (spec) where

import           Data.Char     (isSpace)
import           Test.Hspec    (Spec, describe, it, shouldBe, shouldReturn)

import           Qjanno.File
import           Qjanno.Option

spec :: Spec
spec = do
  readFromFileSpec
  detectSplitterSpec
  splitFixedSizeSpec

readFromFileSpec :: Spec
readFromFileSpec =
  describe "readFromFile" $ do
    let opts = Option { query = Nothing,
                        queryFile = Nothing,
                        showColumns = False,
                        tabDelimited = False,
                        delimiter = Nothing,
                        noHeader = False,
                        outputRaw = True,
                        outputNoHeader = False }

    it "should read from a test file" $ do
      let expected = (
            ["source_file", "foo", "bar", "baz"],
            [["test/tests/basic/data/basic.csv", "a0", "1", "a2"],
             ["test/tests/basic/data/basic.csv", "b0", "3", "b2"],
             ["test/tests/basic/data/basic.csv", "c0", "", "c2"]]
            )
      readFromFile opts "test/tests/basic/data/basic.csv" `shouldReturn` expected

    it "should read from a test file which contains a multiline cell" $ do
      let expected = (
            ["source_file", "foo", "bar", "baz", "qux", "quux"],
            [["test/tests/basic/data/multiline.csv", "a0", "1", "a2\nb0\",3,\"b2\nc0", "", "c2"]]
            )
      readFromFile opts "test/tests/basic/data/multiline.csv" `shouldReturn` expected

detectSplitterSpec :: Spec
detectSplitterSpec =
  describe "detectSplitter" $ do

    it "should detect the column splitter space" $ do
      let (headLine, secondLine) = ("c0 c1 c2 c3 c4", "0 1 2 3 4")
      detectSplitter headLine secondLine ' ' `shouldBe` True
      detectSplitter headLine secondLine '\t' `shouldBe` True

    it "should detect the column splitter comma" $ do
      let (headLine, secondLine) = ("c0,c1,c2,c3,c4", "0,1,2,3,4")
      detectSplitter headLine secondLine ',' `shouldBe` True

    it "should detect the column splitter comma even if the column title has spaces" $ do
      let (headLine, secondLine) = ("foo bar baz,qux quux,hoge huga,cmd", "100,200,300,foo bar baz qux")
      detectSplitter headLine secondLine ',' `shouldBe` True

splitFixedSizeSpec :: Spec
splitFixedSizeSpec =
  describe "splitFixedSize" $ do

    it "should split the String with isSpace" $ do
      let (input, expected) = ("c0 c1 c2", [ "c0", "c1", "c2" ])
      splitFixedSize isSpace 0 input `shouldBe` expected

    it "should ignore the successive spaces when splitting with isSpace" $ do
      let (input, expected) = ("c0 c1   c2   \t\t c3", [ "c0", "c1", "c2", "c3" ])
      splitFixedSize isSpace 0 input `shouldBe` expected

    it "should take the column size into account when splitting with isSpace" $ do
      let (input, (n1, expected1), (n2, expected2), (n3, expected3), (n4, expected4))
            = ("c0 c1   c2   \t\t c3  c4  c5  ",
               (1, [ "c0 c1   c2   \t\t c3  c4  c5  " ]),
               (4, [ "c0", "c1", "c2", "c3  c4  c5  " ]),
               (6, [ "c0", "c1", "c2", "c3", "c4", "c5  " ]),
               (9, [ "c0", "c1", "c2", "c3", "c4", "c5", "", "", "" ]))
      splitFixedSize isSpace n1 input `shouldBe` expected1
      splitFixedSize isSpace n2 input `shouldBe` expected2
      splitFixedSize isSpace n3 input `shouldBe` expected3
      splitFixedSize isSpace n4 input `shouldBe` expected4

    it "should split the String with (==',')" $ do
      let (input, expected) = ("c0,c1,c2", [ "c0", "c1", "c2" ])
      splitFixedSize (==',') 0 input `shouldBe` expected

    it "should not ignore the successive commas when splitting with (==',')" $ do
      let (input, expected) = ("c0,c1,c2,,c3,,,c4", [ "c0", "c1", "c2", "", "c3", "", "", "c4" ])
      splitFixedSize (==',') 0 input `shouldBe` expected

    it "should take the column size into account when splitting with (==',')" $ do
      let (input, (n1, expected1), (n2, expected2), (n3, expected3), (n4, expected4))
            = ("c0,c1,,c2,foo bar baz,c4",
               (1, [ "c0,c1,,c2,foo bar baz,c4" ]),
               (4, [ "c0", "c1", "", "c2,foo bar baz,c4" ]),
               (6, [ "c0", "c1", "", "c2", "foo bar baz", "c4" ]),
               (9, [ "c0", "c1", "", "c2", "foo bar baz", "c4", "", "", "" ]))
      splitFixedSize (==',') n1 input `shouldBe` expected1
      splitFixedSize (==',') n2 input `shouldBe` expected2
      splitFixedSize (==',') n3 input `shouldBe` expected3
      splitFixedSize (==',') n4 input `shouldBe` expected4
