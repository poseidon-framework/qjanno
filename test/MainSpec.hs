module MainSpec (spec) where

import           Control.Applicative
import           Control.Monad
import           System.IO
import           System.Process
import           Test.Hspec          (Spec, describe, it, shouldReturn)

spec :: Spec
spec = qjannoSpec

qjannoSpec :: Spec
qjannoSpec =

  describe "qjanno" $ do

    let basic_tests = [
            "basic", "columns", "stdin", "where", "tab", "tab2",
            "count", "is_null", "not_null", "output_header", "spaces",
            "tab_delimited_output", "multiline",
            "query_file", "empty_query", "empty_query_file", "file_spaces",
            "avg", "sum", "avg_sum", "seq",
            "group", "group_sum", "concat", "join", "invalid"
            ]

    runTestScripts "basic" basic_tests

    let poseidon_tests = [
            "janno_d", "janno_da", "janno_j", "janno_d_da_j", "janno_ext",
            "janno_d_ext_ext", "show_columns", "janno_d_da_j_full"
            ]

    runTestScripts "poseidon" poseidon_tests

    where
        runTestScripts :: FilePath -> [String] -> Spec
        runTestScripts testdir tests = do
            forM_ tests $ \test -> do
                it ("should be executed correctly: " ++ test) $ do
                    let cp = (shell ("bash " ++ test ++ ".sh")) {
                          cwd = Just $ "test/tests/" ++ testdir,
                          std_out = CreatePipe,
                          std_err = CreatePipe
                        }
                    (_, Just out, Just err, _) <- createProcess cp
                    hSetBuffering out NoBuffering
                    hSetBuffering err NoBuffering
                    outExpected <- readFile $ "test/tests/" ++ testdir ++ "/" ++ test ++ ".out"
                    liftA2 (++) (hGetContents out) (hGetContents err) `shouldReturn` outExpected
