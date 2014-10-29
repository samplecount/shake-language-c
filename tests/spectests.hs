module Main where

import Control.Applicative hiding ((*>))
import Control.Monad
import Data.IORef
import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Language.C
import qualified Development.Shake.Language.C.Host as Host
import qualified System.Directory as Dir
import Test.Hspec

withShake :: String
          -> (FilePath
              -> (FilePath -> FilePath)
              -> (FilePath -> FilePath)
              -> Rules FilePath)
          -> IO String
withShake name mkRules = do
  ref <- newIORef undefined
  shake shakeOptions { shakeFiles = "build/" } $ do
    output <- mkRules name
                      (\x -> "build/tests" </> name </> "input" </> x)
                      (\x -> "build/tests" </> name </> "output" </> x)
    action $ do
      need [output]
      liftIO $ writeIORef ref output
  readFile =<< readIORef ref

shouldBeBuiltBy :: String
               -> (FilePath
               -> (FilePath -> FilePath)
               -> (FilePath -> FilePath)
               -> Rules FilePath)
               -> Expectation
shouldBeBuiltBy name mkRules =
  withShake name mkRules `shouldReturn` unlines [name]

cstring :: String -> String
cstring = show

main :: IO ()
main = hspec $ do
  runIO $ do
    b <- Dir.doesDirectoryExist "build"
    when b $ Dir.removeDirectoryRecursive "build"
  describe "Host toolchain" $ do
    it "compiles a C file to an executable" $ do
      "host_toolchain_compile_c" `shouldBeBuiltBy` \name mkInput mkOutput -> do
        let (_, toolChain) = Host.defaultToolChain
            input = mkInput "source.c"
            outputGen = mkOutput $ "result" <.> exe
            output = mkOutput "result.txt"
        input *> \path -> do
          writeFileLines path [
              "#include <stdio.h>"
            , "int main(int argc, char** argv)"
            , "{"
            , "    printf(\"%s\\n\", " ++ cstring name ++ ");"
            , "    return 0;"
            , "}"
            ]
        _ <- executable toolChain outputGen (pure id) (pure [input])
        output *> \path -> do
          need [outputGen]
          cmd Shell (outputGen ++ " > " ++ path)
        return output
    it "compiles a C++ file to an executable" $ do
      "host_toolchain_compile_cpp" `shouldBeBuiltBy` \name mkInput mkOutput -> do
        let (_, toolChain) = Host.defaultToolChain
            input = mkInput "source.cpp"
            outputGen = mkOutput $ "result" <.> exe
            output = mkOutput "result.txt"
        input *> \path -> do
          writeFileLines path [
              "#include <iostream>"
            , "int main(int argc, char** argv)"
            , "{"
            , "    std::cout << " ++ cstring name ++ " << std::endl;"
            , "    return 0;"
            , "}"
            ]
        _ <- executable toolChain outputGen (pure id) (pure [input])
        output *> \path -> do
          need [outputGen]
          cmd Shell (outputGen ++ " > " ++ path)
        return output
