-- Copyright 2012-2016 Samplecount S.L.
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

module Development.Shake.Language.C.Host.Windows (
  getHostToolChain
) where

import           Data.Char (isSpace, toLower)
import           Development.Shake
import           Development.Shake.Language.C.Target
import           Development.Shake.Language.C.Target.Windows
import           Development.Shake.Language.C.ToolChain

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse

getHostArch :: IO Arch
getHostArch = do
    Stdout out <- cmd "wmic os get osarchitecture"
    let spec = map trim . lines $ out
    case map (map toLower) spec of
        ("osarchitecture":"32-bit":_) -> return $ X86 I686
        ("osarchitecture":"64-bit":_) -> return $ X86 X86_64
        ("osarchitecture":arch:_) -> error $ "Unknown host architecture " ++ arch
        _ -> error $ "Couldn't determine host architecture from " ++ show spec

getHostToolChain :: IO (Target, Action ToolChain)
getHostToolChain = do
    t <- fmap target getHostArch
    return (t, return $ toolChain Generic)
