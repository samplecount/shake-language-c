-- Copyright 2012-2014 Samplecount S.L.
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

import           Development.Shake
import           Development.Shake.Language.C.Target
import           Development.Shake.Language.C.Target.Windows
import           Development.Shake.Language.C.ToolChain
import qualified System.Info as System

getHostArch :: IO Arch
getHostArch = do
    -- TODO: Get the info from the environment
    let arch = System.arch
    return $ case arch of
        "i386" -> X86 I386
        "i686" -> X86 I686
        "x86_64" -> X86 X86_64
        _ -> error $ "Unknown host architecture " ++ arch

getHostToolChain :: IO (Target, Action ToolChain)
getHostToolChain = do
    t <- fmap target getHostArch
    return (t, return $ toolChain Generic)
