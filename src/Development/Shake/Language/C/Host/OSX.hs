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

module Development.Shake.Language.C.Host.OSX (
  getHostToolChain
) where

import Control.Applicative hiding ((*>))
import Data.List.Split
import Data.Version
import Development.Shake
import Development.Shake.Language.C.Target
import Development.Shake.Language.C.Target.OSX
import Development.Shake.Language.C.ToolChain

-- | Get OSX system version (first two digits).
systemVersion :: Action Version
systemVersion = do
  Stdout version <- cmd "sw_vers" ["-productVersion"]
  return $ Version (map read . take 2 . splitOn "." $ version) []

-- | Get host toolchain.
getHostToolChain :: IO (Target, Action ToolChain)
getHostToolChain = do
    let defaultTarget = target macOSX (X86 X86_64)
    return ( defaultTarget
           , toolChain
               <$> getSDKRoot
               <*> systemVersion
               <*> pure defaultTarget )
