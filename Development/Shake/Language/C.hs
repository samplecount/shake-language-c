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

{-|
Description: Build @C@ language projects for various target platforms
-}

module Development.Shake.Language.C (
  -- * Build targets
  -- $targets
    module Development.Shake.Language.C.Target
  -- * High-level build rules
  , module Development.Shake.Language.C.BuildFlags
  , module Development.Shake.Language.C.Rules
  -- * Toolchain utilities
  , applyEnv
  , toEnv
) where

import Development.Shake.Language.C.BuildFlags
import Development.Shake.Language.C.Rules
import Development.Shake.Language.C.Target
import Development.Shake.Language.C.ToolChain (
    applyEnv
  , toEnv
  )

{- $targets

This library's focus is on cross compilation. Here's a list of modules that
provide support for targeting specific platforms:

 * "Development.Shake.Language.C.Target.Android"
 * "Development.Shake.Language.C.Target.Linux"
 * "Development.Shake.Language.C.Target.NaCl"
 * "Development.Shake.Language.C.Target.OSX"
 * "Development.Shake.Language.C.Target.Windows"
-}
