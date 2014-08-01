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

module Shakefile.C (
    module Shakefile.C.BuildFlags
  , module Shakefile.C.Language
  , module Shakefile.C.Rules
  , module Shakefile.C.ToolChain
  , module Shakefile.C.Util
) where

import Shakefile.C.BuildFlags hiding (defaultBuildFlags)
import Shakefile.C.Language
import Shakefile.C.Rules
import Shakefile.C.ToolChain
import Shakefile.C.Util
