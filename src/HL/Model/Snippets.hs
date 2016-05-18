-- |

module HL.Model.Snippets where

import           Control.Exception (throwIO)
import qualified Data.Yaml as Yaml
import           HL.Types

-- | Get the snippet from the config.
getSnippets :: IO SnippetInfo
getSnippets =
  Yaml.decodeFileEither "config/snippets.yaml" >>= either throwIO return
