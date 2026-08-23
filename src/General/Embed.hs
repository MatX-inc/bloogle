{-# LANGUAGE TemplateHaskell #-}

-- | The @html/@ assets, baked into the binary at compile time so the
--   installed executable is fully self-contained. The server uses these
--   unless @--datadir@ points it back at an on-disk copy (see
--   'Action.Server').
module General.Embed (embeddedHtml, embeddedHtmlFile) where

import qualified Data.ByteString as BS
import Data.FileEmbed
import Data.Maybe

-- | Every file under @html/@, keyed by its path relative to @html/@
--   (e.g. @index.html@).
embeddedHtml :: [(FilePath, BS.ByteString)]
embeddedHtml = $(makeRelativeToProject "html" >>= embedDir)

-- | Look up a file that must be embedded, e.g. one of the templates.
embeddedHtmlFile :: FilePath -> BS.ByteString
embeddedHtmlFile file =
  fromMaybe (error $ "embeddedHtmlFile: html/" ++ file ++ " is not embedded in the binary") $
    lookup file embeddedHtml
