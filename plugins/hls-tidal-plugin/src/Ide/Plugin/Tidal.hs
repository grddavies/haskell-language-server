{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Ide.Plugin.Tidal (descriptor) where

import Development.IDE (IdeState)
import Development.IDE.LSP.HoverDefinition qualified as Hover
import Development.IDE.LSP.Notifications qualified as Notifications
import Development.IDE.LSP.Outline (moduleOutline)
import Development.IDE.Plugin.Completions qualified as Completions
import Development.IDE.Plugin.TypeLenses qualified as TypeLenses
import Ide.Logger (Recorder, WithPriority)
import Ide.Types (PluginDescriptor (pluginHandlers), PluginId, PluginMethodHandler, defaultPluginDescriptor, mkPluginHandler, pluginFileType)
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types

data Log
  = LogNotifications Notifications.Log
  | LogCompletions Completions.Log
  | LogTypeLenses TypeLenses.Log
  | LogHover Hover.Log
  deriving (Show)

descriptor :: Recorder (WithPriority Hover.Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId =
  (defaultPluginDescriptor plId desc)
    { pluginFileType = [".tidal"],
      pluginHandlers =
        mconcat
          [ mkPluginHandler SMethod_TextDocumentHover (handleHover recorder),
            mkPluginHandler SMethod_TextDocumentDocumentSymbol moduleOutline,
            mkPluginHandler
              SMethod_TextDocumentDefinition
              ( \ide _ DefinitionParams {..} ->
                  Hover.gotoDefinition recorder ide TextDocumentPositionParams {..}
              ),
            mkPluginHandler
              SMethod_TextDocumentTypeDefinition
              ( \ide _ TypeDefinitionParams {..} ->
                  Hover.gotoTypeDefinition recorder ide TextDocumentPositionParams {..}
              ),
            mkPluginHandler
              SMethod_TextDocumentDocumentHighlight
              ( \ide _ DocumentHighlightParams {..} ->
                  Hover.documentHighlight recorder ide TextDocumentPositionParams {..}
              ),
            mkPluginHandler SMethod_TextDocumentReferences (Hover.references recorder),
            mkPluginHandler SMethod_WorkspaceSymbol (Hover.wsSymbols recorder)
          ]
    }
  where
    desc = "Provides IDE features in .tidal files"

handleHover :: Recorder (WithPriority Hover.Log) -> PluginMethodHandler IdeState Method_TextDocumentHover
handleHover recorder ideState _ HoverParams {..} =
  Hover.hover recorder ideState TextDocumentPositionParams {..}
