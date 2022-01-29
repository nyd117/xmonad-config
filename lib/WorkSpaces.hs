module WorkSpaces ( myWorkspaces
 , myExtraWorkspaces
 , myWorkspacesKeys
 , clickable
 , developmentWs
 , browsingWs
 , systemWs
 , documentWs
 , virtualizationWs
 , imWs
 , musicWs
 , videoWs
 , graphicsWs)

where

import XMonad.Core
import XMonad.Hooks.DynamicLog (xmobarAction)
import Data.Maybe (fromMaybe)

developmentWs = "dev"
browsingWs = "www"
systemWs = "sys"
documentWs = "doc"
virtualizationWs = "vbox"
imWs = "chat"
musicWs = "mus"
videoWs = "vid"
graphicsWs = "gfx"
miscellaneousWs = "misc"

myWorkspaces = [developmentWs, browsingWs, systemWs, documentWs, virtualizationWs, imWs, musicWs, videoWs, graphicsWs] ++ (map snd myExtraWorkspaces)
myExtraWorkspaces = [("0", miscellaneousWs)] -- list of (key, name) add keys not forming already bound shortcuts. ex ("o", "workspaceName") since M-o or M-S-o (defined in myKeys for workspace control) is not bound

myWorkspacesKeys = map show [1..9] ++ map fst myExtraWorkspaces --[1..9] ++ (map read(map fst myExtraWorkspaces)) if clickable had signature of [(WorkspaceId, Int)]

clickable :: [(WorkspaceId, [Char])] -> WorkspaceId -> String
clickable ws w = fromMaybe w $ (\x -> xmobarAction ("xdotool key Super_L+" ++ show (x)) "1" w) <$> lookup w ws