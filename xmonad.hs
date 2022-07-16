  -- Base
import XMonad
import System.Directory
import System.IO (hPutStrLn)
import qualified XMonad.StackSet as W

    -- Data
import Data.Monoid

    -- Hooks
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.ManageDocks (docks, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.WorkspaceHistory
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.StatusBar

import XMonad.Layout.ShowWName

import Control.Monad (join)

   -- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run (spawnPipe)

import KeyBindings
import WorkSpaces
import Defaults
import ScratchPads
import Layouts
import Hooks


main :: IO ()
main = do
    home <- getHomeDirectory

    xmonad $ withUrgencyHook FocusHook
           $ withSB (xmobarTop <> xmobarBottom)
           $ ewmhFullscreen
           $ ewmh
           $ docks
           $ def
          { manageHook = ( isFullscreen --> doFullFloat ) <+> myManageHook <+> manageDocks
          -- Run xmonad commands from command line with "xmonadctl command". Commands include:
          -- shrink, expand, next-layout, default-layout, restart-wm, xterm, kill, refresh, run,
          -- focus-up, focus-down, swap-up, swap-down, swap-master, sink, quit-wm. You can run
          -- "xmonadctl 0" to generate full list of commands written to ~/.xsession-errors.
          -- To compile xmonadctl: ghc -dynamic xmonadctl.hs
          , handleEventHook    = serverModeEventHookCmd
                                 <+> serverModeEventHook
                                 <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
          , modMask            = myModMask
          , terminal           = myTerminal
          , startupHook        = myStartupHook <+> setFullscreenSupported -- the last overrides any previous behaviour that is already defined so setFullscreenSupported goes last
          , layoutHook         = showWName' myShowWNameTheme $ myLayoutHook
          , workspaces         = myWorkspaces
          , borderWidth        = myBorderWidth
          , normalBorderColor  = myNormColor
          , focusedBorderColor = myFocusColor
          , logHook = workspaceHistoryHook 
                      <+> myLogHook
          } `additionalKeysP` myKeys home

-- Debuging example write a function and bind it to a key

-- import System.IO
-- debugStuff :: X ()
-- debugStuff = withWindowSet (\ws -> do
--     liftIO $ print ws
--     liftIO $ logToTmpFile $ show ws 
--   )

-- myAppendFile :: FilePath -> String -> IO ()
-- myAppendFile f s = do
--   withFile f AppendMode $ \h -> do
--     hPutStrLn h s

-- logToTmpFile :: String -> IO ()
-- logToTmpFile = myAppendFile "/tmp/xmonad.log" . (++ "\n")
