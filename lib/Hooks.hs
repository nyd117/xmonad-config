module Hooks (myStartupHook, myManageHook, myLogHook, myLogHookWS, myLogHookBottom, setFullscreenSupported)
where

import XMonad
import XMonad.Hooks.SetWMName
import XMonad.Util.SpawnOnce
import qualified XMonad.StackSet as W
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..),xmobarAction)
import XMonad.Hooks.ManageHelpers (isDialog, doCenterFloat, isInProperty)
import XMonad.Hooks.InsertPosition

import Data.Maybe (maybeToList)
import Data.List
import Data.Monoid

import XMonad.Util.NamedScratchpad
import System.IO (hPutStrLn)


import XMonad.Hooks.FadeInactive

import Control.Monad


import WorkSpaces
import ScratchPads


myStartupHook :: X ()
myStartupHook = do { spawnOnce "lxpolkit &"
  ; spawnOnce "feh --bg-fill ~/Pictures/Wallpapers/arch_nz_cl4.png &"
  ; spawnOnce "picom --experimental-backend &"
  ; spawnOnce "mpd ~/.config/mpd/mpd.conf &"
  ; spawnOnce "dunst &"
  ; spawnOnce "udiskie --tray &"
  ; spawnOnce "setxkbmap -layout us,gr && setxkbmap -option 'grp:alt_shift_toggle' &"
  ; spawnOnce "volumeicon &"
  ; spawnOnce "trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 0 --transparent true --alpha 0 --tint 0x151515  --height 24 &"
  --spawnOnce "/usr/bin/emacs --daemon &" -- emacs daemon for the emacsclient
  ; spawnOnce "notify-send 'Βρε καλώς το παλίκάρι'"
  ; setWMName "LG3D"
}

myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = (isDialog --> doF W.shiftMaster)      -- THIS IS THE REQUIRED LINE
               <+> insertPosition Above Newer -- same as xmonad default but explicit
               <+> composeAll
     -- using 'doShift ( myWorkspaces !! 7)' sends program to workspace 8!
     -- I'm doing it this way because otherwise I would have to write out the full
     -- name of my workspaces, and the names would very long if using clickable workspaces.
     [ className =? "Vivaldi-stable" --> viewShift ( myWorkspaces !! 1 )
     , className =? "Sublime_text" --> viewShift ( myWorkspaces !! 0 )
     , className =? "Gimp"    --> doFloat
     , title =? "Oracle VM VirtualBox Manager" --> doFloat
     , className =? "VirtualBox Manager" --> viewShift ( myWorkspaces !! 4 )
     , className =? "vlc" --> viewShift ( myWorkspaces !! 7 )
     , (className =? "firefox" <&&> resource =? "Dialog") --> doFloat  -- Float Firefox Dialog
     , isDialog --> doCenterFloat
     , isPopup --> doCenterFloat
     , isSkipTaskBar --> doCenterFloat -- intellij IDEs splash
     , className =? "discord" --> viewShift ( myWorkspaces !! 5 )
     ] <+> namedScratchpadManageHook myScratchPads
     where viewShift = doF . liftM2 (.) W.greedyView W.shift
     

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
    where fadeAmount = 1.0

myLogHookWS h = dynamicLogWithPP . namedScratchpadFilterOutWorkspacePP $ wsPP { ppOutput = hPutStrLn h }

myLogHookBottom h = dynamicLogWithPP . namedScratchpadFilterOutWorkspacePP $ wsPP { ppOutput = hPutStrLn h, ppOrder = \(ws:l:t:_)   -> [] }


wsPP :: PP
wsPP = xmobarPP
        { -- ppOutput = \x -> hPutStrLn xmproc x
          ppCurrent = xmobarColor "#90a959" "" . wrap "[" "]" -- Current workspace in xmobar
        , ppVisible = xmobarColor "#90a959" ""                -- Visible but not current workspace
        , ppHidden = xmobarColor "#6a9fb5" "" . wrap "*" "" . clickable (zip myWorkspaces myWorkspacesKeys)   -- Hidden workspaces in xmobar
        , ppHiddenNoWindows = xmobarColor "#aa759f" "" . clickable (zip myWorkspaces myWorkspacesKeys)       -- Hidden workspaces (no windows)
        , ppTitle = xmobarColor "#b3afc2" "" . shorten 120     -- Title of active window in xmobar
        , ppSep =  "<fc=#d0d0d0> | </fc>"          -- Separators in xmobar
        , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!" . clickable (zip myWorkspaces myWorkspacesKeys)  -- Urgent workspace
        , ppExtras  = [windowCount]                           -- # of windows current workspace
        , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
        }



windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

isSplash = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_SPLASH"

isSkipTaskBar = isInProperty "_NET_WM_STATE" "_NET_WM_STATE_SKIP_TASKBAR"
-- queries
role = stringProperty "WM_WINDOW_ROLE"
isPopup = role =? "pop-up"

-- This is added so that Xmonad can advertise that it supports FULLSCREEN WM STATE to allow firefox and other programs
-- that respect this behaviour to attempt to go fullscreen
setFullscreenSupported :: X ()
setFullscreenSupported = addSupported ["_NET_WM_STATE", "_NET_WM_STATE_FULLSCREEN"]


addSupported :: [String] -> X ()
addSupported props = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_SUPPORTED"
    newSupportedList <- mapM (fmap fromIntegral . getAtom) props
    io $ do { supportedList <- fmap (join . maybeToList) $ getWindowProperty32 dpy a r
            ; changeProperty32 dpy r a aTOM propModeReplace (nub $ newSupportedList ++ supportedList)
            }
