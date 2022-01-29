module KeyBindings (myKeys)

where

import XMonad
import XMonad.Core
import System.Exit (exitSuccess)

import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.StackSet as W
import qualified XMonad.Actions.Search as S
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

import XMonad.Actions.WithAll (sinkAll, killAll)

import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), nextScreen, prevScreen, Direction1D(..))
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WindowBringer

import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..), Direction2D(..))
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.ResizableTile
import XMonad.Layout.SubLayouts

import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (safeSpawn)

import Data.Maybe (isJust)


import XMonad.Layout.Spacing


import WorkSpaces
import Prompt
import Defaults (myTerminal, myBrowser, searchList)
import Grid
import Tree
import ScratchPads

import XMonad.Actions.GridSelect

import XMonad.Prompt.XMonad
import XMonad.Prompt.Man
import XMonad.Prompt.Pass
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh


myKeys :: String -> [([Char], X ())]
myKeys home =
    -- Xmonad
        [ ("M-C-r", spawn "notify-send Xmonad Recompiling && xmonad --recompile && notify-send Xmonad Recompiled") -- Recompiles xmonad. If using ~/.config/xmonad/ make sure ~/.xmonad doesn't exist.
        , ("M-S-r", spawn "xmonad --restart && notify-send Xmonad Restarted")   -- Restarts xmonad
        , ("M-S-q", io exitSuccess)             -- Quits xmonad

    -- Run Prompt
        , ("M-S-<Return>", shellPrompt nydXPConfig) -- Xmonad Shell Prompt
        , ("M-d", spawn "dmenu_run -nb '#151515' -sf '#d0d0d0' -sb '#aa759f' -nf '#e0e0e0' -i -p \"Run: \"") -- Dmenu
        -- , ("M-S-<Return>", spawn "rofi -show drun -config ~/.config/rofi/themes/dt-dmenu.rasi -display-drun \"Run: \" -drun-display-format \"{name}\"") -- Rofi

    -- Other Prompts
        , ("M-p c", calcPrompt nydXPConfig' "qalc") -- calcPrompt
        , ("M-p e", editPrompt home)               -- editPrompt
        , ("M-p m", manPrompt nydXPConfig)          -- manPrompt
        , ("M-p p", passPrompt nydXPConfig)         -- passPrompt
        , ("M-p g", passGeneratePrompt nydXPConfig) -- passGeneratePrompt
        , ("M-p r", passRemovePrompt nydXPConfig)   -- passRemovePrompt
        , ("M-p s", sshPrompt nydXPConfig)          -- sshPrompt
        , ("M-p x", xmonadPrompt nydXPConfig)       -- xmonadPrompt
        , ("M-p q", scrotPrompt home True)         -- scrotPrompt True
        , ("M-p z", scrotPrompt home False)        -- scrotPrompt False

    -- Useful programs to have a keybinding for launch
        , ("M-<Return>", spawn (myTerminal))
        , ("M-b", spawn (myBrowser ++ "www.google.gr"))
        , ("M-M1-h", spawn (myTerminal ++ " -e htop"))

    -- Kill windows
        , ("M-S-c", kill)     -- Kill the currently focused client
        , ("M-S-a", killAll)   -- Kill all windows on current workspace

    -- Workspaces
        , ("M-.", nextScreen)  -- Switch focus to next monitor
        , ("M-,", prevScreen)  -- Switch focus to prev monitor
        , ("M-S-<Right>", shiftTo Next nonNSP >> moveTo Next nonNSP)       -- Shifts focused window to next ws
        , ("M-S-<Left>", shiftTo Prev nonNSP >> moveTo Prev nonNSP)  -- Shifts focused window to prev ws
        , ("M-C-<Right>", moveTo Next NonEmptyWS)       -- Shifts focused window to next ws
        , ("M-C-<Left>", moveTo Prev NonEmptyWS)
    -- Floating windows
        , ("M-f", sendMessage (T.Toggle "floats")) -- Toggles my 'floats' layout
        , ("M-t", withFocused $ windows . W.sink)  -- Push floating window back to tile
        , ("M-S-t", sinkAll)                       -- Push ALL floating windows to tile

    -- Increase/decrease spacing (gaps)
        , ("M--", decWindowSpacing 4)           -- Decrease window spacing
        , ("M-=", incWindowSpacing 4)           -- Increase window spacing
        , ("M-S--", decScreenSpacing 4)         -- Decrease screen spacing
        , ("M-S-=", incScreenSpacing 4)         -- Increase screen spacing

    -- Grid Select (CTR-g followed by a key)
        , ("C-g g", spawnSelected' myAppGrid)                 -- grid select favorite apps
        , ("C-g t", goToSelected $ mygridConfig myColorizer)  -- goto selected window
        , ("C-g b", bringSelected $ mygridConfig myColorizer) -- bring selected window

    -- Tree Select
        , ("C-t t", treeselectAction tsDefaultConfig)

    -- Windows navigation
        , ("M-m", windows W.focusMaster)  -- Move focus to the master window
        , ("M-j", windows W.focusDown)    -- Move focus to the next window
        , ("M-k", windows W.focusUp)      -- Move focus to the prev window
        , ("M-S-m", windows W.swapMaster) -- Swap the focused window and the master window
        , ("M-S-j", windows W.swapDown)   -- Swap focused window with next window
        , ("M-S-k", windows W.swapUp)     -- Swap focused window with prev window
        , ("M-<Backspace>", promote)      -- Moves focused window to master, others maintain order
        , ("M-S-<Tab>", rotSlavesDown)    -- Rotate all windows except master and keep focus in place
        , ("M-C-<Tab>", rotAllDown)       -- Rotate all the windows in the current stack

        , ("M-S-w", gotoMenuArgs $ ["-nb", "#151515", "-sf" ,"#d0d0d0", "-sb", "#aa759f", "-nf", "#e0e0e0", "-i", "-p", "Go to window: " ])
        , ("M-S-b", bringMenuArgs $ ["-nb", "#151515", "-sf" ,"#d0d0d0", "-sb", "#aa759f", "-nf", "#e0e0e0", "-i", "-p", "Bring window: " ])

    -- Layouts
        , ("M-<Tab>", sendMessage NextLayout)           -- Switch to next layout
        , ("M-C-M1-<Up>", sendMessage Arrange)
        , ("M-C-M1-<Down>", sendMessage DeArrange)
        , ("M-<Space>", sendMessage (MT.Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full
        , ("M-S-<Space>", sendMessage ToggleStruts)     -- Toggles struts
        , ("M-S-n", sendMessage $ MT.Toggle NOBORDERS)  -- Toggles noborder

    -- Increase/decrease windows in the master pane or the stackSgWg
        , ("M-S-<Up>", sendMessage (IncMasterN 1))      -- Increase number of clients in master pane
        , ("M-S-<Down>", sendMessage (IncMasterN (-1))) -- Decrease number of clients in master pane
        , ("M-C-<Up>", increaseLimit)                   -- Increase number of windows
        , ("M-C-<Down>", decreaseLimit)                 -- Decrease number of windows

    -- Window resizing
        , ("M-h", sendMessage Shrink)                   -- Shrink horiz window width
        , ("M-l", sendMessage Expand)                   -- Expand horiz window width
        , ("M-M1-j", sendMessage MirrorShrink)          -- Shrink vert window width
        , ("M-M1-k", sendMessage MirrorExpand)          -- Exoand vert window width

        -- focus urgent window
        , ("M-u u", focusUrgent)

    -- Sublayouts
    -- This is used to push windows to tabbed sublayouts, or pull them out of it.
        , ("M-C-h", sendMessage $ pullGroup L)
        , ("M-C-l", sendMessage $ pullGroup R)
        , ("M-C-k", sendMessage $ pullGroup U)
        , ("M-C-j", sendMessage $ pullGroup D)
        , ("M-C-m", withFocused (sendMessage . MergeAll))
        , ("M-C-u", withFocused (sendMessage . UnMerge))
        , ("M-C-/", withFocused (sendMessage . UnMergeAll))
        , ("M-C-.", onGroup W.focusUp')    -- Switch focus to next tab
        , ("M-C-,", onGroup W.focusDown')  -- Switch focus to prev tab

    -- Scratchpads
        , ("M-C-<Return>", namedScratchpadAction myScratchPads "terminal")
        , ("M-C-c", namedScratchpadAction myScratchPads "nano")

    -- Controls for mpd music player (SUPER-u followed by a key)
        , ("M-u p", spawn "mpc play")
        , ("M-u l", spawn "mpc next")
        , ("M-u h", spawn "mpc prev")
        , ("M-u <Space>", spawn "mpc pause")

    -- Emacs (CTRL-e followed by a key)
        , ("C-e e", spawn "emacsclient -c -a 'emacs'")                            -- start emacs
        , ("C-e b", spawn "emacsclient -c -a 'emacs' --eval '(ibuffer)'")         -- list emacs buffers
        , ("C-e d", spawn "emacsclient -c -a 'emacs' --eval '(dired nil)'")       -- dired emacs file manager
        , ("C-e i", spawn "emacsclient -c -a 'emacs' --eval '(erc)'")             -- erc emacs irc client
        , ("C-e m", spawn "emacsclient -c -a 'emacs' --eval '(mu4e)'")            -- mu4e emacs email client
        , ("C-e n", spawn "emacsclient -c -a 'emacs' --eval '(elfeed)'")          -- elfeed emacs rss client
        , ("C-e s", spawn "emacsclient -c -a 'emacs' --eval '(eshell)'")          -- eshell within emacs
        , ("C-e t", spawn "emacsclient -c -a 'emacs' --eval '(mastodon)'")        -- mastodon within emacs
        , ("C-e v", spawn "emacsclient -c -a 'emacs' --eval '(+vterm/here nil)'") -- vterm within emacs
        -- emms is an emacs audio player. I set it to auto start playing in a specific directory.
        , ("C-e a", spawn "emacsclient -c -a 'emacs' --eval '(emms)' --eval '(emms-play-directory-tree \"~/Music/Non-Classical/70s-80s/\")'")

    -- Multimedia Keys
        , ("<XF86MonBrightnessUp>", spawn "~/.local/bin/brightness.sh up")
        , ("<XF86MonBrightnessDown>", spawn "~/.local/bin/brightness.sh down")
        , ("<XF86AudioPlay>", spawn (myTerminal ++ "mocp --play"))
        , ("<XF86AudioPrev>", spawn (myTerminal ++ "mocp --previous"))
        , ("<XF86AudioNext>", spawn (myTerminal ++ "mocp --next"))
        , ("<XF86AudioMute>",   spawn "~/.local/bin/volume.sh mute")
        , ("<XF86AudioLowerVolume>", spawn "~/.local/bin/volume.sh down")
        , ("<XF86AudioRaiseVolume>", spawn "~/.local/bin/volume.sh up")
        , ("<XF86HomePage>", spawn "firefox")
        , ("<XF86Search>", safeSpawn "firefox" ["https://www.duckduckgo.com/"])
        , ("<XF86Mail>", runOrRaise "thunderbird" (resource =? "thunderbird"))
        , ("<XF86Calculator>", runOrRaise "qalculate-gtk" (resource =? "qalculate-gtk"))
        , ("<XF86Eject>", spawn "toggleeject")
        , ("<Print>", spawn "scrot ~/Pictures/Screenshots/\"$(date +%Y_%m_%d_%H%M%S.png)\" && notify-send \"Full Screenshot taken at ~/Pictures/Screenshots/\"$(date +%Y_%m_%d_%H%M%S)\"\"")
        , ("M1-<Print>", spawn "scrot -u ~/Pictures/Screenshots/\"$(date +%Y_%m_%d_%H%M%S.png)\" && notify-send \"Window Screenshot taken at ~/Pictures/Screenshots/\"$(date +%Y_%m_%d_%H%M%S)\"\"")  -- M1 is Alt in xmodmap output
        ]
    -- Appending search engine prompts to keybindings list.
    -- Look at "search engines" section of this config for values for "k".
    -- The browser is supplied by getBrowser from XMonad.Prompt.Shell. which 
    -- asks the shell what browser the user likes. If the user hasn't defined any $BROWSER, defaults to returning "firefox",
        ++ [("M-s " ++ k, S.promptSearch nydXPConfig' f) | (k,f) <- searchList ]
        ++ [("M-S-s " ++ k, S.selectSearchBrowser "vivaldi-stable" f) | (k,f) <- searchList ]
        -- adding navigation functionality for extra workspaces
        ++ [("M-S-" ++ k, windows $ W.shift ws) | (k,ws) <- myExtraWorkspaces]
        ++ [("M-" ++ k, windows $ W.greedyView ws) | (k,ws) <- myExtraWorkspaces]
        -- adding functionality to shift window to any workspace and focus on it
        ++ [("M-C-" ++ k, windows $ W.greedyView ws . W.shift ws) | (k,ws) <- zip myWorkspacesKeys myWorkspaces ]
    -- The following lines are needed for named scratchpads.
          where nonNSP          = WSIs (return (\ws -> W.tag ws /= "nsp"))
                nonEmptyNonNSP  = WSIs (return (\ws -> isJust (W.stack ws) && W.tag ws /= "nsp"))