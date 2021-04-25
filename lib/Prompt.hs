module Prompt ( nydXPConfig, nydXPConfig', calcPrompt, editPrompt, openInEditor, scrotPrompt)

where

import XMonad
import XMonad.Core
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Man
import XMonad.Prompt.Pass
import XMonad.Prompt.Shell
import XMonad.Prompt.Ssh
import XMonad.Prompt.XMonad
import Control.Arrow (first)
import Control.Monad

import Data.Char (isSpace, toUpper)
import XMonad.Util.Run (runProcessWithInput, safeSpawn)

import Text.Printf


import qualified XMonad.StackSet as W

import qualified Data.Map as M

import Defaults (myFont, altMask)


nydXPConfig :: XPConfig
nydXPConfig = def
      { font                = myFont
      , bgColor             = "#151515"
      , fgColor             = "#d0d0d0"
      , bgHLight            = "#aa759f"
      , fgHLight            = "#e0e0e0"
      , borderColor         = "#aa759f"
      , promptBorderWidth   = 0
      , promptKeymap        = nydXPKeymap
      , position            = Top
      -- , position            = CenteredAt { xpCenterY = 0.3, xpWidth = 0.3 }
      , height              = 23
      , historySize         = 256
      , historyFilter       = id
      , defaultText         = []
      , autoComplete        = Just 100000  -- set Just 100000 for .1 sec
      , showCompletionOnTab = False
      -- , searchPredicate     = isPrefixOf
      , searchPredicate     = fuzzyMatch
      , defaultPrompter     = id $ map toUpper  -- change prompt to UPPER
      -- , defaultPrompter     = unwords . map reverse . words  -- reverse the prompt
      -- , defaultPrompter     = drop 5 .id (++ "XXXX: ")  -- drop first 5 chars of prompt and add XXXX:
      , alwaysHighlight     = True
      , maxComplRows        = Nothing      -- set to 'Just 5' for 5 rows
      }

-- The same config above minus the autocomplete feature which is annoying
-- on certain Xprompts, like the search engine prompts.
nydXPConfig' :: XPConfig
nydXPConfig' = nydXPConfig { autoComplete = Nothing }

calcPrompt c ans =
    inputPrompt c (trim ans) ?+ \input ->
        liftIO(runProcessWithInput "qalc" [input] "") >>= calcPrompt c
    where trim  = f . f
            where f = reverse . dropWhile isSpace

editPrompt :: String -> X ()
editPrompt home = do
    str <- inputPrompt cfg "EDIT: ~/"
    case str of
        Just s  -> openInEditor s
        Nothing -> pure ()
  where
    cfg = nydXPConfig { defaultText = "" }

openInEditor :: String -> X ()
openInEditor path = safeSpawn "emacsclient" ["-c", "-a", "emacs", path]

scrotPrompt :: String -> Bool -> X ()
scrotPrompt home select = do
    str <- inputPrompt cfg "~/scrot/"
    case str of
        Just s  -> spawn $ printf "sleep 0.3 && scrot %s '%s' -e 'mv $f ~/scrot'" mode s
        Nothing -> pure ()
  where
    mode = if select then "--select" else "--focused"
    cfg = nydXPConfig { defaultText = "" }

nydXPKeymap :: M.Map (KeyMask,KeySym) (XP ())
nydXPKeymap = M.fromList 
     $ map (first $ (,) controlMask)      -- control + <key>
     [ (xK_z, killBefore)               -- kill line backwards
     , (xK_k, killAfter)                -- kill line forwards
     , (xK_a, startOfLine)              -- move to the beginning of the line
     , (xK_e, endOfLine)                -- move to the end of the line
     , (xK_m, deleteString Next)        -- delete a character foward
     , (xK_b, moveCursor Prev)          -- move cursor forward
     , (xK_f, moveCursor Next)          -- move cursor backward
     , (xK_BackSpace, killWord Prev)    -- kill the previous word
     , (xK_y, pasteString)              -- paste a string
     , (xK_g, quit)                     -- quit out of prompt
     , (xK_bracketleft, quit)
     ]
     ++ map (first $ (,) altMask)          -- meta key + <key>
     [ (xK_BackSpace, killWord Prev)    -- kill the prev word
     , (xK_f, moveWord Next)            -- move a word forward
     , (xK_b, moveWord Prev)            -- move a word backward
     , (xK_d, killWord Next)            -- kill the next word
     , (xK_n, moveHistory W.focusUp')   -- move up thru history
     , (xK_p, moveHistory W.focusDown') -- move down thru history
     ]
     ++ map (first $ (,) 0) -- <key>
     [ (xK_Return, setSuccess True >> setDone True)
     , (xK_KP_Enter, setSuccess True >> setDone True)
     , (xK_BackSpace, deleteString Prev)
     , (xK_Delete, deleteString Next)
     , (xK_Left, moveCursor Prev)
     , (xK_Right, moveCursor Next)
     , (xK_Home, startOfLine)
     , (xK_End, endOfLine)
     , (xK_Down, moveHistory W.focusUp')
     , (xK_Up, moveHistory W.focusDown')
     , (xK_Escape, quit)
     ]