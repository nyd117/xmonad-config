module Defaults (myFont, myModMask, myTerminal, myBrowser, myEditor, myBorderWidth, myNormColor, myFocusColor, altMask, searchList)

where 

import XMonad
import qualified XMonad.Actions.Search as S


myFont :: String
myFont = "xft:MesloLGS NF:weight=medium:pixelsize=14:antialias=true:hinting=true"

myModMask :: KeyMask
myModMask = mod4Mask       -- Sets modkey to super/windows key

myTerminal :: String
myTerminal = "alacritty"   -- Sets default terminal

myBrowser :: String
myBrowser = "vivaldi-stable "               -- Sets qutebrowser as browser for tree select
-- myBrowser = myTerminal ++ " -e lynx " -- Sets lynx as browser for tree select

myEditor :: String
myEditor = "emacsclient -c -a emacs "  -- Sets emacs as editor for tree select
-- myEditor = myTerminal ++ " -e vim "    -- Sets vim as editor for tree select

myBorderWidth :: Dimension
myBorderWidth = 0          -- Sets border width for windows

myNormColor :: String
myNormColor   = "#d0d0d0"  -- Border color of normal windows

myFocusColor :: String
myFocusColor  = "#aa759f"  -- Border color of focused windows

altMask :: KeyMask
altMask = mod1Mask         -- Setting this for use in xprompts

searchList :: [(String, S.SearchEngine)]
searchList = [ ("a", archwiki)
             , ("d", S.duckduckgo)
             , ("e", ebay)
             , ("g", S.google)
             , ("h", S.hoogle)
             , ("i", S.images)
             , ("n", news)
             , ("r", reddit)
             , ("s", S.stackage)
             , ("t", S.thesaurus)
             , ("v", S.vocabulary)
             , ("b", S.wayback)
             , ("u", urban)
             , ("w", S.wikipedia)
             , ("y", S.youtube)
             , ("S-y", yacy)
             , ("z", S.amazon)
             ]

archwiki, ebay, news, reddit, urban, yacy :: S.SearchEngine

archwiki = S.searchEngine "archwiki" "https://wiki.archlinux.org/index.php?search="
ebay     = S.searchEngine "ebay" "https://www.ebay.com/sch/i.html?_nkw="
news     = S.searchEngine "news" "https://news.google.com/search?q="
reddit   = S.searchEngine "reddit" "https://www.reddit.com/search/?q="
urban    = S.searchEngine "urban" "https://www.urbandictionary.com/define.php?term="
yacy     = S.searchEngine "yacy" "http://localhost:8090/yacysearch.html?query="
