module ScratchPads (myScratchPads)
where

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad

import Defaults(myTerminal)

myScratchPads :: [NamedScratchpad]
myScratchPads = [ NS "terminal" spawnTerm findTerm manageTerm
                , NS "nano" spawnNano findNano manageNano
                ]
  where
    spawnTerm  = myTerminal ++ " --title 'htop' --class 'htop' -e htop"
    findTerm   = resource =? "htop"
    manageTerm = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w
    spawnNano  = myTerminal ++ " --title 'nano' --class 'nano' -e nano"
    findNano   = resource =? "nano"
    manageNano = customFloating $ W.RationalRect l t w h
               where
                 h = 0.9
                 w = 0.9
                 t = 0.95 -h
                 l = 0.95 -w