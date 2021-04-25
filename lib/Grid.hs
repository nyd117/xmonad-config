module Grid (mygridConfig, spawnSelected', myAppGrid, myColorizer)

where

import XMonad
import XMonad.Actions.GridSelect

import Defaults (myFont)


myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
                  (0x28,0x2c,0x34) -- lowest inactive bg
                  (0x28,0x2c,0x34) -- highest inactive bg
                  (0xc7,0x92,0xea) -- active bg
                  (0xc0,0xa7,0x9a) -- inactive fg
                  (0x28,0x2c,0x34) -- active fg

-- gridSelect menu layout
mygridConfig :: p -> GSConfig Window
mygridConfig colorizer = (buildDefaultGSConfig myColorizer)
    { gs_cellheight   = 40
    , gs_cellwidth    = 200
    , gs_cellpadding  = 6
    , gs_originFractX = 0.5
    , gs_originFractY = 0.5
    , gs_font         = myFont
    }

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
  where conf = def { gs_cellheight   = 40
                   , gs_cellwidth    = 200
                   , gs_cellpadding  = 6
                   , gs_originFractX = 0.5
                   , gs_originFractY = 0.5
                   , gs_font         = myFont
                   }

myAppGrid = [ ("Audacity", "audacity")
            , ("Deadbeef", "deadbeef")
            , ("Emacs", "emacsclient -c -a emacs")
            , ("Firefox", "firefox")
            , ("SublimeText", "subl")
            , ("Geary", "geary")
            , ("Gimp", "gimp")
            , ("Kdenlive", "kdenlive")
            , ("LibreOffice Impress", "loimpress")
            , ("LibreOffice Writer", "lowriter")
            , ("OBS", "obs")
            , ("Thunar", "thunar")
            ]