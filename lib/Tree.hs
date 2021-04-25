module Tree (treeselectAction, tsDefaultConfig, myTreeNavigation)

where 

import XMonad
import qualified XMonad.Actions.TreeSelect as TS
import qualified Data.Map as M
import Data.Tree


import Defaults(myFont, altMask)

treeselectAction :: TS.TSConfig (X ()) -> X ()
treeselectAction a = TS.treeselectAction a
   [ Node (TS.TSNode "+ Accessories" "Accessory applications" (return ()))
       [ Node (TS.TSNode "Picom Toggle on/off" "Compositor for window managers" (spawn "killall picom; picom --experimental-backend")) []
       , Node (TS.TSNode "Virtualbox" "Oracle's virtualization program" (spawn "virtualbox")) []
       ]
   ]

tsDefaultConfig :: TS.TSConfig a
tsDefaultConfig = TS.TSConfig { TS.ts_hidechildren = True
                              , TS.ts_background   = 0xdd151515
                              , TS.ts_font         = myFont
                              , TS.ts_node         = (0xffe0e0e0, 0xff202020)
                              , TS.ts_nodealt      = (0xffe0e0e0, 0xff151515)
                              , TS.ts_highlight    = (0xfff5f5f5, 0xffaa759f)
                              , TS.ts_extra        = 0xffd0d0d0
                              , TS.ts_node_width   = 200
                              , TS.ts_node_height  = 20
                              , TS.ts_originX      = 100
                              , TS.ts_originY      = 100
                              , TS.ts_indent       = 80
                              , TS.ts_navigate     = myTreeNavigation
                              }

myTreeNavigation = M.fromList
    [ ((0, xK_Escape),   TS.cancel)
    , ((0, xK_Return),   TS.select)
    , ((0, xK_space),    TS.select)
    , ((0, xK_Up),       TS.movePrev)
    , ((0, xK_Down),     TS.moveNext)
    , ((0, xK_Left),     TS.moveParent)
    , ((0, xK_Right),    TS.moveChild)
    , ((0, xK_k),        TS.movePrev)
    , ((0, xK_j),        TS.moveNext)
    , ((0, xK_h),        TS.moveParent)
    , ((0, xK_l),        TS.moveChild)
    , ((0, xK_o),        TS.moveHistBack)
    , ((0, xK_i),        TS.moveHistForward)
    , ((0, xK_a),        TS.moveTo ["+ Accessories"])
    , ((0, xK_e),        TS.moveTo ["+ Games"])
    , ((0, xK_g),        TS.moveTo ["+ Graphics"])
    , ((0, xK_i),        TS.moveTo ["+ Internet"])
    , ((0, xK_m),        TS.moveTo ["+ Multimedia"])
    , ((0, xK_o),        TS.moveTo ["+ Office"])
    , ((0, xK_p),        TS.moveTo ["+ Programming"])
    , ((0, xK_s),        TS.moveTo ["+ System"])
    , ((0, xK_b),        TS.moveTo ["+ Bookmarks"])
    , ((0, xK_c),        TS.moveTo ["+ Config Files"])
    , ((0, xK_r),        TS.moveTo ["+ Screenshots"])
    , ((mod4Mask, xK_l), TS.moveTo ["+ Bookmarks", "+ Linux"])
    , ((mod4Mask, xK_e), TS.moveTo ["+ Bookmarks", "+ Emacs"])
    , ((mod4Mask, xK_s), TS.moveTo ["+ Bookmarks", "+ Search and Reference"])
    , ((mod4Mask, xK_p), TS.moveTo ["+ Bookmarks", "+ Programming"])
    , ((mod4Mask, xK_v), TS.moveTo ["+ Bookmarks", "+ Vim"])
    , ((mod4Mask .|. altMask, xK_a), TS.moveTo ["+ Bookmarks", "+ Linux", "+ Arch Linux"])
    , ((mod4Mask .|. altMask, xK_n), TS.moveTo ["+ Bookmarks", "+ Linux", "+ Linux News"])
    , ((mod4Mask .|. altMask, xK_w), TS.moveTo ["+ Bookmarks", "+ Linux", "+ Window Managers"])
    ]
