import Data.Function ( (&) )
import Data.List ( elemIndex )
import qualified Data.Map as Map
import Data.Maybe ( fromMaybe )
import Data.Semigroup ( Endo )
import System.IO ( IO )
import XMonad
    ( (-->), (.|.), (<+>), (=?), (|||), ButtonMask, Choose, Default(def)
    , KeyMask, KeySym, Layout, Mirror, Mirror(Mirror), Query, Tall, Tall(Tall)
    , WindowSet, X
    , XConfig(XConfig, terminal, workspaces, normalBorderColor,
        focusedBorderColor, borderWidth, keys, modMask, manageHook), appName
    , className, composeAll, controlMask, doFloat, doShift, layoutHook, mod4Mask
    , shiftMask, spawn, windows, xK_b, xK_bracketleft, xK_bracketright, xK_p
    , xK_s, xK_z, xmonad )
import qualified XMonad.Actions.CycleWS as CycleWS
import XMonad.Actions.SpawnOn ( manageSpawn, spawnHere )
import XMonad.Hooks.DynamicLog
    ( PP, ppCurrent, ppHidden, ppOrder, ppSep, ppTitle, statusBar, xmobar
    , xmobarColor, xmobarPP )
import XMonad.Hooks.ManageDocks ( Direction1D(Next, Prev), manageDocks )
import XMonad.Hooks.ManageHelpers
    ( Side(SW, SE, C), doFullFloat, doSideFloat, isFullscreen )
import XMonad.Layout.LayoutModifier ( ModifiedLayout )
import XMonad.Layout.MultiColumns ( MultiCol, multiCol )
import XMonad.Layout.Named ( named )
import XMonad.Layout.NoBorders ( SmartBorder, noBorders, smartBorders )
import XMonad.Layout.Tabbed as Tabbed ( simpleTabbed )
import XMonad.Layout.ThreeColumns ( ThreeCol(ThreeColMid) )
import XMonad.Layout.ToggleLayouts ( toggleLayouts )
import qualified XMonad.StackSet as StackSet
import XMonad.Util.EZConfig ( additionalKeys )
import XMonad.Util.NamedScratchpad ( namedScratchpadFilterOutWorkspace )
import XMonad.Util.Run ( spawnPipe )
import XMonad.Util.WorkspaceCompare ( getSortByIndex )


main :: IO ()
main =
    xmonad =<< statusBar "xmobar" myPP toggleStrutsKey myConfig


myPP :: XMonad.Hooks.DynamicLog.PP
myPP =
    xmobarPP { ppCurrent = xmobarColor "#8888ff" ""
             , ppHidden  = xmobarColor "#484848" ""
             , ppTitle   = xmobarColor "#808080" ""
             , ppOrder   = \(ws:l:t:_) -> [ ws, l, t ]
             , ppSep     = "   "
             }


toggleStrutsKey :: XConfig l -> (KeyMask, KeySym)
toggleStrutsKey XConfig {XMonad.modMask = modMask} =
    (modMask, xK_b)


myConfig =
    def { terminal = "kitty --single-instance"
        , modMask = mod4Mask
        , workspaces = myWorkspaces
        , normalBorderColor = "#262626"
        , focusedBorderColor = "#44447f"
        , borderWidth = 4
        , layoutHook = myLayoutHook
        , manageHook = myManageHook
        , keys = \c -> myKeys c `Map.union` keys def c
        }

myWorkspaces :: [ String ]
myWorkspaces =
    [ "一", "二", "三", "四", "五", "六", "七", "八", "九", "十" ]


myKeys :: (XConfig Layout -> Map.Map (ButtonMask, KeySym) (X ()))
myKeys conf@XConfig {XMonad.modMask = modMask} = Map.fromList
    [ ((mod4Mask, xK_p), spawn "rofi -show run")

    -- Screenshots
    , ( (mod4Mask .|. shiftMask, xK_s)
        , spawnHere $
            "dir=$HOME/screens;" ++
            "mkdir -p $dir;" ++
            "fn=$dir/$(date +%m-%d-%Y-%H-%M-%S.png);" ++
            "sel=$(slop -f \"-g %g\");" ++
            "shotgun $sel $fn;" ++
            "gthumb $dir" )

    -- Cycle through non-empty workspaces
    , ( (modMask, xK_bracketright), windows . StackSet.greedyView =<< CycleWS.findWorkspace getSortByIndexNoNSP Next CycleWS.HiddenNonEmptyWS 1 )
    , ( (modMask, xK_bracketleft), windows . StackSet.greedyView =<< CycleWS.findWorkspace getSortByIndexNoNSP Prev CycleWS.HiddenNonEmptyWS 1 )

    -- Cycle through all workspaces
    , ( (modMask .|. controlMask, xK_bracketright), CycleWS.nextWS )
    , ( (modMask .|. controlMask, xK_bracketleft), CycleWS.prevWS )
    , ( (modMask, xK_z), CycleWS.toggleWS )

    -- Move window to next/previous workspace
    , ( (modMask .|. shiftMask, xK_bracketright), CycleWS.shiftToNext >> CycleWS.nextWS )
    , ( (modMask .|. shiftMask, xK_bracketleft), CycleWS.shiftToPrev >> CycleWS.prevWS )
    ]

    where
        getSortByIndexNoNSP = fmap (.namedScratchpadFilterOutWorkspace) getSortByIndex


myManageHook :: Query (Endo WindowSet)
myManageHook =
    composeAll
        [ isFullscreen --> doFullFloat
        , manageHook def
        , manageSpawn
        , manageDocks
        , appName =? "Scratch" --> doSideFloat SW
        , appName =? "Msgcompose" --> doSideFloat SW
        , className =? "Float" --> doSideFloat SW
        , className =? "Slack" --> (doFloat <+> doShift (getWS 2))
        , className =? "Gthumb" --> doSideFloat SE
        , className =? "Galculator" --> doSideFloat C
        , className =? "Gcolor2" --> doSideFloat C
        ]



myLayoutHook :: ModifiedLayout SmartBorder (Choose Tall MultiCol) a
myLayoutHook =
    smartBorders $
        Tall 1 incr (7/10) ||| multiCol [1] 1 incr (-1/2)
    where
        incr = 1/10


{-| Get a workspace by its number. Note that we're using 1-based indexing
     because that's generally how we're naming our workspaces. That is to say,
     the workspace at the 0 index of our list is called "1".

     If the index of the workspace doesn't exist we return the empty string.
-}
getWS :: Int -> String
getWS n
    | n < 1                        = ""
    | n - 1 >= length myWorkspaces = ""
    | otherwise                    = myWorkspaces !! (n - 1)
