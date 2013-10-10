import XMonad
import qualified XMonad.StackSet as W
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.UrgencyHook
import XMonad.Actions.GridSelect
import XMonad.Actions.CycleWS
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Window
import XMonad.Prompt.Ssh
import XMonad.Layout.Circle
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import System.IO

myBitmapsDir = "/home/bnicholas/.xmonad/dzen2"
myLayout = avoidStruts (tall ||| Mirror tall ||| Full ||| Circle)
    where
        tall = Tall 1 (3/100) (1/2)
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
    { ppCurrent           =   dzenColor "#000000" "#ffaf00" . pad
    , ppVisible           =   dzenColor "#00005f" "#d7ff5f" . pad
    , ppHidden            =   dzenColor "#cccccc" "#1B1D1E" . pad
    , ppHiddenNoWindows   =   dzenColor "#7b7b7b" "#1B1D1E" . pad
    , ppUrgent            =   dzenColor "white" "#6f1D1E" . pad
    , ppWsSep             =   ""
    , ppSep               =   "  |  "
    , ppLayout            =   dzenColor "#ffaf00" "#1B1D1E" .
                              (\x -> case x of
                                  "Tall"             ->      "^i(" ++ myBitmapsDir ++ "/tall.xbm)"
                                  "Mirror Tall"      ->      "^i(" ++ myBitmapsDir ++ "/mtall.xbm)"
                                  "Full"             ->      "^i(" ++ myBitmapsDir ++ "/full.xbm)"
                                  "Circle"           ->      "O"
                                  _                  ->      x
                              )
    , ppTitle             =   (" " ++) . dzenColor "#cccccc" "#1B1D1E" . dzenEscape
    , ppOutput            =   hPutStrLn h
    }

myXmonadBar = "dzen2 -x '0' -y '0' -h '24' -w '1000' -ta 'l' -fg '#FFFFFF' -bg '#1B1D1E'"
myStatusBar = "conky -c /home/bnicholas/.xmonad/.conky_dzen | dzen2 -x '1000' -w '920' -h '24' -ta 'r' -bg '#1B1D1E' -fg '#FFFFFF' -y '0'"

myWorkspaces :: [String]
myWorkspaces = clickable . (map dzenEscape) $ ["1:web", "2:vim", "3:mail", "4:chat", "5:music", "6:db", "7:vm", "8:/", "9:/"]
    where 
        clickable l = [ "^ca(1,xdotool key super+" ++ show (n) ++ ")" ++ imagify(ws) ++ "^ca()" | (i,ws) <- zip [1..] l, let n = i ]
        imagify = (\ws -> case ws of
                    "1:web"   -> "^i(/home/bnicholas/.xmonad/dzen2/005b_03.xbm)"
                    "2:vim"   -> "^i(/home/bnicholas/.xmonad/dzen2/005b_19.xbm)"
                    "3:mail"  -> "^i(/home/bnicholas/.xmonad/dzen2/005b_02.xbm)"
                    "4:chat"  -> "^i(/home/bnicholas/.xmonad/dzen2/005b_31.xbm)"
                    "5:music" -> "^i(/home/bnicholas/.xmonad/dzen2/005b_66.xbm)"
                    "6:db"    -> "^i(/home/bnicholas/.xmonad/dzen2/005b_46.xbm)"
                    "7:vm"    -> "^i(/home/bnicholas/.xmonad/dzen2/005b_25.xbm)"
                    "8:/"     -> "^i(/home/bnicholas/.xmonad/dzen2/005b_24.xbm)"
                    "9:/"     -> "^i(/home/bnicholas/.xmonad/dzen2/005b_24.xbm)"
                    _         -> ws
                  )
-- ["1 ^i(/home/bnicholas/.xmonad/dzen2/005b_03.xpm)", "2 ", "3 ", "4 ", "5 ", "6 ", "7 ", "8 ", "9 "]

main = do
    dzenLeftBar <- spawnPipe myXmonadBar
    dzenRightBar <- spawnPipe myStatusBar
    xmonad $ withUrgencyHook NoUrgencyHook $defaultConfig
        { modMask = mod4Mask
        , manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = myLayout
        , logHook = myLogHook dzenLeftBar >> fadeInactiveLogHook 0xdddddddd
        , terminal = "urxvt"
        , focusedBorderColor = "#444444"
        , normalBorderColor = "#1B1D1E"
        , workspaces = myWorkspaces
        } `additionalKeys`
-- General Commands
        [ ((mod4Mask .|. shiftMask, xK_l), spawn "gnome-screensaver-command -l")
        , ((0, xK_Print), spawn "scrot")
-- Navigation controls
        , ((mod4Mask, xK_g), goToSelected defaultGSConfig)
        , ((mod4Mask .|. shiftMask, xK_w), gridselectWorkspace defaultGSConfig W.greedyView)
        , ((mod4Mask, xK_d), windowPromptGoto defaultXPConfig)
-- Run prompts
        , ((mod4Mask, xK_r), runOrRaisePrompt defaultXPConfig)
        , ((mod4Mask .|. shiftMask, xK_k), sshPrompt defaultXPConfig)
        ] `additionalKeysP`
        [ ("<XF86AudioPlay>", spawn "mpc toggle")
        , ("<XF86AudioStop>", spawn "mpc stop")
        , ("<XF86AudioPrev>", spawn "mpc prev")
        , ("<XF86AudioNext>", spawn "mpc next")
        , ("M-[", prevWS)
        , ("M-]", nextWS)
        ]

