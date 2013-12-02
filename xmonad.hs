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
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.Prompt.Ssh
import XMonad.Prompt.AppendFile
import XMonad.Prompt.XMonad
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import System.IO

myBitmapsDir = "/home/bnicholas/.xmonad/dzen2"

myLayout = avoidStruts (tall ||| Mirror tall ||| Full)
    where
        tall = Tall 1 (3/100) (1/2)

myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
    { ppCurrent           =   dzenColor "#073642" "#cb4b16" . pad
    , ppVisible           =   dzenColor "#073642" "#b58900" . pad
    , ppHidden            =   dzenColor "#93a1a1" "#002b36" . pad
    , ppHiddenNoWindows   =   dzenColor "#586e75" "#002b36" . pad
    , ppUrgent            =   dzenColor "#073642" "#d33682" . pad
    , ppWsSep             =   ""
    , ppSep               =   "|"
    , ppLayout            =   dzenColor "#cb4b16" "#002b36" .
                              (\x -> case x of
                                  "Tall"             ->      "^i(" ++ myBitmapsDir ++ "/tall.xbm)"
                                  "Mirror Tall"      ->      "^i(" ++ myBitmapsDir ++ "/mtall.xbm)"
                                  "Full"             ->      "^i(" ++ myBitmapsDir ++ "/full.xbm)"
                                  _                  ->      x
                              )
    , ppTitle             =   (" " ++) . dzenColor "#93a1a1" "#002b36" . dzenEscape
    , ppOutput            =   hPutStrLn h
    }

myXPConfig :: XPConfig
myXPConfig =
    defaultXPConfig { font                  = "xft:Meslo LG S DZ for Powerline:Regular:pixelsize=12:antialias=true:hinting=true"
                    , bgColor               = "#073642"
                    , fgColor               = "#93a1a1"
                    , bgHLight              = "#b48900"
                    , fgHLight              = "#073642"
                    , borderColor           = "#002b36"
                    , promptBorderWidth     = 0
                    , height                = 24
                    , historyFilter         = deleteConsecutive
                    , position              = Top
                    }

myXmonadBar = "dzen2 -x '0' -y '0' -h '24' -w '1000' -ta 'l' -fg '#93a1a1' -bg '#002b36' -fn 'Segoe UI:Mono Regular:pixelsize=14:antialias=true:hinting=true'"
myStatusBar = "conky -c /home/bnicholas/.xmonad/.conky_dzen | dzen2 -x '1000' -w '920' -h '24' -ta 'r' -bg '#002b36' -fg '#93a1a1' -y '0' -fn 'Segoe UI:Mono Regular:pixelsize=14:antialias=true:hinting=true'"
myLocker = "xautolock -time 10 -locker 'gnome-screensaver-command -l' -notify 10 -notifier \"echo 'Locking in 10 seconds' | dzen2 -x 0 -y 24 -w 1920 -bg '#d33682' -fg '#073642' -e 'onstart=uncollapse' -p 10 -h 24\""
myCompositor = "xcompmgr -c"
myWallpaper = "nitrogen --restore"
myMail = "davmail"

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

main = do
    dzenLeftBar <- spawnPipe myXmonadBar
    dzenRightBar <- spawnPipe myStatusBar
    locker <- spawn myLocker
    compositing <- spawn myCompositor
    wallpapers <- spawn myWallpaper
    mail <- spawn myMail
    xmonad $ withUrgencyHook NoUrgencyHook $defaultConfig
        { modMask = mod4Mask
        , manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = myLayout
        , logHook = myLogHook dzenLeftBar >> fadeInactiveLogHook 0xdddddddd
        , terminal = "urxvt"
        , borderWidth = 0
        , workspaces = myWorkspaces
        } `additionalKeys`
-- General Commands
        [ ((mod4Mask .|. shiftMask, xK_l), spawn "gnome-screensaver-command -l")
        , ((0, xK_Print), spawn "scrot")
        ] `additionalKeysP`
-- Audio controls
        [ ("<XF86AudioPlay>", spawn "mpc toggle")
        , ("<XF86AudioStop>", spawn "mpc stop")
        , ("<XF86AudioPrev>", spawn "mpc prev")
        , ("<XF86AudioNext>", spawn "mpc next")
        , ("<XF86AudioLowerVolume>", spawn "~/.xmonad/dvol -d 4")
        , ("<XF86AudioRaiseVolume>", spawn "~/.xmonad/dvol -i 4")
        , ("<XF86AudioMute>", spawn "~/.xmonad/dvol -d 100")
-- Navigation controls
        , ("M-[", prevWS)
        , ("M-]", nextWS)
        , ("M-g", goToSelected defaultGSConfig)
        , ("M-S-d", windowPromptGoto myXPConfig)
-- Run prompts
        , ("M-S-r", shellPrompt myXPConfig)
        , ("M-r", runOrRaisePrompt myXPConfig)
        , ("M-S-k", sshPrompt myXPConfig)
        , ("M-d", appendFilePrompt myXPConfig "~/todo.txt")
        , ("M-x", xmonadPrompt myXPConfig)
        ]

