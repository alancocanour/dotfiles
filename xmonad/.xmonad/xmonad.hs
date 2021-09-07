import Data.Ratio((%))
import XMonad
import XMonad.Actions.FindEmptyWorkspace(sendToEmptyWorkspace, tagToEmptyWorkspace)
import XMonad.Actions.WindowGo(runOrRaise)
import XMonad.Hooks.DynamicLog(ppOutput, ppTitle, dynamicLogWithPP, xmobarPP, xmobarColor)
import XMonad.Hooks.EwmhDesktops(ewmh)
import XMonad.Hooks.ManageDocks(docks, avoidStruts)
import XMonad.Layout.NoBorders(noBorders)
import XMonad.Layout.Reflect(reflectHoriz)
import XMonad.Util.Run(spawnPipe, safeSpawnProg, hPutStrLn)
import qualified Data.Map.Lazy as M

main = do
  trayer <- spawn "killall --exact --quiet trayer ; trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 6 --transparent true --alpha 0 --tint 0x000000 --height 16"
  xmobar <- spawnPipe "killall --exact --quiet xmobar ; xmobar"
  spawn "killall --exact --quiet redshift ; redshift"
  xmonad . docks. ewmh $ def
          {   modMask = myModMask
            , terminal = "urxvt -e tmux new-session"
            , borderWidth = 1
            , normalBorderColor = "#000000"
            , manageHook = myManageHook
            , layoutHook = myLayoutHook
            , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmobar
                        , ppTitle = xmobarColor "green" ""
                        }
            , keys = myKeys <+> keys def
            }

myModMask = mod4Mask

myKeys :: XConfig Layout -> M.Map (ButtonMask, KeySym) (X ())
myKeys _ = M.fromList
           [ ((myModMask .|. shiftMask .|. controlMask, xK_Return), safeSpawnProg "urxvt")
           , ((myModMask .|. shiftMask, xK_n), spawn "pkill -USR1 redshift")
           , ((myModMask .|. shiftMask, xK_e), runOrRaise "emacs" (className =? "Emacs"))
           , ((myModMask .|. shiftMask, xK_b), runOrRaise "firefox" (className =? "Firefox"))
           , ((myModMask .|. shiftMask, xK_z), sendToEmptyWorkspace)
           , ((myModMask .|. shiftMask, xK_x), tagToEmptyWorkspace)
           ]

myManageHook :: ManageHook
myManageHook = composeAll [
  className =? "Xfce4-notifyd" --> doIgnore, --Keep notifications from stealing focus
  manageHook def
  ]


myLayoutHook = (avoidStruts $ reflectHoriz tall)
               ||| (avoidStruts $ Mirror tall)
               ||| noBorders Full
  where tall = Tall 1 (3 % 100) (1 % 2)
