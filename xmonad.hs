
import qualified Data.Map.Lazy as M
import XMonad
import XMonad.Actions.WindowGo
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Util.Run

main = do
  xmobar <- spawnPipe "/usr/bin/xmobar /home/alan/.xmobarrc"
  trayer <- spawnPipe "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 6 --transparent true --alpha 0 --tint 0x000000 --height 16"
  redshift <- spawnPID "redshift"
  xmonad defaultConfig
          {   modMask = myModMask
            , terminal = "terminator"
            , borderWidth = 1
            , normalBorderColor = "#000000"
            , manageHook = myManageHook
            , layoutHook = avoidStruts . smartBorders $ layoutHook defaultConfig
            , logHook = dynamicLogWithPP defaultPP
                  { ppOutput = hPutStrLn xmobar
                  , ppLayout = const ""
                  }
            , keys = myKeys <+> keys defaultConfig
            }

myModMask = mod4Mask

myKeys :: XConfig Layout -> M.Map (ButtonMask, KeySym) (X ())
myKeys _ = M.fromList
           [ ((myModMask .|. shiftMask, xK_n), unsafeSpawn "pkill -USR1 redshift")
           , ((myModMask .|. shiftMask, xK_l), unsafeSpawn "physlock")
           , ((myModMask .|. shiftMask, xK_e), runOrRaise "emacs" (className =? "Emacs"))
           , ((myModMask .|. shiftMask, xK_b), runOrRaise "chromium" (className =? "Chromium"))
           ]

myManageHook :: ManageHook
myManageHook = composeAll [
  className =? "Xfce4-notifyd" --> doIgnore, --Keep notifications from stealing focus
  manageDocks,
  manageHook defaultConfig
  ]
