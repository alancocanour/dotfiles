
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run

main = do
  xmobar <- spawnPipe "/usr/bin/xmobar /home/alan/.xmobarrc"
  trayer <- spawnPipe "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 6 --transparent true --alpha 0 --tint 0x000000 --height 16"
  xmonad defaultConfig
          {   modMask = mod4Mask
            , terminal = "terminator"
            , borderWidth = 1
            , normalBorderColor = "#000000"
            , manageHook = manageDocks <+> manageHook defaultConfig
            , layoutHook = avoidStruts $ layoutHook defaultConfig
            , logHook = dynamicLogWithPP defaultPP {
                  ppOutput = hPutStrLn xmobar
                             --, ppTitle = shorten 50
                  , ppLayout = const ""
                }
            }