import Control.Monad(join)
import Data.List(nub)
import Data.Maybe(maybeToList)
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
  xmobar <- spawnPipe "killall --exact --quiet xmobar ; xmobar"
  spawn "killall --exact --quiet redshift ; redshift"
  xmonad . docks. ewmh $ def
          {   modMask = myModMask
            , terminal = "urxvt -e tmux new-session"
            , borderWidth = 1
            , normalBorderColor = "#000000"
            , layoutHook = myLayoutHook
            , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmobar
                        , ppTitle = xmobarColor "green" ""
                        }
            , keys = myKeys <+> keys def
            , startupHook = setFullscreenSupported
            }

myModMask = mod4Mask

myKeys :: XConfig Layout -> M.Map (ButtonMask, KeySym) (X ())
myKeys _ = M.fromList
           [ ((myModMask .|. shiftMask .|. controlMask, xK_Return), safeSpawnProg "urxvt")
           , ((myModMask .|. shiftMask, xK_n), spawn "pkill -USR1 redshift") -- Toggle redshift
           , ((myModMask .|. shiftMask, xK_e), runOrRaise "emacs" (className =? "Emacs"))
           , ((myModMask .|. shiftMask, xK_b), runOrRaise "firefox" (className =? "Firefox"))
           , ((myModMask .|. shiftMask, xK_d), spawn "pkill -USR1 xmobar") -- Move xmobar to next screen
           , ((myModMask .|. shiftMask, xK_z), sendToEmptyWorkspace)
           , ((myModMask .|. shiftMask, xK_x), tagToEmptyWorkspace)
           ]

myLayoutHook = (avoidStruts $ reflectHoriz tall)
               ||| (avoidStruts $ Mirror tall)
               ||| noBorders Full
  where tall = Tall 1 (3 % 100) (1 % 2)

setFullscreenSupported :: X ()
setFullscreenSupported = addSupported ["_NET_WM_STATE", "_NET_WM_STATE_FULLSCREEN"]

addSupported :: [String] -> X ()
addSupported props = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_SUPPORTED"
    newSupportedList <- mapM (fmap fromIntegral . getAtom) props
    io $ do
      supportedList <- fmap (join . maybeToList) $ getWindowProperty32 dpy a r
      changeProperty32 dpy r a aTOM propModeReplace (nub $ newSupportedList ++ supportedList)
