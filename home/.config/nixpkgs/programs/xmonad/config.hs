import qualified Control.Exception             as E
import qualified DBus                          as D
import qualified DBus.Client                   as D
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           System.IO                      ( hClose
                                                , hPutStr
                                                )
import           XMonad
import           XMonad.Hooks.EwmhDesktops      ( ewmh )
import           XMonad.Hooks.ManageDocks       ( Direction2D(..)
                                                , avoidStruts
                                                , docks
                                                )
import           XMonad.Layout.Gaps             ( gaps )
import           XMonad.Layout.MultiToggle      ( Toggle(..)
                                                , mkToggle
                                                , single
                                                )
import           XMonad.Layout.MultiToggle.Instances
                                                ( StdTransformers(NBFULL) )
import           XMonad.Layout.NoBorders        ( smartBorders )
import           XMonad.Layout.Spacing          ( spacing )
import           XMonad.Layout.ThreeColumns     ( ThreeCol(..) )
import           XMonad.Util.CustomKeys
import           XMonad.Util.NamedActions       ( NamedAction(..)
                                                , (^++^)
                                                , addDescrKeys'
                                                , addName
                                                , showKm
                                                , subtitle
                                                )
import           XMonad.Util.Run                ( safeSpawn
                                                , spawnPipe
                                                )

main :: IO ()
main = xmonad . docks . ewmh $ def { terminal           = myTerminal
                                   , focusFollowsMouse  = True
                                   , clickJustFocuses   = False
                                   , borderWidth        = 3
                                   , modMask            = myModMask
                                   , layoutHook         = myLayout
                                   , keys = customKeys delkeys inskeys
                                   , normalBorderColor  = "#dddddd"
                                   , focusedBorderColor = "#1681f2"
                                   , startupHook        = myStartupHook
                                   }
  where myModMask = mod4Mask

myStartupHook = startupHook def

myTerminal = "alacritty"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--

delkeys conf@XConfig { XMonad.modMask = modm } = []

inskeys conf@XConfig { XMonad.modMask = modm } =
  [ ((modm .|. shiftMask, xK_Return), spawn (XMonad.terminal conf))
  , ( (modm, xK_space)
    , spawn "rofi -modi drun,ssh,window -show drun -show-icons"
    )
  , ((modm .|. shiftMask, xK_space), sendMessage NextLayout)
  , ((modm, xK_f)                  , sendMessage (Toggle NBFULL))
  , ((modm, xK_equal)              , spawn "polybar-msg cmd toggle &")
  ]


------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout =
  avoidStruts
    . smartBorders
    . fullScreenToggle
    $ (column3 ||| tiled ||| Mirror tiled ||| full)
 where
     -- default tiling algorithm partitions the screen into two panes
  tiled   = gapSpaced 10 $ Tall nmaster delta ratio
  full    = gapSpaced 5 Full
  column3 = gapSpaced 5 $ ThreeColMid 1 (3 / 100) (1 / 3)

  -- The default number of windows in the master pane
  nmaster = 1

  -- Default proportion of screen occupied by master pane
  ratio   = 1 / 2

  -- Percent of screen to increment by when resizing panes
  delta   = 3 / 100

  -- Gaps bewteen windows
  myGaps gap = gaps [(U, gap), (D, gap), (L, gap), (R, gap)]
  gapSpaced g = spacing g . myGaps g

  -- Fullscreen
  fullScreenToggle = mkToggle (single NBFULL)

------------------------------------------------------------------------
-- Polybar settings (needs DBus client).

mkDbusClient :: IO D.Client
mkDbusClient = do
  dbus <- D.connectSession
  D.requestName dbus (D.busName_ "org.xmonad.log") opts
  return dbus
 where
  opts = [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
