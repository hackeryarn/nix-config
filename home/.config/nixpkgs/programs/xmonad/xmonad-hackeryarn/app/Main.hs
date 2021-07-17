import qualified Codec.Binary.UTF8.String      as UTF8
import qualified Control.Exception             as E
import qualified DBus                          as D
import qualified DBus.Client                   as D
import           Data.Foldable                  ( traverse_ )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           System.IO                      ( hClose
                                                , hPutStr
                                                )
import           XMonad
import           XMonad.Hooks.DynamicLog        ( PP
                                                  ( ppCurrent
                                                  , ppHidden
                                                  , ppHiddenNoWindows
                                                  , ppOrder
                                                  , ppOutput
                                                  , ppTitle
                                                  , ppUrgent
                                                  , ppVisible
                                                  )
                                                , dynamicLogWithPP
                                                , shorten
                                                , wrap
                                                )
import           XMonad.Hooks.EwmhDesktops      ( ewmh
                                                , ewmhDesktopsEventHook
                                                , fullscreenEventHook
                                                )
import           XMonad.Hooks.FadeInactive      ( fadeInactiveLogHook )
import           XMonad.Hooks.ManageDocks       ( Direction2D(..)
                                                , avoidStruts
                                                , docks
                                                , docksEventHook
                                                )
import           XMonad.Hooks.UrgencyHook       ( UrgencyHook(..)
                                                , withUrgencyHook
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
import           XMonad.Prompt                  ( XPConfig(..)
                                                , XPPosition(CenteredAt)
                                                , amberXPConfig
                                                )
import qualified XMonad.StackSet               as W
import           XMonad.Util.CustomKeys         ( customKeys )
import           XMonad.Util.NamedActions       ( NamedAction(..)
                                                , (^++^)
                                                , addDescrKeys'
                                                , addName
                                                , showKm
                                                , subtitle
                                                )
import           XMonad.Util.NamedScratchpad    ( NamedScratchpad(NS)
                                                , customFloating
                                                , namedScratchpadAction
                                                , namedScratchpadManageHook
                                                )
import qualified XMonad.Util.NamedWindows      as W
import           XMonad.Util.Run                ( safeSpawn
                                                , spawnPipe
                                                )


main :: IO ()
main = mkDbusClient >>= main'

main' :: D.Client -> IO ()
main' dbus = xmonad . docks . ewmh . myUrgencyHook $ def
  { terminal           = myTerminal
  , focusFollowsMouse  = False
  , clickJustFocuses   = False
  , borderWidth        = 3
  , modMask            = myModMask
  , layoutHook         = myLayout
  , keys               = customKeys delkeys inskeys
  , normalBorderColor  = "#FDF6E3"
  , focusedBorderColor = "#268bd2"
  , workspaces         = myWS
  , handleEventHook    = myEventHook
  , logHook            = myPolybarLogHook dbus
  , startupHook        = myStartupHook
  , manageHook         = myManageHook
  }
 where
  myModMask     = mod4Mask
  myUrgencyHook = withUrgencyHook LibNotifyUrgencyHook
  myManageHook  = namedScratchpadManageHook scratchpads


myStartupHook = startupHook def

data LibNotifyUrgencyHook = LibNotifyUrgencyHook
  deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
  urgencyHook LibNotifyUrgencyHook w = do
    name     <- W.getName w
    maybeIdx <- W.findTag w <$> gets windowset
    traverse_ (\i -> safeSpawn "notify-send" [show name, "workspace " ++ i])
              maybeIdx

myTerminal :: String
myTerminal = "alacritty"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.

delkeys conf@XConfig { XMonad.modMask = modm } = []

inskeys conf@XConfig { XMonad.modMask = modm } =
  [ ((modm .|. shiftMask, xK_Return), spawn (XMonad.terminal conf))
  , ( (modm, xK_space)
    , spawn "rofi -modi drun,ssh,window -show drun -show-icons"
    )
  , ((modm .|. shiftMask, xK_space), sendMessage NextLayout)
  , ((modm, xK_f)                  , sendMessage (Toggle NBFULL))
  , ((modm, xK_equal)              , spawn "polybar-msg cmd toggle &")
  , ((modm, xK_s)                  , spawn "systemctl suspend")
  , ((modm, xK_c)                  , spawn "flameshot gui -p ~/Pictures")
  , ((modm, xK_t), namedScratchpadAction scratchpads "alacritty")
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
-- Workspaces
sysWs = "sys"
workWs = "wrk"
comWs = "com"
ossWs = "oss"
devWs = "dev"
etcWs = "etc"

myWS :: [WorkspaceId]
myWS = [sysWs, workWs, comWs, ossWs, devWs, etcWs]


------------------------------------------------------------------------
-- Workspaces
scratchpads =
  [ NS "alacritty"
       "alacritty"
       (className =? "Alacritty")
       (customFloating $ W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3))
  ]

------------------------------------------------------------------------
-- Polybar settings (needs DBus client).

mkDbusClient :: IO D.Client
mkDbusClient = do
  dbus <- D.connectSession
  D.requestName dbus (D.busName_ "org.xmonad.log") opts
  return dbus
 where
  opts = [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str =
  let opath  = D.objectPath_ "/org/xmonad/Log"
      iname  = D.interfaceName_ "org.xmonad.Log"
      mname  = D.memberName_ "Update"
      signal = D.signal opath iname mname
      body   = [D.toVariant $ UTF8.decodeString str]
  in  D.emit dbus $ signal { D.signalBody = body }

polybarHook :: D.Client -> PP
polybarHook dbus =
  let wrapper c s | s /= "NSP" = wrap ("%{F" <> c <> "} ") " %{F-}" s
                  | otherwise  = mempty
      blue   = "#268bd2"
      gray   = "#96A7A9"
      orange = "#cb4b16"
      purple = "#6c71c4"
      red    = "#dc322f"
  in  def { ppOutput          = dbusOutput dbus
          , ppCurrent         = wrapper blue
          , ppVisible         = wrapper gray
          , ppUrgent          = wrapper orange
          , ppHidden          = wrapper gray
          , ppHiddenNoWindows = wrapper red
          , ppOrder           = \(ws : _ : t : _) -> [ws, t]
          , ppTitle           = wrapper purple . shorten 80
          }

myPolybarLogHook dbus = myLogHook <+> dynamicLogWithPP (polybarHook dbus)

------------------------------------------------------------------------
-- Event Handling

myEventHook = docksEventHook <+> ewmhDesktopsEventHook <+> fullscreenEventHook


------------------------------------------------------------------------
-- Status bar and logging

myLogHook = fadeInactiveLogHook 0.9
