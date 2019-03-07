{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib where

import qualified GI.Gtk                        as Gtk
import qualified GI.Gtk.Declarative            as GtkD
import qualified GI.Gtk.Declarative.App.Simple as GtkS

type State = ()

data Event = Closed | ButtonClicked

view' :: State -> GtkS.AppView Gtk.Window Event
view' _ = GtkD.bin
  Gtk.Window
  [ #title GtkD.:= "Demo"
  , GtkD.on #deleteEvent (const (True, Closed))
  ]
  $ GtkD.widget Gtk.Button 
    [ #label GtkD.:= "Hello, World!" 
    , GtkD.on #clicked ButtonClicked
    ]

update' :: State -> Event -> GtkS.Transition State Event
update' _ Closed = GtkS.Exit
update' _ ButtonClicked = GtkS.Exit

runloop :: IO ()
runloop = GtkS.run GtkS.App {GtkS.view = view', GtkS.update = update', GtkS.inputs = [], GtkS.initialState = ()}