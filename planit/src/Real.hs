{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
module Real where

import           Control.Monad                    (void)
import           Data.Text                        (Text)
import           Data.Vector                      (Vector)
import qualified Data.Vector                   as Vector
import qualified Data.Vector.Mutable           as MVector
import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative
import qualified GI.Gtk.Declarative.App.Simple as GtkS

data Ticket
  = UnestimatedTicket 
    { title :: Text
    }
  | EstimatedTicket 
    { title :: Text
    , myEstimate :: Char
    }

newtype State = State
  { currentTicket :: Ticket
  }

data Event
  = Estimated Char
  | NewEstimate
  | Closed

pointsScale :: Vector Text
pointsScale = ["0", "1", "2", "3", "5", "8", "13", "21", "40", "?"]

view' :: State -> GtkS.AppView Gtk.Window Event
view' s = bin
  Gtk.Window
  [ #title := "Plan it!"
  , on #deleteEvent (const (True, Closed))
  ]
  (container Gtk.Box 
    [ #orientation := Gtk.OrientationVertical ]
    [ticketDisplay, newTicketButton, pointsDisplay]
  )
  where
    newTicketButton = BoxChild defaultBoxChildProperties
      (widget Gtk.Button [#label := "Open new ticket", on #clicked NewEstimate])
    ticketDisplay = BoxChild defaultBoxChildProperties { expand = True, fill = True }
      (container Gtk.Box 
        [ #orientation := Gtk.OrientationVertical
        , #valign := Gtk.AlignCenter
        ]
        [ widget Gtk.Label [ #label := tTitle ] ]
      )
      where 
        tTitle = title $ currentTicket s
    pointsDisplay =
      container Gtk.Box 
        [ #orientation := Gtk.OrientationHorizontal ]
        (fmap (\i ->
          BoxChild defaultBoxChildProperties { expand = True, fill = True } (widget Gtk.Button [#label := ("Points: " <> i)])
          ) pointsScale)

update' :: State -> Event -> GtkS.Transition State Event
update' (State (UnestimatedTicket title')) e = case e of
  Estimated c -> GtkS.Transition (State $ EstimatedTicket title' c) (pure Nothing)
  NewEstimate -> error "Current estimate incomplete!"
  Closed -> GtkS.Exit
update' (State (EstimatedTicket _ _)) e = case e of
  Estimated _ -> error "Cant estimate an estimated ticket!"
  NewEstimate -> GtkS.Transition (State $ UnestimatedTicket "Unestimated ticket") (pure Nothing)
  Closed -> GtkS.Exit

mapAt :: Int -> (a -> a) -> Vector a -> Vector a
mapAt i f = Vector.modify (\v -> MVector.write v i . f =<< MVector.read v i)

runloop :: IO ()
runloop = void $ GtkS.run GtkS.App 
  { GtkS.view = view'
  , GtkS.update = update'
  , GtkS.inputs = []
  , GtkS.initialState = State (UnestimatedTicket "Blank slate ticket")
  }
