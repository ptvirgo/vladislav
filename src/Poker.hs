{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE FlexibleContexts #-}

module Poker where

import Control.Monad (void)
import qualified Data.Text as T
import qualified Network.HTTP.Req as Req

import qualified ReqArgs as RA

import GI.Gtk
    ( Box (..)
    , Label (..)
    , Orientation (..)
    , Window (..)
    )
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple

-- State

data ReqPage = ReqPage
    { reqArgs :: RA.ReqArgs
    } deriving (Eq, Show)

type State = ReqPage

defaultState = ReqPage
    { reqArgs = RA.defaultState
    }

-- Updates

data Event = UpdateReqArgs RA.Event | Closed

update' :: State -> Event -> Transition State Event
update' _ Closed = Exit
update' s (UpdateReqArgs a) =
    Transition s { reqArgs = RA.update' (reqArgs s) a } $ return Nothing


-- View

view' :: State -> AppView Window Event
view' s =
    bin
        Window
        [ #title := "Vladislav the Poker"
        , on #deleteEvent (const (True, Closed))
        , #widthRequest := 800
        , #heightRequest := 600
        ]
        $ container Box [ #orientation := OrientationVertical ]
        [ RA.requestSelector UpdateReqArgs (reqArgs s)
        , widget Label [#label := (T.pack . show $ s)]
        ]

defaultMain :: IO ()
defaultMain = void $ run App
    { view = view'
    , update = update'
    , inputs = []
    , initialState = defaultState
    }
