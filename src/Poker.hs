{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}

module Poker where

import Control.Monad (void)
import qualified Data.Text as T
import qualified Network.HTTP.Req as Req

import GI.Gtk
    ( Box (..)
    , Orientation (..)
    , Window (..)
    )
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple

import qualified ComboBox as CB

-- State

data Scheme = HTTP | HTTPS deriving (Eq, Show)
data Method = GET | PUT | POST | DELETE | OPTIONS | PATCH deriving (Eq, Show)

data ReqArgs = ReqArgs
    { reqMethod :: Method
    , reqScheme :: Scheme
    , reqUrl :: T.Text
    }

data ReqPage = ReqPage
    { reqArgs :: ReqArgs
    }

type State = ReqPage

defaultState = ReqPage
    { reqArgs = ReqArgs
                { reqMethod = GET
                , reqScheme = HTTPS
                , reqUrl = "www.example.com"
                }
    }


-- Updates

data Event = PickScheme Scheme | PickMethod Method | PickUrl T.Text | Closed

update' :: State -> Event -> Transition State Event
update' _ Closed = Exit
update' s (PickScheme scheme) = updateScheme s scheme
update' s _ = Transition s (return Nothing) 

updateScheme :: State -> Scheme -> Transition State Event
updateScheme s scheme =
    let args = reqArgs s
        newArgs = args { reqScheme = scheme }
        newPage = s { reqArgs = newArgs }
    in Transition newPage (return Nothing)

-- View

schemeSelection :: State -> Maybe T.Text -> Event
schemeSelection _ (Just "HTTP") = PickScheme HTTP
schemeSelection _ (Just "HTTPS") = PickScheme HTTPS
schemeSelection s _ = PickScheme . reqScheme . reqArgs $ s

view' :: State -> AppView Window Event
view' s =
    bin
        Window
        [ #title := "Vladistav the Poker"
        , on #deleteEvent (const (True, Closed))
        , #widthRequest := 800
        , #heightRequest := 600
        ]
        $ container
            Box [ #orientation := OrientationHorizontal ]
                [ BoxChild defaultBoxChildProperties { expand = True }
                    $ CB.comboBox [HTTPS, HTTP] [] (schemeSelection s)]


defaultMain :: IO ()
defaultMain = void $ run App
    { view = view'
    , update = update'
    , inputs = []
    , initialState = defaultState
    }
