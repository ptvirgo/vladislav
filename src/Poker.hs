{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE FlexibleContexts #-}

module Poker where

import Control.Monad (void)
import qualified Data.Text as T
import qualified Network.HTTP.Req as Req

import GI.Gtk
    ( Box (..)
    , Entry (..)
    , entryGetText
    , Label (..)
    , Orientation (..)
    , Window (..)
    )
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple

import qualified Implements as Impl

-- State

data Scheme = HTTP | HTTPS deriving (Eq, Show, Read)
data Method = GET | PUT | POST | DELETE | OPTIONS | PATCH deriving (Eq, Show)

data ReqArgs = ReqArgs
    { reqMethod :: Method
    , reqScheme :: Scheme
    , reqUrl :: T.Text
    } deriving (Eq, Show)

data ReqPage = ReqPage
    { reqArgs :: ReqArgs
    } deriving (Eq, Show)

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
update' s (PickMethod method) = updateMethod s method
update' s (PickUrl url) = updateUrl s url

updateScheme :: State -> Scheme -> Transition State Event
updateScheme s scheme =
    let args = reqArgs s
        newArgs = args { reqScheme = scheme }
        newPage = s { reqArgs = newArgs }
    in Transition newPage $ return Nothing

updateMethod :: State -> Method -> Transition State Event
updateMethod s method =
    let args = reqArgs s
        newArgs = args { reqMethod = method }
        newPage = s { reqArgs = newArgs }
    in Transition newPage $ return Nothing

updateUrl :: State -> T.Text -> Transition State Event
updateUrl s url =
    let args = reqArgs s
        newArgs = args { reqUrl = url }
        newPage = s { reqArgs = newArgs }
    in Transition newPage $ return Nothing

-- View

schemeSelection :: State -> Maybe T.Text -> Event
schemeSelection _ (Just "HTTP") = PickScheme HTTP
schemeSelection _ (Just "HTTPS") = PickScheme HTTPS
schemeSelection s _ = PickScheme . reqScheme . reqArgs $ s


methodSelection :: State -> Maybe T.Text -> Event
methodSelection _ (Just "GET") = PickMethod GET
methodSelection _ (Just "PUT") = PickMethod PUT
methodSelection _ (Just "POST") = PickMethod POST
methodSelection _ (Just "DELETE") = PickMethod DELETE
methodSelection _ (Just "OPTIONS") = PickMethod OPTIONS
methodSelection _ (Just "PATCH") = PickMethod PATCH
methodSelection s _ = PickMethod . reqMethod . reqArgs $ s


view' :: State -> AppView Window Event
view' s =
    bin
        Window
        [ #title := "Vladistav the Poker"
        , on #deleteEvent (const (True, Closed))
        , #widthRequest := 800
        , #heightRequest := 600
        ]
        $ container Box [ #orientation := OrientationVertical ]
        [ requestSelector s
        , widget Label [#label := (T.pack . show $ s)]
        ]


requestSelector :: State -> BoxChild Event
requestSelector s = container Box
    [ #orientation := OrientationHorizontal ]
    [ BoxChild defaultBoxChildProperties
        $ Impl.comboBox [GET, POST, PUT, DELETE, OPTIONS, PATCH] []
          (methodSelection s)
    , BoxChild defaultBoxChildProperties
        $ Impl.comboBox [HTTPS, HTTP] [] (schemeSelection s)
    , BoxChild defaultBoxChildProperties
        $ widget Entry
            [ #text := (reqUrl . reqArgs $ s)
            , onM #changed (\x -> PickUrl <$> entryGetText x)
            ]
    ]


defaultMain :: IO ()
defaultMain = void $ run App
    { view = view'
    , update = update'
    , inputs = []
    , initialState = defaultState
    }
