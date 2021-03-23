{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE FlexibleContexts #-}

module ReqArgs where

import qualified Data.Text as T

import GI.Gtk
    ( Box (..)
    , Entry (..)
    , entryGetText
    , Orientation (..)
    )
import GI.Gtk.Declarative
import qualified Implements as Impl

-- State

data Scheme = HTTP | HTTPS deriving (Eq, Show, Read)
data Method = GET | PUT | POST | DELETE | OPTIONS | PATCH deriving (Eq, Show)

data ReqArgs = ReqArgs
    { reqMethod :: Method
    , reqScheme :: Scheme
    , reqUrl :: T.Text
    } deriving (Eq, Show)

type State = ReqArgs

defaultState = ReqArgs
    { reqMethod = GET
    , reqScheme = HTTPS
    , reqUrl = "www.example.com"
    }

-- Updates

data Event = PickScheme Scheme | PickMethod Method | PickUrl T.Text

update' :: State -> Event -> State
update' s (PickScheme scheme) = s { reqScheme = scheme }
update' s (PickMethod method) = s { reqMethod = method }
update' s (PickUrl url) = s { reqUrl = url }

-- View

schemeSelector :: State -> Maybe T.Text -> Event
schemeSelector _ (Just "HTTP") = PickScheme HTTP
schemeSelector _ (Just "HTTPS") = PickScheme HTTPS
schemeSelector s _ = PickScheme . reqScheme $ s


methodSelector :: State -> Maybe T.Text -> Event
methodSelector _ (Just "GET") = PickMethod GET
methodSelector _ (Just "PUT") = PickMethod PUT
methodSelector _ (Just "POST") = PickMethod POST
methodSelector _ (Just "DELETE") = PickMethod DELETE
methodSelector _ (Just "OPTIONS") = PickMethod OPTIONS
methodSelector _ (Just "PATCH") = PickMethod PATCH
methodSelector s _ = PickMethod . reqMethod $ s


requestSelector :: (Event -> wrapper) -> State -> BoxChild wrapper
requestSelector wrapper s = container Box
    [ #orientation := OrientationHorizontal ]
    [ BoxChild defaultBoxChildProperties
        $ Impl.comboBox [GET, POST, PUT, DELETE, OPTIONS, PATCH] []
          (wrapper . methodSelector s)
    , BoxChild defaultBoxChildProperties
        $ Impl.comboBox [HTTPS, HTTP] [] (wrapper . schemeSelector s)
    , BoxChild defaultBoxChildProperties
        $ widget Entry
            [ #text := reqUrl s
            , onM #changed (\x -> (wrapper . PickUrl) <$> entryGetText x)
            ]
    ]
