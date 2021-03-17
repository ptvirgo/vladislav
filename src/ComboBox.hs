{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ComboBox where

import Control.Monad

import qualified Data.Text as T
import qualified Data.Vector as V

import qualified GI.Gtk as Gtk
import qualified GI.GObject as GI

import GI.Gtk.Declarative
import GI.Gtk.Declarative.EventSource ( fromCancellation )

comboBox :: (Show vis)
    => V.Vector vis
    -> V.Vector (Attribute Gtk.Box event)
    -> (Maybe T.Text -> event)
    -> Widget event
comboBox items attributes event = Widget (CustomWidget
    { customWidget = cw
    , customCreate = cc
    , customPatch = cpatch
    , customSubscribe = cs
    , customAttributes = attributes
    , customParams = mempty :: [Int]
    }) where
    cw = Gtk.Box
    cc props = do
        wrap <- Gtk.new Gtk.Box []
        selector <- Gtk.new Gtk.ComboBoxText []
        mapM_ (Gtk.comboBoxTextAppendText selector . T.pack . show) items
        Gtk.comboBoxSetActive selector 0
        #packStart wrap selector False False 5
        return (wrap, selector)
    cpatch _ _ _ = CustomKeep
    cs _params selector _box eventIO = do
        h <- Gtk.on (selector :: Gtk.ComboBoxText) #changed
             $ eventIO . event =<< #getActiveText selector
        return (fromCancellation (GI.signalHandlerDisconnect selector h))
