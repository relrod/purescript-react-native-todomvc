module Styles where

import Prelude
import ReactNative.Styles as S
import React.DOM.Props as P

appStyleSheet :: S.StyleSheet
appStyleSheet = S.createStyleSheet [
  S.Style "container" [
     S.flex 1.0,
     S.flexDirection S.Column,
     S.backgroundColor backgroundColor
     ],
  S.Style "title" [
     S.fontSize 50.0,
     S.color "rgba(175, 47, 47, 0.15)",
     S.textAlign S.TextAlignCenter
     ],
  S.Style "todoList" [
    S.flex 1.0,
    S.flexDirection S.Column
    ],
  S.Style "newTodoContainer" [
    S.paddingHorizontal 10.0,
    S.height 56.0,
    S.backgroundColor todoBackgroundColor,
    S.borderTopColor borderColor,
    S.borderTopWidth 1.0,
    S.borderBottomColor borderColor,
    S.borderBottomWidth 1.0
    ],
  S.Style "newTodo" [
    S.fontSize 18.0,
    S.paddingHorizontal 10.0,
    S.flex 1.0,
    S.backgroundColor todoBackgroundColor,
    S.textDecorationColor fontColorFaded
    ],
  S.Style "todo" [
    S.paddingHorizontal 10.0,
    S.paddingVertical 15.0,
    S.backgroundColor todoBackgroundColor
    ],
  S.Style "todoText" [
    S.fontSize 18.0,
    S.color fontColorDefault
    ],
  S.Style "todoTextCompleted" [
    S.color fontColorFaded
    ],
  S.Style "separator" [
    S.backgroundColor borderColor,
    S.height 1.0
    ],
  S.Style "bottomBar" [
    S.paddingVertical 10.0,
    S.paddingHorizontal 15.0,
    S.borderTopColor borderColor,
    S.borderTopWidth 1.0,
    S.flexDirection S.Row,
    S.alignItems S.AlignItemsStretch,
    S.backgroundColor todoBackgroundColor
    ],
  S.Style "filters" [
    S.flexDirection S.Row,
    S.alignItems S.AlignItemsStretch,
    S.flex 1.0
    ],
  S.Style "filter" [
    S.marginHorizontal 5.0,
    S.padding 5.0
    ],
  S.Style "activeFilter" [
    S.borderWidth 1.0,
    S.borderColor "rgba(175, 47, 47, 0.2)",
    S.borderStyle S.BorderSolid,
    S.borderRadius 3.0
    ],
  S.Style "clearCompleted" [
    S.margin 5.0
    ]
  ]

fontColorDefault :: String
fontColorDefault = "#000000" 
fontColorFaded :: String
fontColorFaded = "#D9D9D9" 
backgroundColor :: String
backgroundColor = "#F5F5F5"
todoBackgroundColor :: String
todoBackgroundColor = "#FFFFFF"
borderColor :: String
borderColor = "#EDEDED"
  
style :: String -> P.Props
style key = S.style $ S.getStyleId appStyleSheet key
  
styles :: Array String -> P.Props
styles keys = S.styles $ map (S.getStyleId appStyleSheet) keys
