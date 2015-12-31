module Main where

import Prelude
import Control.Monad.Eff (Eff())
import Control.Monad.Eff.Console (CONSOLE(), log)
import Control.Monad.Eff.Unsafe (unsafeInterleaveEff)
import Data.Array ((:), filter, sortBy)
import Data.Generic (class Generic, gEq)
import React (ReactElement(), createFactory)
import ReactNative (registerComponent)
import ReactNative.Components (ListViewDataSource(), cloneWithRows, listView, listViewDataSource, text, textInput, touchableHighlight, view)

import ReactNative.Props as N
import React.DOM.Props as P
import ReactNative.Styles as S
import Thermite as T

import Styles (style, styles)

data AppState = AppState {
  nextId :: Int, 
  newTodo :: String, 
  todos :: Array Todo, 
  dataSource :: ListViewDataSource, 
  filter :: Filter
  }

data Todo = Todo Int String Boolean

derive instance genericTodo :: Generic Todo

instance eqTodo :: Eq Todo where
  eq = gEq

getTodoId :: Todo -> Int
getTodoId (Todo id _ _) = id

data TodoListAction 
  = UpdateNewTodo String
  | AddTodo
  | ClearCompleted
  | FilterTodos Filter
  
data Filter = All | Active | Completed
instance eqFilter :: Eq Filter where
  eq All       All       = true
  eq Active    Active    = true
  eq Completed Completed = true
  eq _         _         = false

initialTodos :: Array Todo
initialTodos = [
  Todo 1 "Hack PureScript into Android (using JS mostly)" true,
  Todo 2 "Display text field using purescript-react" true,
  Todo 3 "Port SampleApp to PureScript" true,
  Todo 4 "Pull out bindings as separate library" true,
  Todo 5 "Add support for ListViews" true,
  Todo 6 "Display list of todo items" true,
  Todo 7 "Fix wrapping of long todos" true,
  Todo 8 "Display completed items as completed" true,
  Todo 9 "Make items highlightable" true,
  Todo 10 "Make items completable" true,
  Todo 12 "Add new todos" true,
  Todo 13 "Clear completed todos" true,
  Todo 14 "Filter All/Active/Completed todos" true,
  Todo 15 "Delete todos" false,
  Todo 16 "Clean up styling" false,
  Todo 17 "Re-focus input field when adding todo" false
]
    
todoListSpec :: forall props eff. T.Spec eff AppState props TodoListAction
todoListSpec = T.simpleSpec updateAppState render
  where
    render :: T.Render AppState props TodoListAction
    render dispatch _ (AppState state) _ = 
      [view [(style "container")] $ [
        text [style "title"] "todos",
        view [style "newTodoContainer"] [
          textInput [style "newTodo", 
                     P.value state.newTodo,
                     P.placeholder "What needs to be done?",
                     N.onChangeText \text -> dispatch (UpdateNewTodo text),
                     N.onSubmitEditing \_ -> dispatch AddTodo
                     ]],
        listView [style "todoList",
                  N.renderRow rowFn,
                  N.renderSeparator todoSeparator,
                  N.renderHeader $ view [style "separator"] [],
                  N.dataSource state.dataSource],
        view [style "bottomBar"] [
          view [style "filters"] [
             filterButton dispatch state.filter All, 
             filterButton dispatch state.filter Active,
             filterButton dispatch state.filter Completed],
          text [style "clearCompleted", N.onPress \_ -> dispatch ClearCompleted] "Clear completed"]]]

data TodoAction = ToggleTodo

rowFn :: forall highlightFn. Todo -> String -> String -> highlightFn -> ReactElement
rowFn todo sectionId rowId highlightRow = 
  createFactory (createClass' spec todo) todo
  where
    spec :: forall eff props. T.Spec eff Todo props TodoAction
    spec = T.simpleSpec updateTodo render
    updateTodo :: forall props eff. T.PerformAction eff Todo props TodoAction
    updateTodo ToggleTodo _ (Todo id item completed) k = k $ Todo id item (not completed)
    render :: forall props. T.Render Todo props TodoAction
    render dispatch _ (Todo id item completed) _ = 
      [touchableHighlight [N.onPress \_ -> dispatch ToggleTodo] $
        view [style "todo"] [
          text [styles textStyles] item]]
      where textStyles = (if completed then ["todoText", "todoTextCompleted"] else ["todoText"])

todoSeparator :: N.RenderSeparatorFn
todoSeparator sectionId rowId adjacentHighlighted = view [style "separator"] []
          
filterButton :: (TodoListAction -> T.EventHandler) -> Filter -> Filter -> ReactElement
filterButton dispatch activeFilter filter = 
  view [styles (if activeFilter == filter then ["filter", "activeFilter"] else ["filter"])] [
    text [N.onPress \_ -> dispatch $ FilterTodos filter] filterText]
  where filterText = case filter of 
          All -> "All"
          Active -> "Active"
          Completed -> "Completed"
        
updateAppState :: forall props eff. T.PerformAction eff AppState props TodoListAction
updateAppState action _ state k = k $ updateAppState' state action

updateAppState' :: AppState -> TodoListAction -> AppState
updateAppState' (AppState state) (UpdateNewTodo text) = AppState state { newTodo = text }
updateAppState' (AppState state) AddTodo = updateDataSource $ AppState newState
  where newState = state { nextId = state.nextId + 1, newTodo = "", todos = todos}
        todos = (Todo state.nextId state.newTodo false) : state.todos
updateAppState' (AppState state) ClearCompleted  = updateDataSource $ AppState state { todos = newTodos }
  where newTodos = filter notCompleted state.todos
        notCompleted (Todo _ _ completed) = not completed
updateAppState' (AppState state) (FilterTodos filter) = updateDataSource $ AppState state { filter = filter }

updateDataSource :: AppState -> AppState
updateDataSource (AppState state) = AppState $ state { dataSource = cloneWithRows state.dataSource filteredTodos }
  where filteredTodos = sortBy todoOrdering $ filter (applyFilter state.filter) state.todos

todoOrdering :: Todo -> Todo -> Ordering
todoOrdering (Todo _ _ true) (Todo _ _ false) = GT
todoOrdering (Todo _ _ false) (Todo _ _ true) = LT
todoOrdering (Todo id1 _ _) (Todo id2 _ _) = if id1 < id2 then LT else GT

applyFilter :: Filter -> Todo -> Boolean
applyFilter All _ = true
applyFilter Active (Todo _ _ c) = not c
applyFilter Completed (Todo _ _ c) = c
        
foreign import unsafeLog :: forall p e. p -> Eff e Unit
foreign import unsafeLog2 :: forall p. p -> p
  
main :: Eff (console :: CONSOLE) Unit
main = do
  log "Running app"
  registerComponent "PureScriptSampleApp" component
  where
    component = createClass' todoListSpec initialState
    dataSource = listViewDataSource initialTodos
    initialState = updateDataSource $ AppState { nextId: 18, 
                                                 newTodo: "", 
                                                 todos: initialTodos, 
                                                 dataSource: dataSource, 
                                                 filter: All }


-- Copied from Thermite project because Thermite uses divs when generating
-- the UI but we need views. This also requires one tiny change to Thermite
-- to expose the T.Spec data constructor.

createClass' :: forall eff state props action. T.Spec eff state props action -> state -> React.ReactClass props
createClass' spec state = React.createClass <<< _.spec $ createReactSpec spec state

createReactSpec ::
  forall eff state props action.
  T.Spec eff state props action ->
  state ->
  { spec :: React.ReactSpec props state eff
  , dispatcher :: React.ReactThis props state -> action -> T.EventHandler
  }
createReactSpec (T.Spec spec) state =
  { spec: React.spec state render
  , dispatcher: dispatch
  }
  where
  dispatch :: React.ReactThis props state -> action -> T.EventHandler
  dispatch this action = do
    props <- React.getProps this
    state <- React.readState this
    unsafeInterleaveEff $ spec.performAction action props state (void <<< unsafeInterleaveEff <<< React.writeState this)

  render :: React.Render props state eff
  render this = map (view [S.style' [S.flex 1.0]]) $
    spec.render (dispatch this)
      <$> React.getProps this
      <*> React.readState this
      <*> React.getChildren this
