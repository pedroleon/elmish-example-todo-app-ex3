module App

open System
open Elmish
open Elmish.React
open Feliz
open Zanaptak.TypedCssClasses

type tw = CssClasses<"https://unpkg.com/tailwindcss@^1.0/dist/tailwind.min.css", Naming.Verbatim>

type Filter = 
  | All
  | Done
  | Pending

type Todo = {
  Id : Guid
  Description : string
  Completed : bool
}

type TodoBeingEdited = {
  Id: Guid
  Description: string
  CurrentDescription: string 
}

type State = {
  TodoList: Todo list
  FilteredList: Todo list
  FilteredBy: Filter 
  NewTodo : string
  TodosBeingEdited : TodoBeingEdited list 
}

type SetEditionMsgParams = {
  Todo: TodoBeingEdited
  NewText: string
}

type Msg =
  | SetNewTodo of string
  | AddNewTodo
  | DeleteTodo of Guid
  | ToggleCompleted of Guid
  | CancelEdit of TodoBeingEdited
  | ApplyEdit of TodoBeingEdited
  | StartEditingTodo of Guid
  | SetEditedDescription of SetEditionMsgParams 
  | SetFilter of Filter

let initialTodoList = [
    { Id = Guid.NewGuid(); Description = "Learn F#"; Completed = false }
    { Id = Guid.NewGuid(); Description = "Learn Elmish"; Completed = true }
  ]

let filterTodoList (filter: Filter) (list: Todo list) = 
    match filter with
      | All -> list
      | Done -> List.filter (fun (todo: Todo) -> todo.Completed) list
      | Pending -> List.filter (fun (todo: Todo) -> not todo.Completed) list

let todosBeignEditedWithout (todos: TodoBeingEdited list) (todoBeingEdited: TodoBeingEdited) =
    todos |> List.filter (fun todo -> todo.Id <> todoBeingEdited.Id )
           

let init() = {
  TodoList = initialTodoList
  FilteredList = initialTodoList
  TodosBeingEdited = []
  FilteredBy = All
  NewTodo = ""
}

let update (msg: Msg) (state: State) =
  match msg with
  | SetNewTodo desc ->
      { state with NewTodo = desc }

  | AddNewTodo when String.IsNullOrWhiteSpace state.NewTodo ->
      state

  | AddNewTodo ->
      let nextTodoId = Guid.NewGuid()

      let nextTodo =
        { Id = nextTodoId
          Description = state.NewTodo
          Completed = false }

      let nextTodoList = List.append state.TodoList [nextTodo]
      { state with
          NewTodo = ""
          TodoList = nextTodoList
          FilteredList = filterTodoList state.FilteredBy nextTodoList }

  | DeleteTodo todoId ->
      let nextTodoList =
        state.TodoList
        |> List.filter (fun todo -> todo.Id <> todoId)

      { state with 
          TodoList = nextTodoList
          FilteredList = filterTodoList state.FilteredBy nextTodoList }

  | ToggleCompleted todoId ->
      let nextTodoList =
        state.TodoList
        |> List.map (fun todo ->
           if todo.Id = todoId     
           then { todo with Completed = not todo.Completed }
           else todo)

      { state with 
          TodoList = nextTodoList 
          FilteredList = filterTodoList state.FilteredBy nextTodoList }

  | StartEditingTodo todoId ->
      let nextEditModel =
        state.TodoList
        |> List.tryFind (fun todo -> todo.Id = todoId)
        |> Option.map (fun todo -> { Id = todoId; Description = todo.Description; CurrentDescription = todo.Description })
      
      
      match nextEditModel with
        | Some editModel -> { state with TodosBeingEdited = List.append state.TodosBeingEdited [editModel] }
        | None -> state

  | CancelEdit todoBeingEdited ->
      { state with TodosBeingEdited = (todosBeignEditedWithout state.TodosBeingEdited todoBeingEdited) }

  | ApplyEdit todoBeingEdited ->
      match todoBeingEdited with
      | todoBeingEdited when todoBeingEdited.Description = "" ->          
          { state with TodosBeingEdited = (todosBeignEditedWithout state.TodosBeingEdited todoBeingEdited) }
      | todoBeingEdited ->
          let nextTodoList =
            state.TodoList
            |> List.map (fun todo ->
                if todo.Id = todoBeingEdited.Id
                then { todo with Description = todoBeingEdited.Description }
                else todo)

          { state with 
               TodoList = nextTodoList
               TodosBeingEdited = (todosBeignEditedWithout state.TodosBeingEdited todoBeingEdited) 
               FilteredList = filterTodoList state.FilteredBy nextTodoList }

  | SetEditedDescription setEditedDescriptionParams ->
      let nextTodosBeignEdited =
        state.TodosBeingEdited |> List.map (fun todo -> 
            if setEditedDescriptionParams.Todo.Id = todo.Id then
              { todo with Description = setEditedDescriptionParams.NewText }
            else
              todo 
        )
      { state with TodosBeingEdited = nextTodosBeignEdited  }

  | SetFilter filter -> 
    { state with 
        FilteredBy = filter
        FilteredList = filterTodoList filter state.TodoList }

let buttonClasses = [ tw.border; tw.``bg-gray-300``; tw.``px-4``; tw.``py-2``] 
let primaryButtonClasses = List.append buttonClasses [tw.``bg-green-300``]
let cancelButtonClasses = List.append buttonClasses [tw.``bg-white``]
let inputClasses = [tw.border; tw.``border-gray-200``; tw.``p-2``; tw.``w-6/12``]
let formFieldClasses = [tw.flex; tw.``my-2``; tw.``min-w-full``] 
let labelClasses = [tw.``bg-gray-100``; tw.border; tw.``p-2``; tw.``w-6/12``]
// Helper function to easily construct div with only classes and children
let div (classes: string list) (children: ReactElement list) =
    Html.div [
        prop.classes classes
        prop.children children
    ]

let appTitle =
    Html.p [
      prop.className "title"
      prop.text "Elmish To-Do List exercise 3"
    ]

let inputField (state: State) (dispatch: Msg -> unit) =
  div formFieldClasses [
    Html.input [
      prop.classes inputClasses
      prop.valueOrDefault state.NewTodo
      prop.onTextChange (SetNewTodo >> dispatch)
      prop.onChange (SetNewTodo >> dispatch)
      prop.onKeyUp(key.enter, fun _ -> dispatch AddNewTodo)
    ]

    div [ "control" ] [
      Html.button [
        prop.className primaryButtonClasses 
        prop.onClick (fun _ -> dispatch AddNewTodo)
        prop.children [
          Html.i [ prop.classes [ "fa"; "fa-plus" ] ]
          Html.text "Add todol"
        ]
      ]
    ]
  ]

let renderTodo (todo: Todo) (dispatch: Msg -> unit) =
  div [ "box" ] [
    div formFieldClasses [
      div labelClasses [
        Html.p [
          prop.className "subtitle"
          prop.text todo.Description
        ]
      ]

      div [ "column"; "is-narrow" ] [
        div [ "buttons" ] [
          Html.button [
            prop.className buttonClasses 
            prop.style [ style.backgroundColor (if todo.Completed then color.orange else color.lightSeaGreen )]
            prop.onClick (fun _ -> dispatch (ToggleCompleted todo.Id))
            prop.children [
              Html.i [ prop.classes [ "fa"; "fa-check" ] ]
              Html.text  (if todo.Completed then "Mark as pending" else "Mark as done")
            ]
          ]

          Html.button [
            prop.classes buttonClasses
            prop.onClick (fun _ -> dispatch (StartEditingTodo  todo.Id))
            prop.children [
              Html.i [ prop.classes [ "fa"; "fa-edit" ] ]
              Html.text "Edit"
            ]
          ]

          Html.button [
            prop.classes buttonClasses
            prop.onClick (fun _ -> dispatch (DeleteTodo todo.Id))
            prop.children [ 
              Html.i [ prop.classes [ "fa"; "fa-times" ] ]
              Html.text "Delete"
            ]
          ]
        ]
      ]
    ]
  ]


let renderEditForm (todoBeingEdited: TodoBeingEdited) (dispatch: Msg -> unit) =
  let todoBeingEditedDescriptionUnchanged = todoBeingEdited.Description = todoBeingEdited.CurrentDescription
  div [ "box" ] [
    div [ tw.flex ] [
      div [ "control is-expanded" ] [
        Html.input [
          prop.classes inputClasses
          prop.valueOrDefault todoBeingEdited.Description;
          prop.onTextChange (fun text -> { NewText = text; Todo = todoBeingEdited}  |> SetEditedDescription |> dispatch )
        ]
      ]

      div [ "control"; "buttons" ] [
        Html.button [
          prop.classes primaryButtonClasses
          prop.style [ style.backgroundColor ( if todoBeingEditedDescriptionUnchanged then color.white else color.lightBlue) ]
          prop.onClick (fun _ -> todoBeingEdited |> ApplyEdit |> dispatch )
          prop.children [
            Html.i [ prop.classes ["fa"; "fa-save" ] ]
            Html.text  ( if todoBeingEditedDescriptionUnchanged then "Cancel edit" else "Save Todo")
          ]
        ]

        Html.button [
          prop.classes cancelButtonClasses
          prop.onClick (fun _ -> dispatch (CancelEdit todoBeingEdited))
          prop.children [
            Html.i [ prop.classes ["fa"; "fa-arrow-right"] ]
            Html.text "Cancel"
          ]
        ]
      ]
    ]
  ]

let todoList (state: State) (dispatch: Msg -> unit) =
  let renderTodoComponent (todosBeingEdited: TodoBeingEdited list) (todo: Todo) =
    let todoBeingEditedOption = todosBeingEdited |> List.tryFind( fun todoBeingEdited -> todoBeingEdited.Id = todo.Id  )

    match todoBeingEditedOption with
      | Some todoBeignEdited -> renderEditForm todoBeignEdited dispatch
      | None -> renderTodo todo dispatch

  let renderTodoComponents = state.FilteredList |> List.map (renderTodoComponent state.TodosBeingEdited)  
  match state.FilteredList with
    | [] -> Html.div [ 
        prop.style [ style.margin 0 ]
        prop.text "This list is empty!"
      ]
    | list -> Html.ul [prop.children renderTodoComponents]
            

let filterTypeToString (filter: Filter ) =
  match filter with
    | All -> "All"
    | Done -> "Completed"
    | Pending -> "Pending"
    
let filterButton (current: Filter) (filter: Filter) (dispatch: Msg -> unit) = 
   Html.button [
      prop.text (filterTypeToString filter);
      prop.className [tw.``mx-1``; tw.``w-40``; tw.border; tw.rounded; tw.``p-2``; (if current = filter then tw.``bg-blue-400`` else tw.``bg-white``)]
      prop.onClick (fun _ -> dispatch (SetFilter filter))
    ]


let filterBar (state: State) (dispatch: Msg -> unit) =
  Html.div [
    prop.style [style.paddingBottom 20]
    prop.children [
      filterButton state.FilteredBy All dispatch
      filterButton state.FilteredBy Done dispatch
      filterButton state.FilteredBy Pending dispatch
    ]
  ]

let render (state: State) (dispatch: Msg -> unit) =
  Html.div [
    prop.className [tw.``mx-auto``; tw.container;]
    prop.style [ style.padding 20 ]
    prop.children [
      appTitle
      inputField state dispatch
      filterBar state dispatch
      todoList state dispatch
    ]
  ]

Program.mkSimple init update render
|> Program.withReactSynchronous "app"
|> Program.withConsoleTrace
|> Program.run