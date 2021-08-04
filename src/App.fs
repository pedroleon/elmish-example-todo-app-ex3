module App

open Elmish
open Elmish.React
open Feliz

type State =
    { 
      Count: int 
      maxIncrementReached: bool 
    }

type Msg =
    | Increment
    | Decrement

let incrementStateWithConstraints (state: State) =
  let maximum = 5
  let current = state.Count + 1
  let currentSanitized = min current maximum
  let maxReached = currentSanitized = maximum
  { Count = currentSanitized; maxIncrementReached = maxReached }

let init() =
    { Count = 0; maxIncrementReached = false }

let update (msg: Msg) (state: State): State =
    match msg with
    | Increment -> 
        incrementStateWithConstraints state 
    | Decrement ->
        { state with Count = state.Count - 1; maxIncrementReached = false }

let render (state: State) (dispatch: Msg -> unit) =
  Html.div [
    Html.button [
      prop.onClick (fun _ -> dispatch Increment)
      prop.text "Increment"
      prop.disabled state.maxIncrementReached
    ]

    Html.button [
      prop.onClick (fun _ -> dispatch Decrement)
      prop.text "Decrement"
    ]

    Html.h1 state.Count
  ]

Program.mkSimple init update render
|> Program.withReactSynchronous "app"
|> Program.withConsoleTrace
|> Program.run