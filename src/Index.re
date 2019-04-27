module App = {
  module Task = {
    type status =
      | Pending
      | Done;

    type t = {
      id: float,
      label: string,
      status,
    };
  };

  module State = {
    type t = {
      input: option(string),
      tasks: list(Task.t),
    };

    let initial_state = {
      input: None,
      tasks: [{id: Js.Date.now(), label: "Learn Reason", status: Pending}],
    };
  };

  type action =
    | Write(string)
    | Add_task
    | Complete_task(float);

  let reducer = (state, action) => {
    switch (action) {
    | Write("") => State.{...state, input: None}
    | Write(text) => State.{...state, input: Some(text)}
    | Add_task =>
      switch (state.input) {
      | None => state
      | Some(label) =>
        State.{
          input: None,
          tasks: [
            {id: Js.Date.now(), label, status: Pending},
            ...state.tasks,
          ],
        }
      }
    | Complete_task(id) =>
      let new_tasks =
        state.tasks
        |> List.map(task =>
             switch (id == task.Task.id) {
             | true => {...task, status: Done}
             | _ => task
             }
           );
      State.{...state, tasks: new_tasks};
    };
  };
};

module Component = {
  module Task = {
    [@react.component]
    let make = (~task) => {
      <li>
        <span> App.Task.(React.string(task.label)) </span>
        <button> {React.string("done")} </button>
      </li>;
    };
  };

  module App = {
    [@react.component]
    let make = (~state) => {
      <div>
        <input />
        <button> {React.string("add")} </button>
        <ul>
          App.State.(
            state.tasks
            |> List.map(task => <Task task />)
            |> Array.of_list
            |> React.array
          )
        </ul>
      </div>;
    };
  };
};

let state = App.State.initial_state;
ReactDOMRe.renderToElementWithId(<Component.App state />, "app");
