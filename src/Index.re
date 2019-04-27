module App = {
  module Task = {
    type status =
      | Pending
      | Done;

    type t = {
      id: string,
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
      tasks: [
        {
          id: Js.Date.now() |> Js.Float.toString,
          label: "Learn Reason",
          status: Pending,
        },
      ],
    };
  };

  type action =
    | Write(string)
    | Add_task
    | Complete_task(string);

  let reducer = (state, action) => {
    let new_state =
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
              {
                id: Js.Date.now() |> Js.Float.toString,
                label,
                status: Pending,
              },
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
    Js.log(new_state);
    new_state;
  };
};

module Component = {
  module Task = {
    [@react.component]
    let make = (~task, ~onDone) => {
      <li>
        <span>
          {switch (task.App.Task.status) {
           | Pending => "pending" |> React.string
           | Done => "done" |> React.string
           }}
        </span>
        <span> App.Task.(React.string(task.label)) </span>
        <button onClick={_ => onDone(task.App.Task.id)}>
          {React.string("done")}
        </button>
      </li>;
    };
  };

  module App = {
    [@react.component]
    let make = (~initial_state) => {
      let (state, dispatch) = React.useReducer(App.reducer, initial_state);
      <div>
        <input
          value={
            switch (state.App.State.input) {
            | None => ""
            | Some(str) => str
            }
          }
          onChange={e => {
            let text = ReactEvent.Form.target(e)##value;
            dispatch(Write(text));
          }}
        />
        <button onClick={_ => dispatch(Add_task)}>
          {React.string("add")}
        </button>
        <ul>
          App.State.(
            state.tasks
            |> List.map(task =>
                 <Task
                   key={task.App.Task.id}
                   task
                   onDone={task_id => dispatch(Complete_task(task_id))}
                 />
               )
            |> Array.of_list
            |> React.array
          )
        </ul>
      </div>;
    };
  };
};
let initial_state = App.State.initial_state;
ReactDOMRe.renderToElementWithId(<Component.App initial_state />, "app");
