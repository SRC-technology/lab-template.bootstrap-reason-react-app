/** Welcome to your first Reason React application!

  If you are familiar with Javascript and React, there should be 3 things that
  may be new to you:

  * Modules
  * Types
  * Pipe operator (|>)

  ## Modules

  Just like Javascript, Reason has modules too. Reason modules are similar to
  Javascript, but they have a few differences:

    * Reason modules are first-class, like functions, and they are defined with
      a special syntax: `module Module_name = { ... };`

    * Modules are global! 

  ## Types

  Reason is strongly and staticaly typed, meaning you have to define the type
  of data you work with beforehand. It is a common practice to have a small
  module that handles one specific type, so we normally call that type `t`.
  This makes it easier to find the main type on any module. It is normally
  `Module_name.t`, and any auxiliary type typically has a name, like
  `Module_name.aux_type_name`.

  ## Pipe operator

  If you haven't used the `|>` operator before, it's called _pipe_, and it lets
  you put the output of a function as the input of another one like this:

  ```reason
  add(1, 2)
  |> duplicate
  ```

  which is equals to writing:

  ```reason
  let three = add(1, 2);
  duplicate(three);
  ```
*/

module Model = {
  /** This module defines 3 things for our application:

      * Tasks, with their statuses and how to create them
      * Action, with the possible actions we can use in our application
      * State, representing the state of our application and its reducer
  */
  module Task = {
    /** The statuses a Task can be in */
    type status =
      | /** A task that is Pending is a task that hasn't been completed. */
        Pending
      | /** A task that is Done is a task that has been completed. */
        Done;

    /** The type of a task in the TooDoo app. */
    type t = {
      /** Every task should have a unique id */
      id: string,
      /** The label to be displayed to the user */
      label: string,
      /** Whether this task has been done or is still pending */
      status,
    };

    /** Task.make("my task") will create a new task with a random id, with
        Pending status, and with "my task" as a label  */
    let make = label => {
      id: Js.Date.now() |> Js.Float.toString,
      label,
      status: Pending,
    };
  };

  module Action = {
    /** The possible actions of our application */
    type t =
      | /** Write is used to capture user input */
        Write(string)
      | /** Add_task will create a new task using the currently captured user
            input that is already on the state */
        Add_task
      | /** Marks a task as completed */
        Complete_task(string);
  };

  module State = {
    /** The type of the state of the application. */
    type t = {
      /** A temporary input that holds whatever the user is typing. May be
          empty, represented as None */
      input: option(string),
      /** A list of tasks the user has created */
      tasks: list(Task.t),
    };

    /** The initial state of the application */
    let initial_state = {input: None, tasks: [Task.make("Learn Reason")]};

    /** Our reducer works very similarly to other reducers you may have seen
        before. If receives the current state, and an action, and will compute
        a new state for the application to continue with. */
    let reducer = (state, action) => {
      let new_state =
        switch (action) {
        /* If the action Write has no text at all, we clear up the input */
        | Action.Write("") => {...state, input: None}

        /* If the action Write has text we save it in the state */
        | Action.Write(text) => {...state, input: Some(text)}

        | Action.Add_task =>
          switch (state.input) {
          /* If we have no input in the state, we can't add a task! */
          | None => state

          /* But if we have some input, we can use it as a label */
          | Some(label) => {
              input: None,
              tasks: [Task.make(label), ...state.tasks],
            }
          }

        | Action.Complete_task(id) =>
          let new_tasks =
            state.tasks
            |> List.map(task =>
                 switch (id == task.Task.id) {
                 | true => {...task, status: Done}
                 | _ => task
                 }
               );
          {...state, tasks: new_tasks};
        };
      /* We log the state for convenience when developing */
      Js.log(new_state);
      new_state;
    };
  };
};

module Components = {
  /** This module creates all the components we will need in our TooDoo
      application:

      * A Task component, that will show a single task
      * An App component, that will glue together the whole application
  */
  module Task = {
    [@react.component]
    let make = (~task, ~onDone) => {
      <li>
        <span>
          {Model.Task.(switch (task.status) {
           | Pending => "pending" |> React.string
           | Done => "done" |> React.string
           })}
        </span>
        <span> Model.Task.(React.string(task.label)) </span>
        <button onClick={_ => onDone(Model.Task.(task.id))}>
          {React.string("done")}
        </button>
      </li>;
    };
  };

  module TooDoo = {
    /** The main component of our application, it initializes TooDoo app with
        an initial state and hooks up the reducer and actions with the DOM
        events. */
    [@react.component]
    let make = () => {
      let (state, dispatch) =
        Model.State.(React.useReducer(reducer, initial_state));

      <div>
        <input
          value={
            Model.State.(
            switch (state.input) {
            | None => ""
            | Some(str) => str
            })
          }
          onChange={e => {
            let text = ReactEvent.Form.target(e)##value;
            Write(text) |> dispatch;
          }}
        />
        <button onClick={_ => dispatch(Add_task)}>
          {React.string("add")}
        </button>
        <ul>
          Model.State.(
            state.tasks
            |> List.map(task =>
                 <Task
                   key={Model.Task.(task.id)}
                   task
                   onDone={task_id => Complete_task(task_id) |> dispatch}
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

ReactDOMRe.renderToElementWithId(<Components.TooDoo />, "app");
