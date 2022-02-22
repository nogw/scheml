let rec input prompt callback =
  match LNoise.linenoise prompt with
  | None -> ()
  | Some v ->
      callback v;
      input prompt callback

let main callback =
  LNoise.history_load ~filename:"history.txt" |> ignore;
  LNoise.history_set ~max_length:100 |> ignore;

  (fun from_user ->
    if from_user = "quit" then exit 0;
    LNoise.history_add from_user |> ignore;
    LNoise.history_save ~filename:"history.txt" |> ignore;
    from_user |> callback)
  |> input "user> "
