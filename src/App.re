open ReasonReact;

module Styles = {
    open Css;
    let container = style([
        width(`percent(80.)),
        margin2(~h=`auto, ~v=`px(64)),
    ]);
    let filter = style([
        fontSize(`px(16)),
        backgroundColor(`hex("444")),
        color(`hex("ED0")),
        width(`percent(100.)),
        borderStyle(`none),
        padding(`px(8)),
    ]);
};

type state = string;
type action = string;

let rows =
    {|
        {"message":"listening","level":"info"}
        {"path":"/get-my-user-data/","ip":"::ffff:127.0.0.1","ytPlaylistId":"PLcrWYEiKEjYBCRUHOuitoAmc_xhNzYjyF","message":"Creating external playlist link","level":"debug"}
        {"path":"/get-my-user-data/","ip":"::ffff:127.0.0.1","ytPlaylistId":"PLcrWYEiKEjYBvbdj6arpFi6xgNid8sWZ9","message":"Creating external playlist link","level":"debug"}
        {"path":"/get-my-user-data/","ip":"::ffff:127.0.0.1","ytPlaylistId":"PLcrWYEiKEjYDlg7ap9iyGOBnnp4XrnMen","message":"Creating external playlist link","level":"debug"}
        {"path":"/get-my-user-data/","ip":"::ffff:127.0.0.1","ytPlaylistId":"PLcrWYEiKEjYBCRUHOuitoAmc_xhNzYjyF","message":"Matched external playlist to local","level":"debug"}
        {"path":"/get-my-user-data/","ip":"::ffff:127.0.0.1","ytPlaylistId":"PLcrWYEiKEjYBCRUHOuitoAmc_xhNzYjyF","message":"Refrshing YT playlist","level":"debug"}
        {"path":"/get-my-user-data/","ip":"::ffff:127.0.0.1","ytPlaylistId":"PLcrWYEiKEjYBvbdj6arpFi6xgNid8sWZ9","message":"Matched external playlist to local","level":"debug"}
        {"path":"/get-my-user-data/","ip":"::ffff:127.0.0.1","ytPlaylistId":"PLcrWYEiKEjYDlg7ap9iyGOBnnp4XrnMen","message":"Matched external playlist to local","level":"debug"}
        {"path":"/get-my-user-data/","ip":"::ffff:127.0.0.1","ytPlaylistId":"PLcrWYEiKEjYBvbdj6arpFi6xgNid8sWZ9","message":"Refrshing YT playlist","level":"debug"}
        {"path":"/get-my-user-data/","ip":"::ffff:127.0.0.1","ytPlaylistId":"PLcrWYEiKEjYDlg7ap9iyGOBnnp4XrnMen","message":"Refrshing YT playlist","level":"debug"}
        {"path":"/get-my-user-data/","ip":"::ffff:127.0.0.1","ytPlaylistId":"PLcrWYEiKEjYBqzCkt_I3FJrvsaZOJwjOe","message":"Creating external playlist link","level":"debug"}
        {"path":"/get-my-user-data/","ip":"::ffff:127.0.0.1","ytPlaylistId":"PLcrWYEiKEjYAID9FpRldiWrzsDznDs1zx","message":"Creating external playlist link","level":"debug"}
        {"path":"/get-my-user-data/","ip":"::ffff:127.0.0.1","ytPlaylistId":"PLcrWYEiKEjYAID9FpRldiWrzsDznDs1zx","message":"Matched external playlist to local","level":"debug"}
        {"path":"/get-my-user-data/","ip":"::ffff:127.0.0.1","ytPlaylistId":"PLcrWYEiKEjYAID9FpRldiWrzsDznDs1zx","message":"Refrshing YT playlist","level":"debug"}
        {"path":"/get-my-user-data/","ip":"::ffff:127.0.0.1","ytPlaylistId":"PLcrWYEiKEjYBqzCkt_I3FJrvsaZOJwjOe","message":"Matched external playlist to local","level":"debug"}
        {"path":"/get-my-user-data/","ip":"::ffff:127.0.0.1","ytPlaylistId":"PLcrWYEiKEjYBqzCkt_I3FJrvsaZOJwjOe","message":"Refrshing YT playlist","level":"debug"}
        {"path":"/get-my-user-data/","ip":"::ffff:127.0.0.1","ytPlaylistId":"PLcrWYEiKEjYBqzCkt_I3FJrvsaZOJwjOe","playlistId":80,"message":"Matched to local","level":"debug"}
        {"path":"/get-my-user-data/","ip":"::ffff:127.0.0.1","ytPlaylistId":"PLcrWYEiKEjYBqzCkt_I3FJrvsaZOJwjOe","playlistId":80,"message":"Found link","level":"debug"}
    |}
    |> Js.String.split("\n")
    |> Js.Array.map(Js.String.trim)
    |> Js.Array.filter(s => Js.String.length(s) !== 0)
    |> Js.Array.map(Js.Json.parseExn);

let filterChange = (event, { send }) => {
    open ReDom;

    let value = ReactEvent.Form.currentTarget(event)
        |> Obj.magic
        |> Element.fromDom
        |> Input.cast
        |> Belt.Option.getExn
        |> Input.value;

    send(value);
};

let renderFilter = ({ state: filter, handle }) =>
    <input className=Styles.filter type_="text" value=filter
        onChange=(handle(filterChange)) />;

type filter =
    | And(filter, filter) | Or(filter, filter)
    | String(string) | Key(string, filter) | Null;

let parseFilter = (filter) =>
    Js.String.split(" ", filter)
    |> Js.Array.map(s =>
        !Js.String.includes(":", s) ?
            String(s)
        : {
            let pieces = Js.String.splitAtMost(":", ~limit=2, s);
            Key(pieces[0], String(pieces[1]));
        }
    )
    |> Js.Array.reduce((acc, filter) => Or(acc, filter), Null);

let getRowString = (row) =>
    Js.Json.stringify(row);

let rec filterRow = (filter, row) =>
    switch (filter) {
        | Null => false
        | And(f1, f2) => filterRow(f1, row) && filterRow(f2, row)
        | Or(f1, f2) => filterRow(f1, row) || filterRow(f2, row)

        | String(s) =>
            getRowString(row)
            |> Js.String.includes(s)

        | Key(key, filter) => {
            Js.Json.decodeObject(row)
            -> Belt.Option.map(Lodash.Get.get(_, key))
            -> Belt.Option.flatMap(Js.Nullable.toOption)
            -> Belt.Option.map(filterRow(filter))
            -> Belt.Option.getWithDefault(false)
        }
    };

let renderRows = ({ state: filter }) =>
    rows
    |> Js.Array.filter(filterRow(parseFilter(filter)))
    |> Js.Array.mapi((json, i) => <Row key=string_of_int(i) json />)
    |> array;

let render = (self) =>
    <div className=Styles.container>
        (renderFilter(self))
        (renderRows(self))
    </div>;

let reducer = (newFilter, _) => Update(newFilter);

let initialState = _ => "";

let component = ReasonReact.reducerComponent("App");
let make = (_) => { ...component, render, initialState, reducer };
