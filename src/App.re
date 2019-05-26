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
    let filterRow = style([
        display(`flex),
    ]);
    let textarea = style([
        width(`percent(100.)),
        borderWidth(`zero),
        boxSizing(`borderBox),
    ]);
};

type state = {
    filter: string,
    editingRows: bool,
    rows: array(Js.Json.t),
    textarea: ref(option(Dom.element)),
};
type action =
    | SetFilter(string) | Edit | SetRows(array(Js.Json.t));

let rowsFromString = (str) =>
    str
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

    send(SetFilter(value));
};

let renderFilter = ({ state: { filter }, handle }) =>
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

let renderRows = ({ state: { filter, rows } }) =>
    rows
    |> Js.Array.filter(filterRow(parseFilter(filter)))
    |> Js.Array.mapi((json, i) => <Row key=string_of_int(i) json />)
    |> array;

let editRows = (_, { send }) => send(Edit);

let saveJson = (_, { send, state: { textarea }}) => ReDom.(
    Belt.Option.getExn(textarea^)
    |> Element.fromDom
    |> Textarea.cast
    |> Belt.Option.getExn
    |> Textarea.value
    |> rowsFromString
    |> (v => SetRows(v))
    |> send
);

let renderJsonEditButton = ({ state: { editingRows }, handle }) =>
    editingRows ?
        <button type_="button" onClick=handle(saveJson)>
            (string("Save"))
        </button>
    :
        <button type_="button" onClick=handle(editRows)>
            (string("Edit"))
        </button>;

let setTextarea = (el, { state: { textarea } }) =>
    textarea := Js.Nullable.toOption(el);

let renderJsonEditor = ({ state: { editingRows }, handle }) =>
    editingRows ?
        <textarea rows=10 className=Styles.textarea ref=handle(setTextarea) />
    : null;

let render = (self) =>
    <div className=Styles.container>
        <div className=Styles.filterRow>
            (renderFilter(self))
            (renderJsonEditButton(self))
        </div>
        (renderJsonEditor(self))
        (renderRows(self))
    </div>;

let reducer = (action, state) =>
    switch action {
        | SetFilter(filter) => Update({ ...state, filter })
        | Edit => Update({ ...state, editingRows: true })
        | SetRows(rows) => Update({ ...state, rows, editingRows: false })
    };

let initialState = _ =>
    { filter: "", rows: [||], editingRows: true, textarea: ref(None) };

let component = ReasonReact.reducerComponent("App");
let make = (_) => { ...component, render, initialState, reducer };
