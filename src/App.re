open React;

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
    editingRows: bool,
    rows: array(Js.Json.t),
};
type action = Edit | SetRows(array(Js.Json.t));

let rowsFromString = (str) =>
    str
    |> Js.String.split("\n")
    |> Js.Array.map(Js.String.trim)
    |> Js.Array.filter(s => Js.String.length(s) !== 0)
    |> Js.Array.map(Js.Json.parseExn);

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

let reducer = (state, action) =>
    switch action {
        | Edit => { ...state, editingRows: true }
        | SetRows(rows) => { rows, editingRows: false }
    };

[@react.component]
let make = (_) =>{
    let (filter, setFilter) = useState(() => "");
    let ({rows, editingRows}, dispatch) = useReducer(reducer, { rows: [||], editingRows: true });
    let textareaRef = useRef(Js.Nullable.null);

    let handleFilterChange = useCallback0(
        (event) => {
            open ReDom;

            let value = ReactEvent.Form.currentTarget(event)
                |> Obj.magic
                |> Element.fromDom
                |> Input.cast
                |> Belt.Option.getExn
                |> Input.value;

            setFilter(_ => value);
        }
    );

    let renderRows = useCallback4(
        () =>
            rows
            |> Js.Array.filter(filterRow(parseFilter(filter)))
            |> Js.Array.mapi((json, i) => <Row key=string_of_int(i) json />)
            |> array,
        (rows, filter, filterRow, parseFilter)
    );

    let saveJson = useCallback0(
        (_) => ReDom.(
            React.Ref.current(textareaRef)
            |> Js.Nullable.toOption
            |> Belt.Option.getExn
            |> Element.fromDom
            |> Textarea.cast
            |> Belt.Option.getExn
            |> Textarea.value
            |> rowsFromString
            |> (v => SetRows(v))
            |> dispatch
        )
    );

    let renderJsonEditButton = useCallback2(
        () =>
            editingRows ?
                <button type_="button" onClick=saveJson>
                    (string("Save"))
                </button>
            :
                <button type_="button" onClick={_ => dispatch(Edit)}>
                    (string("Edit"))
                </button>,
        (editingRows, saveJson)
    );

    <div className=Styles.container>
        <div className=Styles.filterRow>
            <input
                className=Styles.filter type_="text" value=filter
                onChange=(handleFilterChange)
            />
            (renderJsonEditButton())
        </div>

        (!editingRows ? null :
            <textarea rows=10 className=Styles.textarea ref={ReactDOMRe.Ref.domRef(textareaRef)} />)

        (renderRows())
    </div>;
};
