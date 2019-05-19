open ReasonReact;

module Styles = {
    open Css;
    let objBody = style([ paddingLeft(`px(16)) ]);
    let header = style([
        fontSize(`px(16)),
        cursor(`pointer),
        textOverflow(`ellipsis),
        whiteSpace(`nowrap),
        overflow(`hidden),
        userSelect(`none),
    ]);
    let subtitle = style([
        fontWeight(`normal),
        fontSize(`em(0.8)),
        opacity(0.8),
        marginLeft(`px(4)),
    ]);
};

type state = bool;
type action = bool;

let getKeyPrefix = (key) => key
    -> Belt.Option.map(key => key ++ ": ")
    -> Belt.Option.getWithDefault("");

let rec renderEntry = ((key, value)) =>
    <div key>
        (renderJson(~key, value))
    </div>

and renderKeys = (dict) =>
    <div>
        (Js.Dict.entries(dict)
        |> Js.Array.map(renderEntry)
        |> array)
    </div>

and renderObject = (~key=?, dict) =>
    <div>
        <div>(string(getKeyPrefix(key) ++ "{"))</div>
        <div className=Styles.objBody>
            (renderKeys(dict))
        </div>
        <div>(string("}"))</div>
    </div>

and renderJson = (~key=?, json) =>
    switch (Js.Json.classify(json)) {
        | Js.Json.JSONObject(dict) => renderObject(~key?, dict)
        | _ => (getKeyPrefix(key) ++ Js.Json.stringify(json)) |> string
    };

let getMessage = (json) =>
    switch (Js.Json.classify(json)) {
        | Js.Json.JSONObject(dict) =>
            Js.Dict.get(dict, "message")
            -> Belt.Option.getWithDefault(json)

        | _ => json
    }
    |> Js.String.make;

let component = reducerComponent("Row");
let make = (~json, _) => {
    let effectiveJson = {
        let dict = Js.Json.stringify(json)
            |> Js.Json.parseExn
            |> Js.Json.decodeObject;

        Belt.Option.map(dict, dict => {
            Js.Promise.resolve(dict);
            [%bs.raw {| delete dict.message |}];
        });

        dict
        -> Belt.Option.map(Js.Json.object_)
        -> Belt.Option.getWithDefault(json);
    };

    let subtitle =
        <span className=Styles.subtitle>
            (Js.Json.stringify(effectiveJson) |> string)
        </span>;

    let clicked = (_, { state: expanded, send }) =>
        send(!expanded);

    let header = ({ state: expanded, handle }) =>
        <h2 className=Styles.header onClick=(handle(clicked))>
            (getMessage(json) |> string)
            (!expanded ? subtitle : null)
        </h2>;

    let render = ({ state: expanded } as self) =>
        <div>
            (header(self))
            (expanded ? renderJson(json) : null)
        </div>;

    let initialState = _ => false;

    let reducer = (action, _: state) => Update(action);

    { ...component, render, initialState, reducer };
};