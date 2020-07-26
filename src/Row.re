open React;

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
    let level = style([
        display(`inlineBlock),
        width(`px(75)),
    ]);
    let error = style([color(`hex("f00"))]);
    let warn = style([color(`hex("ff0"))]);
    let info = style([color(`hex("55F"))]);
    let debug = style([color(`hex("eee"))]);
    let verbose = style([color(`hex("f0f"))]);
    let silly = style([color(`hex("777"))]);
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

let getLevel = (json) =>
    switch (Js.Json.classify(json)) {
        | Js.Json.JSONObject(dict) =>
            Js.Dict.get(dict, "level")
            -> Belt.Option.getWithDefault(json)

        | _ => json
    }
    |> Js.String.make;

let renderLevel = (level) => {
    let className = switch level {
        | "error" => Styles.error
        | "warn" => Styles.warn
        | "info" => Styles.info
        | "debug" => Styles.debug
        | "verbose" => Styles.verbose
        | "silly" => Styles.silly
        | _ => failwith("Unrecognized level: " ++ level);
    };

    <span className=Css.(merge([className, Styles.level]))>
        (string(level))
    </span>
};

[@react.component]
let make = (~json) => {
    let (expanded, setExpanded) = useState(() => false);

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

    <div>
        <h2 className=Styles.header onClick=((_) => setExpanded(v => !v))>
            (renderLevel(getLevel(json)))
            (getMessage(json) |> string)
            (expanded ? null :
                <span className=Styles.subtitle>
                    (Js.Json.stringify(effectiveJson) |> string)
                </span>)
        </h2>
        (expanded ? renderJson(json) : null)
    </div>
};
