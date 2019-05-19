const path = require("path");

module.exports = {
    mode: "development",
    devtool: "source-map",

    entry:  {
        index: rel("lib/js/src/index.js"),
    },

    output: {
        path: rel("html/js"),
        filename: "[name].js",
    },
};

function rel(relPath) {
    return path.resolve(__dirname, relPath);
}
