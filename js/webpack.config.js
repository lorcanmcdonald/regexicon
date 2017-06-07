const clc = require("cli-color"),
  path = require("path"),
  webpack = require("webpack");

const argv = require("minimist")(process.argv.slice(2));
console.log("__dirname", __dirname);

let config = {
  entry: {
    app: path.resolve(__dirname, "./client.js")
  },
  resolve: {
    modules: [
      __dirname,
      path.resolve(__dirname, "node_modules")
    ]
  },
  output: {
    filename: "bundle.js",
    path: path.resolve(__dirname, "dist")
  },
  module: {
    rules: [
      { test: /\.less$/, use: [ "style-loader", "css-loader", "less-loader" ] },
      { test: /\.scss$/, use: [ "style-loader", "css-loader", "sass-loader" ] },
      { test: /\.js$/, exclude: /node_modules/, loader: "babel-loader" }
    ]
  },
  plugins: [
    new webpack.ContextReplacementPlugin(/moment[\/\\]locale$/, /en/),
    new webpack.DefinePlugin({
      "process.env.NODE_ENV": "\"production\""
    }),
    new webpack.DefinePlugin({
      "globalConfig.env": "\"production\""
    }),
    new webpack.optimize.UglifyJsPlugin({ compress: { warnings: false } })
    // new webpack.optimize.AggressiveMergingPlugin()
  ]
};

module.exports = config;

