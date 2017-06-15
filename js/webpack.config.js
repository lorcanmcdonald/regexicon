const clc = require("cli-color"),
  path = require("path"),
  webpack = require("webpack");

const config = {
  entry: {
    app: path.resolve(__dirname, "./client.js")
  },
  resolve: {
    alias: {
      "react": "preact-compat",
      "react-dom": "preact-compat"
    },
    modules: [
      __dirname,
      path.resolve(__dirname, "node_modules")
    ]
  },
  output: {
    filename: "bundle.js",
    path: path.resolve(__dirname, "dist")
  },
  devtool: "hidden-source-map",
  module: {
    rules: [
      { test: /\.js$/, exclude: /node_modules/, loader: "babel-loader" },
      { test: /\.jsx$/, exclude: /node_modules/, loader: "babel-loader" },
      { test: /\.less$/, use: [ "style-loader", "css-loader", "less-loader" ] },
      { test: /\.scss$/, use: [ "style-loader", "css-loader", "sass-loader" ] }
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
  ]
};

module.exports = config;

