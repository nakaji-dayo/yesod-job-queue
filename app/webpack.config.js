var path = require("path");
module.exports = {
  entry: "./app.js",
  output: {
    path: path.resolve(__dirname, "dist"),
    filename: "app.bundle.js"
  },
  module: {
    rules: [
      {
        test: /\.jsx?$/,
        exclude: /(node_modules)/,
        use: [
          {
            loader: 'babel-loader',
            options: {
              presets: [ '@babel/preset-env' ],
            }
          }
        ]
      }
    ]
  },
  resolve: {
    alias: {
      vex: 'vex-js'
    }
  },
  devtool: 'inline-source-map'
};
