export default {
  mode: "production",
  entry: "./src/index.js",
  module: {
    rules: [
      {
        test: /\.ts$/,
        use: "ts-loader",
      },
    ],
  },
  resolve: {
    extensions: [".ts"],
    extensionAlias: {
      ".js": [".ts", ".js"],
    },
  },
  output: {
    library: {
      type: "module",
    },
  },
  experiments: {
    outputModule: true,
  },
};
