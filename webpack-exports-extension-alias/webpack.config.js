export default {
  mode: "production",
  entry: "./src/main.js",
  resolve: {
    extensions: [".js"],
    extensionAlias: {
      ".js": [".tomato", ".js"],
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
