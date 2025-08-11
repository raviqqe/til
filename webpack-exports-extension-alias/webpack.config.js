export default {
  entry: "./src/main.js",
  experiments: {
    outputModule: true,
  },
  mode: "production",
  output: {
    library: {
      type: "module",
    },
  },
  resolve: {
    extensionAlias: {
      ".js": [".lisp", ".js"],
    },
    extensions: [".js"],
  },
};
