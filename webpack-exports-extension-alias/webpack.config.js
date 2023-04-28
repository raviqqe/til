export default {
  mode: "production",
  entry: "./src/main.js",
  resolve: {
    extensions: [".js"],
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
