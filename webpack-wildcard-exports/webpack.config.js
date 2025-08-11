export default {
  entry: "./src/index.js",
  experiments: {
    outputModule: true,
  },
  mode: "production",
  output: {
    library: {
      type: "module",
    },
  },
};
