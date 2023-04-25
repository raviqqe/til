export default {
  mode: "production",
  entry: "./src/index.js",
  output: {
    library: {
      type: "module",
    },
  },
  experiments: {
    outputModule: true,
  },
};
