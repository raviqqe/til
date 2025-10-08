import { parseArgs } from "node:util";
import { table, tsMarkdown } from "ts-markdown";
import { readBenchmarks } from "./benchmark.ts";

const referenceCommand = "mstak";

const mixedCommands = [
  referenceCommand,
  "stak",
  "stak-interpret",
  "mstak-interpret",
  "gsi",
  "chibi-scheme",
  "gosh",
  "guile",
  "python3",
  "micropython",
  "ruby",
  "mruby",
  "lua",
];

const schemeCommands = [
  referenceCommand,
  "stak",
  "mstak-interpret",
  "stak-interpret",
  "tr7i",
  "gsi",
  "chibi-scheme",
  "gosh",
];

const fractionDigits = 2;
const numberFormat = new Intl.NumberFormat(undefined, {
  maximumFractionDigits: fractionDigits,
  minimumFractionDigits: fractionDigits,
});

const {
  positionals: [directory],
  values: { scheme },
} = parseArgs({
  allowPositionals: true,
  options: {
    scheme: { type: "boolean" },
  },
});

if (!directory) {
  throw new Error("directory argument not defined");
}

const commands = scheme ? schemeCommands : mixedCommands;
const benchmarks = await readBenchmarks(directory, referenceCommand);

console.log(
  tsMarkdown([
    table({
      columns: ["Benchmark", ...commands].map((name, index) => ({
        align: index === 0 ? undefined : "right",
        name,
      })),
      rows: benchmarks.map(([name, results]) => [
        name,
        ...commands.map((command) =>
          results[command] === undefined
            ? "-"
            : numberFormat.format(results[command].mean),
        ),
      ]),
    }),
  ]),
);
