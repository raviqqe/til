import { table, tsMarkdown } from "ts-markdown";
import { readBenchmarks } from "./benchmark.ts";

const referenceCommand = "mstak-interpret";
const commands = [
  referenceCommand,
  "stak-interpret",
  "mstak",
  "stak",
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

const fractionDigits = 2;
const numberFormat = new Intl.NumberFormat(undefined, {
  maximumFractionDigits: fractionDigits,
  minimumFractionDigits: fractionDigits,
});

const [, , directory] = process.argv;

if (!directory) {
  throw new Error("directory argument not defined");
}

const benchmarks = await readBenchmarks(directory, referenceCommand);

console.log(
  tsMarkdown([
    table({
      columns: ["Benchmark", ...commands],
      rows: benchmarks.map(([name, results]) => [
        name,
        ...commands.map((command) =>
          results[command] === undefined
            ? "-"
            : numberFormat.format(results[command]),
        ),
      ]),
    }),
  ]),
);
