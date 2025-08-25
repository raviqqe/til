import { parseArgs } from "node:util";
import { readBenchmarks } from "./benchmark.ts";

const referenceCommand = "mstak";

const mixedCommands = [
  referenceCommand,
  "stak",
  "tr7i",
  "gsi",
  "chibi-scheme",
  "python3",
  "micropython",
  "ruby",
  "mruby",
];

const schemeCommands = [
  referenceCommand,
  "stak",
  "mstak-interpret",
  "stak-interpret",
  "gsi",
  "chibi-scheme",
  "gosh",
];

const benchmarkNames = ["empty", "fibonacci", "hello", "sum", "tak"];

const fractionDigits = 2;
const numberFormat = new Intl.NumberFormat(undefined, {
  maximumFractionDigits: fractionDigits,
  minimumFractionDigits: fractionDigits,
});

const printLine = () => console.log("\\hline");
const printRow = (row: string[]) => console.log(`${row.join(" & ")} \\\\`);

const {
  positionals: [directory],
  values: { scheme },
} = parseArgs({
  allowPositionals: true,
  options: {
    scheme: {
      type: "boolean",
    },
  },
});

if (!directory) {
  throw new Error("directory argument not defined");
}

const benchmarks = await readBenchmarks(directory, referenceCommand);

const commands = scheme ? schemeCommands : mixedCommands;

printLine();
printRow(["Benchmark", ...commands]);
printLine();

for (const [name, results] of benchmarks) {
  if (!benchmarkNames.includes(name)) {
    continue;
  }

  printRow([
    name,
    ...commands.map((command) =>
      results[command] === undefined
        ? "-"
        : numberFormat.format(results[command].mean),
    ),
  ]);
}

printLine();
