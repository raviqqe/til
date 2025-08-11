import { readBenchmarks } from "./benchmark.ts";

const referenceCommand = "mstak";
const commands = [
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
const benchmarkNames = ["fibonacci", "sum", "tak"];

const fractionDigits = 2;
const numberFormat = new Intl.NumberFormat(undefined, {
  maximumFractionDigits: fractionDigits,
  minimumFractionDigits: fractionDigits,
});

const printLine = () => console.log("\\hline");
const printRow = (row: string[]) => console.log(`${row.join(" & ")} \\\\`);

const [, , directory] = process.argv;

if (!directory) {
  throw new Error("directory argument not defined");
}

const benchmarks = await readBenchmarks(directory, referenceCommand);

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
        : numberFormat.format(results[command]),
    ),
  ]);
}

printLine();
