import { readBenchmarks } from "./benchmark.ts";

const referenceCommand = "mstak";
const commands = [
  referenceCommand,
  "stak",
  "gsi",
  "chibi-scheme",
  "gosh",
  "python3",
  "micropython",
  "ruby",
  "mruby",
  "lua",
];
const benchmarkNames = ["fibonacci", "sum", "stak"];

const columnSeparator = " & ";

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

console.log(["Benchmark", ...commands].join(columnSeparator));

for (const [name, results] of benchmarks) {
  if (!benchmarkNames.includes(name)) {
    continue;
  }

  console.log(
    [
      name,
      ...commands.map((command) =>
        results[command] === undefined
          ? "-"
          : numberFormat.format(results[command]),
      ),
    ].join(columnSeparator) + " \\\\",
  );
}
