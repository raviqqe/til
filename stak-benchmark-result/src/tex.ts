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
const benchmarkNames = ["fibonacci", "sum", "tak"];

const columnSeparator = " & ";

const fractionDigits = 2;
const numberFormat = new Intl.NumberFormat(undefined, {
  maximumFractionDigits: fractionDigits,
  minimumFractionDigits: fractionDigits,
});

const horizontalLine = () => console.log("\\hline");

const [, , directory] = process.argv;

if (!directory) {
  throw new Error("directory argument not defined");
}

const benchmarks = await readBenchmarks(directory, referenceCommand);

horizontalLine();
console.log(["Benchmark", ...commands].join(columnSeparator));
horizontalLine();

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

horizontalLine();
