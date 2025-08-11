import { readBenchmarks } from "./benchmark.ts";

const referenceCommand = "mstak-interpret";
const _commands = [
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
const _numberFormat = new Intl.NumberFormat(undefined, {
  maximumFractionDigits: fractionDigits,
  minimumFractionDigits: fractionDigits,
});

const [, , directory] = process.argv;

if (!directory) {
  throw new Error("directory argument not defined");
}

const _benchmarks = await readBenchmarks(directory, referenceCommand);
