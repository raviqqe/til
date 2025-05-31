import { readdir, readFile } from "fs/promises";
import { join } from "path";
import { object, array, parse, number, string } from "valibot";
import { joinBlocks, table } from "ts-markdown-builder";

const commands = [
  "stak",
  "mstak",
  "stak-interpret",
  "mstak-interpret",
  "chibi-scheme",
  "gosh",
  "guile",
  "gsi",
  "python3",
  "micropython",
  "ruby",
  "mruby",
  "lua",
];

const benchmarkSchema = object({
  results: array(
    object({
      command: string(),
      median: number(),
    }),
  ),
});

const [, , directory] = process.argv;

if (!directory) {
  throw new Error("directory argument not defined");
}

const readResults = (path: string) => {
  const data = parse(
    benchmarkSchema,
    JSON.parse(await readFile(join(directory, path), "utf-8")),
  );

  return Object.fromEntries(
    data.results.map(({ command, median }) => [
      command.split(" ")[0] ?? command,
      median,
    ]),
  );
};

for (const path of await readdir(directory)) {
  if (!path.endsWith(".json")) {
    continue;
  }

  const results = results;
}

console.log(
  joinBlocks([
    table(
      ["Benchmark", ...results.map(({ command }) => command)],
      [results.map(({ median }) => median.toString())],
    ),
  ]),
);
