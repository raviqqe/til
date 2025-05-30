import { readdir, readFile } from "fs/promises";
import { join } from "path";
import { object, array, parse, number, string } from "valibot";
import { table, tsMarkdown } from "ts-markdown";
import { mapValues } from "es-toolkit";

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

const benchmarkSchema = object({
  results: array(
    object({
      command: string(),
      median: number(),
    }),
  ),
});

const readResults = async (path: string): Promise<Record<string, number>> => {
  const data = parse(
    benchmarkSchema,
    JSON.parse(await readFile(path, "utf-8")),
  );

  return Object.fromEntries(
    data.results.map(({ command, median }) => [
      command.split(" ")[0] ?? command,
      median,
    ]),
  );
};

const [, , directory] = process.argv;

if (!directory) {
  throw new Error("directory argument not defined");
}

const results = (
  await Promise.all(
    (await readdir(directory))
      .filter((path) => path.endsWith(".json"))
      .map(
        async (path): Promise<[string, Record<string, number>]> => [
          path.replace(".json", ""),
          await readResults(join(directory, path)),
        ],
      ),
  )
)
  .map(([name, results]): [string, Record<string, number>] => [
    name,
    mapValues(results, (value) => value / (results[referenceCommand] ?? 0)),
  ])
  .toSorted();

console.log(
  tsMarkdown([
    table({
      columns: ["Benchmark", ...commands],
      rows: results.map(([name, results]) => [
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
