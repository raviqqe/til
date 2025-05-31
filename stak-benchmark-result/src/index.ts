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

const readResults = async (path: string) => {
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
      .map(async (path) => [
        path.replace(".json", ""),
        await readResults(join(directory, path)),
      ]),
  )
).toSorted();

console.log(
  joinBlocks([
    table(
      ["Benchmark", ...commands],
      results.map(([name, results]) => [
        name,
        ...commands.map((command) =>
          Object.hasOwn(results, command) ? results[command].toString() : "N/A",
        ),
      ]),
    ),
  ]),
);
