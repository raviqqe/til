import { mapValues } from "es-toolkit";
import { readdir, readFile } from "fs/promises";
import { join } from "path";
import { object, array, parse, number, string } from "valibot";

const benchmarkSchema = object({
  results: array(
    object({
      command: string(),
      median: number(),
    }),
  ),
});

const readBenchmark = async (path: string): Promise<Record<string, number>> => {
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

export const readBenchmarks = async (
  directory: string,
): Promise<[string, Record<string, number>][]> => {
  return (
    await Promise.all(
      (await readdir(directory))
        .filter((path) => path.endsWith(".json"))
        .map(
          async (path): Promise<[string, Record<string, number>]> => [
            path.replace(".json", ""),
            await readBenchmark(join(directory, path)),
          ],
        ),
    )
  )
    .map(([name, results]): [string, Record<string, number>] => [
      name,
      mapValues(results, (value) => value / (results[referenceCommand] ?? 0)),
    ])
    .toSorted();
};
