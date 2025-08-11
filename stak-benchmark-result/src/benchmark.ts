import { readdir, readFile } from "node:fs/promises";
import { join } from "node:path";
import { mapValues } from "es-toolkit";
import { array, number, object, parse, string } from "valibot";

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
  reference: string,
): Promise<[string, Record<string, number>][]> => {
  return (
    await Promise.all(
      (
        await readdir(directory)
      )
        .filter((path) => path.endsWith(".json"))
        .map(
          async (path): Promise<[string, Record<string, number>]> => [
            path.replace(".json", ""),
            await readBenchmark(join(directory, path)),
          ],
        ),
    )
  )
    .map(([name, results]): [string, Record<string, number>] => {
      const referenceValue = results[reference];

      if (referenceValue === undefined) {
        throw new Error("reference not found");
      }

      return [name, mapValues(results, (value) => value / referenceValue)];
    })
    .toSorted();
};
