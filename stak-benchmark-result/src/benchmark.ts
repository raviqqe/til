import { readdir, readFile } from "node:fs/promises";
import { join } from "node:path";
import { mapValues } from "es-toolkit";
import {
  array,
  type InferOutput,
  number,
  object,
  parse,
  string,
} from "valibot";

const benchmarkSchema = object({
  results: array(
    object({
      command: string(),
      mean: number(),
      stddev: number(),
    }),
  ),
});

type Benchmark = InferOutput<typeof benchmarkSchema>;

type BenchmarkResult = Omit<Benchmark["results"][0], "command">;

const readBenchmark = async (
  path: string,
): Promise<Record<string, BenchmarkResult>> => {
  const data = parse(
    benchmarkSchema,
    JSON.parse(await readFile(path, "utf-8")),
  );

  return Object.fromEntries(
    data.results.map(({ command, ...rest }) => [
      command.split(" ")[0] ?? command,
      rest,
    ]),
  );
};

export const readBenchmarks = async (
  directory: string,
  reference: string,
): Promise<[string, Record<string, { mean: number }>][]> => {
  return (
    await Promise.all(
      (
        await readdir(directory)
      )
        .filter((path) => path.endsWith(".json"))
        .map(
          async (path): Promise<[string, Record<string, BenchmarkResult>]> => [
            path.replace(".json", ""),
            await readBenchmark(join(directory, path)),
          ],
        ),
    )
  )
    .map(([name, results]): [string, Record<string, BenchmarkResult>] => {
      const referenceResult = results[reference];

      if (referenceResult === undefined) {
        throw new Error("reference not found");
      }

      return [
        name,
        mapValues(results, (value) => ({
          mean: value.mean / referenceResult.mean,
          stddev: 0,
        })),
      ];
    })
    .toSorted();
};
