import { readdir, readFile } from "fs/promises";
import { join } from "path";
import { object, array, parse, number, string } from "valibot";
import { joinBlocks, table } from "ts-markdown-builder";

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

for (const path of await readdir(directory)) {
  if (!path.endsWith(".json")) {
    continue;
  }

  const data = parse(
    benchmarkSchema,
    JSON.parse(await readFile(join(directory, path), "utf-8")),
  );

  console.log(joinBlocks());
}
