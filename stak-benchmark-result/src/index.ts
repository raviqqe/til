import { readdir, readFile } from "fs/promises";
import { object, array, parse, number } from "valibot";

const benchmarkSchema = object({
  results: array(object({ median: number() })),
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
    JSON.parse(await readFile(path, "utf-8")),
  );

  console.log(data);
}
