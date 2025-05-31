import { readdir, readFile } from "fs/promises";

const [, , directory] = process.argv;

if (!directory) {
  throw new Error("directory argument not defined");
}

for (const path of await readdir(directory)) {
  if (!path.endsWith(".json")) {
    continue;
  }

  const data = JSON.parse(await readFile(path, "utf-8"));

  console.log(data);
}
