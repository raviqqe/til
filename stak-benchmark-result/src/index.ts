import { readdir } from "fs/promises";

const [, , directory] = process.argv;

if (!directory) {
  throw new Error("directory argument not defined");
}

await readdir(directory);
