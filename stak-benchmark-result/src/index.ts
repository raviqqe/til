import { readdir } from "fs/promises";

const [, , directory] = process.argv;

await readDirectory(directory);
