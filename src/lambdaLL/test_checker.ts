import * as fs from "fs";
import * as path from "path";
import { Lexer, Parser } from "./parser";
import { check_program } from "./typeChecker";
import { createContextTree } from "./contextTree";

const filePath = path.join(__dirname, "../../example/basic.ll");
const input = fs.readFileSync(filePath, "utf-8");

console.log("Reading file:", filePath);
// console.log("Input:", input);

try {
  const lexer = new Lexer(input);
  const tokens = lexer.tokenize();
  console.log("Tokenization successful. Token count:", tokens.length);
  // console.log(tokens);

  const parser = new Parser(tokens);
  const program = parser.parseProgram();
  console.log("Parsing successful. Statement/Alias count:", program.length);
  // console.log(JSON.stringify(program, null, 2));

  const tree = createContextTree("tensor");
  console.log("Context Tree initialized.");

  check_program(program, tree);
  console.log("Type checking completed successfully.");

  // Check invariants if needed?
  // normalize(tree); // check_program calls normalize at appropriate times (return)
} catch (e) {
  console.error("Error:", e);
  process.exit(1);
}
