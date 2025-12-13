export class CompilerError extends Error {
  line: number;
  column: number;
  length: number;

  constructor(
    message: string,
    line: number,
    column: number,
    length: number = 1
  ) {
    super(message);
    this.name = "CompilerError";
    this.line = line;
    this.column = column;
    this.length = length;
  }
}
