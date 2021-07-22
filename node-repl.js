import { SchemeJS } from 'SchemeJS.mjs';

let fs = require('fs');
let inputFd, closeFd, oldRawMode;
try {
  try {
  if (process.platform === 'win32') {
    inputFd = process.stdin.fd;
    if (process.stdin.setRawMode) {
      oldRawMode = process.stdin.isRaw;
      process.stdin.setRawMode(true);
    }
  } else {
    inputFd = closeFd = fs.openSync('/dev/tty', 'rs')
  }
  } catch(e) {
    console.info("Can't open termnal", e);
    SchemeJS({ unitTest: true });  // XXX silly debugging hack
  }
  if (inputFd !== undefined) {
  console.log(`SchemeJS 1.1 REPL. Type "." to exit.`);
  let buffer = Buffer.alloc(2000);
  function getLine(prompt) {
    process.stdout.write(prompt);
    let read = fs.readSync(inputFd, buffer);
    let line = buffer.slice(0, read).toString();
    while (line.endsWith('\n') || line.endsWith('\r'))
    line = line.substr(0, line.length-1);
    return line;
  }
  function readFile(path) {
    let fileContent = fs.readFileSync(path);
    fileContent = fileContent.toString();
    return fileContent;
  }
  // It isn't really a "constructor" but can be invoked with or without "new"
  let schemeJS = new SchemeJS( { readFile });
  // getLine("Attach debugger and hit return!");  // Uncomment to do what it says
  schemeJS.evalString('(define (test) (load "test.scm"))');
  schemeJS.REPL(getLine);
  }
} finally {
  if (closeFd !== undefined)
    fs.closeSync(closeFd);
  if (oldRawMode !== undefined)
    process.stdin.setRawMode(oldRawMode);
}
