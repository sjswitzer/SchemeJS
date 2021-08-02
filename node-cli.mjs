//
// SchemeJS Node.js REPL
//
// Copyright 2021 Stan Switzer
//   This work is licensed under a Creative Commons Attribution-ShareAlike
//   4.0 International License. https://creativecommons.org/licenses/by-sa/4.0/
//

//
// node [nodeopts] node-cli.mjs [--load loadfile.scm]* [file.scm]
//   Loads and executes loadfile.scm....
//   If file.scm is not provided, it runs the REPL,
//   otherwise, loads and executes file.scm.

import * as SchemeJS from './SchemeJS.mjs';
import * as fs from 'fs';

let loadFiles = [], runREPL = true;

let argv = process.argv.slice(2);
while (argv.length > 0) {
  if (argv[0] === '--load') {
    loadFiles.push(argv[1]);
    argv = argv.slice(2);
    continue;
  }
  if (argv[0]) {
    runREPL = false;
    loadFiles.push(argv[0]);
    argv = argv.slice(1);
    break;
  }
  break;
}

// Run the REPL if no files specified or --repl arg is present
if (loadFiles.length === 0)
  runREPL = true;

let inputFd, closeFd, oldRawMode;
try {
  if (process.platform === 'win32') {
    inputFd = process.stdin.fd;
    if (process.stdin.setRawMode) {
      oldRawMode = process.stdin.isRaw;
      process.stdin.setRawMode(true);
    }
  } else {
    try {
      inputFd = closeFd = fs.openSync('/dev/tty', 'rs');
    } catch(e) {
      console.info("Can't open termnal", e);
    }
  }
  if (inputFd !== undefined) {
    console.log(`SchemeJS ${SchemeJS.VERSION} REPL. Type "." to exit.`);

    // For the REPL
    let buffer = Buffer.alloc(2000);
    function getLine(prompt) {
      process.stdout.write(prompt);
      let read = fs.readSync(inputFd, buffer);
      let line = buffer.slice(0, read).toString();
      while (line.endsWith('\n') || line.endsWith('\r'))
      line = line.substr(0, line.length-1);
      return line;
    }
    // getLine("Attach debugger and hit return! ");  // Uncomment to do what it says

    // For "load" and "require"
    function readFile(path) {
      let fileContent = fs.readFileSync(path);
      fileContent = fileContent.toString();
      return fileContent;
    }

    let endTest = (line => line === ".");  // end on a "."
    let globalScope = SchemeJS.createInstance({ readFile, endTest });
    globalScope[globalScope.Atom('*argv*')] = globalScope.to_list(argv);
    globalScope[globalScope.Atom('*env*')] = process.env;
    for (let file of loadFiles)
      globalScope.load(file);

    if (runREPL) {
      let assignSyntax = true;
      globalScope.REPL(getLine, { assignSyntax });
    }
  }
} finally {
  if (oldRawMode !== undefined)
    process.stdin.setRawMode(oldRawMode);
  if (closeFd !== undefined)
    fs.closeSync(closeFd);
}