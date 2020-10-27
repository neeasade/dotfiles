#!/usr/bin/env node
// debounce <ms>

var timeout = process.argv[2]

// npm i -g lodash split
lodash = require('lodash');
process.stdin.pipe(require('split')()).on('data', lodash.debounce(processLine, timeout))

function processLine (line) {
  console.log(line)
}
