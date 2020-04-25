#!/usr/bin/env node

// npm i -g lodash
// npm i -g split
lodash = require('lodash');
process.stdin.pipe(require('split')()).on('data',
					  lodash.debounce(processLine, 100) // ms
					  // processLine
					 )

function processLine (line) {
  console.log(line)
}
