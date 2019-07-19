'use strict';

const fs = require('fs');

var nativeFunctions = {
    js: {
        print_int: i => process.stdout.write(String(i)) // no newline
    }
};

function run(source) {
    return WebAssembly.instantiate(
        new Uint8Array(fs.readFileSync(source)),
        nativeFunctions
    ).then(o => {
        o.instance.exports.start()
    }).catch(e => {
      console.log(e);
    });
}

run(process.argv[2]);
