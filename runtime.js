'use strict';

const fs = require('fs');


var coreFunctions = {
    core: {
        print_int: i => process.stdout.write(String(i)), // no newline
        print_newline: () => console.log(),
        abs_float: a => Math.abs(a),
        sqrt: a => Math.sqrt(a),
        cos: a => Math.cos(a),
        sin: a => Math.sin(a),
        float_of_int: i => i,
        int_of_float: a => a|0,
    }
};


function run(source) {
    return WebAssembly.instantiate(
        new Uint8Array(fs.readFileSync(source)),
        coreFunctions
    ).then(o => {
        o.instance.exports.start()
    }).catch(e => {
      console.log(e);
    });
};


run(process.argv[2]);
