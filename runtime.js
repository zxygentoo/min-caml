'use strict';

const fs = require('fs');


const core = {
    core: {
        print_int:      i => process.stdout.write(String(i)),
        print_newline:  _ => console.log(),
        abs_float:      a => Math.abs(a),
        sqrt:           a => Math.sqrt(a),
        cos:            a => Math.cos(a),
        sin:            a => Math.sin(a),
        float_of_int:   i => i,
        int_of_float:   a => a|0,
        truncate:       a => a|0
    }
};


const run = src => {
    return WebAssembly.instantiate(
        new Uint8Array(fs.readFileSync(src)),
        core
    ).then(mod => {
        mod.instance.exports.start();
    }).catch(e => {
        console.log(e);
    });
};


run(process.argv[2]);
