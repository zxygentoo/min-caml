'use strict';

const fs = require('fs');
const wabt = require("wabt")();


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


const run_wasm = src => {
    return WebAssembly.instantiate(
        new Uint8Array(fs.readFileSync(src)),
        core
    ).then(mod => {
        mod.instance.exports.start();
    }).catch(e => {
        console.log(e);
    });
};


const run_wat = src => {
    return WebAssembly.instantiate(
        wabt.parseWat(
            src,
            fs.readFileSync(src).toString()
        ).toBinary(
            { write_debug_names: true }
        ).buffer,
        core
    ).then(mod => {
        mod.instance.exports.start();
    }).catch(e => {
        console.log(e);
    });
};


run_wat(process.argv[2]);
