"use strict";

const fs = require("fs");
const wabt = require("wabt")();


const core = {
    core: {
        print_int:      i => process.stdout.write(String(i)),
        print_byte:     i => process.stdout.write(String.fromCharCode(i)),
        print_newline:  _ => console.log(),
        abs_float:      a => Math.abs(a),
        sqrt:           a => Math.sqrt(a),
        cos:            a => Math.cos(a),
        sin:            a => Math.sin(a),
        atan:           a => Math.atan(a),
        floor:          a => Math.floor(a),
        float_of_int:   i => i,
        int_of_float:   a => a|0,
        truncate:       a => a|0
    }
};


const run_wat = (filename, debug) => {
    let mod = wabt.parseWat(filename, fs.readFileSync(filename).toString());

    if (debug == true) {
        console.error(mod.toText({ foldExprs: false, inlineExport: false }));
    };

    return WebAssembly.instantiate(
        mod.toBinary({ write_debug_names: true }).buffer, core
    ).then().catch(e => {console.log(e); });
};


// run_wat(process.argv[2], true);
run_wat(process.argv[2], false);
