const fs = require('fs');
const chai = require('chai');

var assert = chai.assert;

var native_functions = {
    js: {
        print_int: function(_, i) { console.log(i) },
    }
};


function test(source, result) {
    return WebAssembly.instantiate(
        new Uint8Array(fs.readFileSync(source))
        // , native_functions
    ).then(o => {
        // console.log(
        //     o.instance.exports.start()
        // )
        assert.equal(
            o.instance.exports.start(),
            result
        );
    }).catch(e => {
      console.log(e);
    });
}


tests = {
    "test/adder.wasm": 1110,
    "test/cls-bug.wasm": 912,
    "test/funcomp.wasm": 247
}

for (var src in tests) {
    test(src, tests[src])
}
