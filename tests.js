const fs = require('fs');
const chai = require('chai');

var assert = chai.assert;

function test(source, result) {
    return WebAssembly.instantiate(
        new Uint8Array(fs.readFileSync(source))
    ).then(o => {
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
