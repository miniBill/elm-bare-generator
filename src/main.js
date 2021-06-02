const { Elm } = require('../build/elm.js');
const fs = require('fs')

if (process.argv.length != 5) {
    console.error("Usage: node main.js <input schema> <output file> <module name>");
    process.exit(1);
}

const inputSchema = process.argv[2];
const outputFile = process.argv[3];
const moduleName = process.argv[4];

const schema = fs.readFileSync(inputSchema, 'utf8');

const program = Elm.Main.init({
    flags: {
        moduleName: moduleName,
        outputFile: outputFile,
        schema: schema,
    }
});

program.ports.print.subscribe(message => {
    console.log(message);
    program.ports.printDone.send({});
});
program.ports.writeFile.subscribe(data => {
    fs.writeFileSync(outputFile, data);
    program.ports.writeFileDone.send({});
})
program.ports.exit.subscribe(result => {
    process.exit(result);
});
