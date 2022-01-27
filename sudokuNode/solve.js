const { Board } = require('./board.js');
const { State } = require('./solver.js');

const { program } = require('commander');
const fs = require('fs');
const { performance } = require('perf_hooks');

program
  .requiredOption('-f, --file <type>', 'File containing position to solve');

program.parse(process.argv);

const options = program.opts();

const boardDef = fs.readFileSync(options['file'], 'utf-8');
boardLines = boardDef.split(/\r?\n/).filter(line => line.length > 0).map(v => v.split(' ').join(''));
board_size = boardLines.length;
if (boardLines.some(l => l.length != board_size)) {
    throw "non-square board provided";
}

function kernel_size(n) {
    switch(n) {
        case 4:
            result = 2;
            break;
        case 9:
            result = 3;
            break;
        case 16:
            result = 4;
            break;
        case 25:
            result = 5;
            break;
         default:
            throw `Unsupported board size ${n}`
    }
    return result;
}

var alphabet = {};
var board = new Board(kernel_size(board_size), false);

boardLines.forEach((colVals, row) => {
    [...colVals].forEach((val, col) => {
        if (val != '.') {
            var code = alphabet[val];
            if (code === undefined) {
                code = Object.keys(alphabet).length;
                alphabet[val] = code;
            }
            board.cells[row][col] = code + 1
        }
    });
});

start = performance.now();
state = new State(board);
solved = state.solve();

duration = performance.now() - start;
if (solved) {
    console.log("Solved in %i milliseconds", duration);
    console.log(state.to_board().render(alphabet));
}
else {
    console.log("No solution found (in %i milliseconds) for board:", duration);
    console.log(board.render(alphabet));
}