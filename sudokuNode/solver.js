const { Board } = require('./board.js');

function _range(n, start=0) {
    return Array.from({length: n}).map((_, i) => i + start);
}

function _intersect(set1, set2) {
    if (set1.length > set2.length) {
        var set3 = set1;
        set1 = set2;
        set2 = set3;
    }
    var result = new Set();
    set1.forEach(v => {
        if (set2.has(v)) {
            result.add(v)
        }
    });
    return result;
}

class State {
    constructor(board) {
        function all_set(n) {
            return new Set(_range(n, 1));
        }

        this.kernel_size = board.kernel_size;
        this.board_size = board.board_size;
        this.row_sets = _range(this.board_size).map(_ => all_set(this.board_size));
        this.col_sets = _range(this.board_size).map(_ => all_set(this.board_size));
        this.sqr_sets = _range(this.board_size).map(_ => all_set(this.board_size));
        this.cells = _range(this.board_size).map((_, i) => board.cells[i].slice());
        this.open = 0;
        _range(this.board_size).map((_, row) => {
            _range(this.board_size).map((_, col) => {
                const value = board.cells[row][col];
                if (value == 0) {
                    this.open += 1;
                }
                else {
                    this.row_sets[row].delete(value);
                    this.col_sets[col].delete(value);
                    this.sqr_sets[this._sq_idx(row, col)].delete(value);
                }
            });
        });
    }

    _sq_idx(row, col) {
        return Math.floor(row/this.kernel_size)*this.kernel_size + Math.floor(col/this.kernel_size)
    }

    to_board() {
        result = new Board(this.kernel_size);
        result.cells = this.cells
        return result
    }

    solve(min_hint=0, reverse=false) {
        const self = this;

        if (this.open == 0) {
            return true;
        }
        else {
            var [row, col, open] = this._min_choice_cell(min_hint);
            this.open -= 1;

            function try_choice(choice) {
                self._set(row, col, choice);
                if (self.solve(min_hint=open.size-1, reverse=reverse)) {
                    return true;
                }
                else {
                    self._unset(row, col, choice);
                    return false;
                }
            }

            const open_ordered = reverse ? reversed(Array.from(open)) : Array.from(open);
            const solved = open_ordered.some(try_choice);
            if (solved) {
                return true;
            }
            else {
                this.open += 1;
                return false;
            }
        }
    }

    _set(row, col, value) {
        this.cells[row][col] = value;
        this.row_sets[row].delete(value);
        this.col_sets[col].delete(value);
        this.sqr_sets[this._sq_idx(row, col)].delete(value);
    }

    _unset(row, col, value) {
        this.cells[row][col] = 0;
        this.row_sets[row].add(value);
        this.col_sets[col].add(value);
        this.sqr_sets[this._sq_idx(row, col)].add(value);
    }

    _min_choice_cell(min_hint) {
        var min_found = this.board_size;
        var best_row = null;
        var best_col = null;
        var best_open_set = null;
        _range(this.board_size).map((_, row) => {
            var row_set = this.row_sets[row];
            _range(this.board_size).map((_, col) => {
                if (this.cells[row][col] == 0) {
                    var col_set = this.col_sets[col];
                    var sq_set = this.sqr_sets[this._sq_idx(row, col)];
                    var open = _intersect(row_set, _intersect(col_set, sq_set));
                    if (open.size < min_found) {
                        min_found = open.size;
                        best_open_set = open;
                        best_row = row;
                        best_col = col;
                        if (min_found <= min_hint) {
                            return [best_row, best_col, best_open_set]
                        }
                    }
                }
            });
        });

        return [best_row, best_col, best_open_set]
    }
}

module.exports = { State }