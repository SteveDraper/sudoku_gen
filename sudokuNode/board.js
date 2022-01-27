
class Board {
  constructor(kernel_size, generate=true) {
    this.kernel_size = kernel_size;
    this.board_size = kernel_size*kernel_size
    this.cells = new Array(this.board_size).fill(0).map(_ => new Array(this.board_size).fill(0));
    if (generate) {
        this._make_seed();
    }
  }

  _make_seed() {
    const self = this;
    function seed_val(row, col) {
        return (self.kernel_size*(row % self.kernel_size)+col+Math.floor(row/self.kernel_size)) % self.board_size + 1;
    }

    this.cells.forEach((row, row_idx) => {
        row.forEach((_, col_idx) => row[col_idx] = seed_val(row_idx, col_idx))
    });
  }

  render(alphabet=null) {
    var lookup;

    if (alphabet === null) {
        lookup = [...'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789']
    }
    else {
        lookup = new Array(alphabet.length);
        Object.entries(alphabet).forEach(([k, v]) => lookup[v] = k);
    }

    function as_char(n) {
        if (n == 0) {
            return '.';
        }
        else {
            return lookup[n-1];
        }
    }

    function render_row(row) {
        return row.map(as_char).join(' ');
    }

    return [...Array(this.board_size)].map((_, i) => render_row(this.cells[i])).join('\n');
  }
}

module.exports = { Board }