_powers_of_2 = new Array(32).fill(0).map((_, i) => 2 ** i);

function _calc_size(n) {
    var count = 0;
    while (n > 0)
    {
        n &= (n - 1);
        count++;
    }
    return count;
}

class BitSet32 {
    constructor(init_value=0, max_possible=31) {
        this.value = init_value;
        this.max_set = max_possible;
        this.size = _calc_size(init_value);
    }

    add(n) {
        if (!this.has(n)) {
            this.value = this.value | _powers_of_2[n];
            this.size += 1;
            if (n > this.max_set) {
                this.max_set = n;
            }
        }
    }

    delete(n) {
        if (this.has(n)) {
            this.value = this.value & ~_powers_of_2[n];
            this.size -= 1;
        }
    }

    intersect(other) {
        return new BitSet32(this.value & other.value, this.max_set)
    }

    has(n) {
        return ((this.value & _powers_of_2[n]) != 0);
    }

    *[Symbol.iterator]() {
        for (let i = 0; i <= this.max_set; i++) {
            if (this.has(i)) {
                yield i;
            }
        }
    }
}

module.exports = { BitSet32 }