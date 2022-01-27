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
    constructor(init_value=0) {
        this.value = init_value;
        this.size = _calc_size(init_value);
    }

    copy_from(other) {
        this.value = other.value;
        this.size = other.size;
    }

    add(n) {
        if (!this.has(n)) {
            this.value = this.value | _powers_of_2[n];
            this.size += 1;
        }
    }

    delete(n) {
        if (this.has(n)) {
            this.value = this.value & ~_powers_of_2[n];
            this.size -= 1;
        }
    }

    intersect(other) {
        return new BitSet32(this.value & other.value)
    }

    intersect_with(other) {
        this.value = this.value & other.value;
        this.size = _calc_size(this.value);
    }

    has(n) {
        return ((this.value & _powers_of_2[n]) != 0);
    }

    some(f) {
        for (const value of this) {
            if (f(value)) {
                return true;
            }
        }
        return false;
    }

    *[Symbol.iterator]() {
        var count = this.size;
        for (let i = 0; count > 0; i++) {
            if (this.has(i)) {
                yield i;
                count--;
            }
        }
    }
}

module.exports = { BitSet32 }