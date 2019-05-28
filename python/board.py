from typing import Optional

import numpy as np
from random import randint


class Board():
    def __init__(self, kernel_size: int, generate: bool=True):
        self.kernel_size = kernel_size
        self.board_size = kernel_size*kernel_size
        self.cells = np.zeros((self.board_size, self.board_size), dtype=int)
        if generate:
            self._make_seed()

    def render(self, alphabet: Optional[dict]=None) -> str:
        if alphabet is not None:
            lookup = [' ']*self.board_size
            for c, code in alphabet.items():
                lookup[code] = c
        else:
            lookup = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'

        def as_char(n: int) -> str:
            if n == 0:
                return '.'
            else:
                return lookup[n-1]

        def render_row(row: int) -> str:
            return " ".join([as_char(int(self.cells[row, col])) for col in range(self.board_size)])

        return "\n".join([render_row(row) for row in range(self.board_size)])

    def _make_seed(self):
        def seed_val(row: int, col: int) -> int:
            return (self.kernel_size*(row % self.kernel_size)+col+int(row/self.kernel_size)) % self.board_size + 1

        for row in range(self.board_size):
            row_cells = [seed_val(row, col) for col in range(self.board_size)]
            self.cells[row] = row_cells

    def randomize(self, num_permutes: int):
        def gen_sub_perm() -> np.array:
            super_idx = randint(0, self.kernel_size-1)
            perm = np.random.permutation(range(self.kernel_size)) + super_idx*self.kernel_size
            return np.concatenate([
                np.arange(super_idx*self.kernel_size, dtype=int),
                perm,
                np.arange((super_idx+1)*self.kernel_size, self.board_size, dtype=int)
            ])

        def gen_super_perm() -> np.array:
            perm = np.random.permutation(range(self.kernel_size))
            return np.concatenate([
                np.arange(s*self.kernel_size, (s+1)*self.kernel_size, dtype=int) for s in perm
            ])

        for _ in range(num_permutes):
            op_type = randint(0, 3)
            if op_type == 0:
                #   permute rows within a super-row
                perm = gen_sub_perm()
                self.cells = self.cells[perm, :]
            elif op_type == 1:
                #   permute columns within a super-column
                perm = gen_sub_perm()
                self.cells = self.cells[:, perm]
            elif op_type == 2:
                #   permute super-rows
                perm = gen_super_perm()
                self.cells = self.cells[perm, :]
            else:
                #   permute super-columns
                perm = gen_super_perm()
                self.cells = self.cells[:, perm]
        assert(self.valid())

    def valid(self) -> bool:
        s = sum(range(1, self.board_size+1))
        for idx in range(self.board_size):
            if int(np.sum(self.cells[idx, :])) != s:
                return False
            if int(np.sum(self.cells[:, idx])) != s:
                return False
            super_row = int(idx/self.kernel_size)
            super_col = idx % self.kernel_size
            if int(np.sum(self.cells[super_row*self.kernel_size:(super_row+1)*self.kernel_size,
                                     super_col*self.kernel_size:(super_col+1)*self.kernel_size])) != s:
                return False
        return True
