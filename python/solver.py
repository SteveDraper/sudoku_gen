from typing import Set

from board import *


class State:
    def __init__(self, board: Board):
        def all_set(n: int) -> Set:
            return set(range(1, n+1))

        self.kernel_size = board.kernel_size
        self.board_size = board.board_size
        self.row_sets = [all_set(self.board_size) for _ in range(self.board_size)]
        self.col_sets = [all_set(self.board_size) for _ in range(self.board_size)]
        self.sqr_sets = [all_set(self.board_size) for _ in range(self.board_size)]
        self.cells = np.copy(board.cells)
        self.open = 0
        for row in range(self.board_size):
            for col in range(self.board_size):
                value = board.cells[row, col]
                if value == 0:
                    self.open += 1
                else:
                    self.row_sets[row].remove(value)
                    self.col_sets[col].remove(value)
                    self.sqr_sets[self._sq_idx(row, col)].remove(value)

    def _sq_idx(self, row: int, col: int) -> int:
        return int(row/self.kernel_size)*self.kernel_size + int(col/self.kernel_size)

    def to_board(self) -> Board:
        result = Board(self.kernel_size)
        result.cells = self.cells
        return result

    def solve(self, min_hint=0, reverse=False) -> bool:
        if self.open == 0:
            return True
        else:
            row, col, open = self._min_choice_cell(min_hint)
            self.open -= 1
            for value in reversed(list(open)) if reverse else open:
                self._set(row, col, value)
                if self.solve(min_hint=len(open)-1, reverse=reverse):
                    return True
                self._unset(row, col, value)
            self.open += 1
            return False

    def _set(self, row: int, col: int, value: int):
        self.cells[row, col] = value
        self.row_sets[row].remove(value)
        self.col_sets[col].remove(value)
        self.sqr_sets[self._sq_idx(row, col)].remove(value)

    def _unset(self, row: int, col: int, value: int):
        self.cells[row, col] = 0
        self.row_sets[row].add(value)
        self.col_sets[col].add(value)
        self.sqr_sets[self._sq_idx(row, col)].add(value)

    def _min_choice_cell(self, min_hint: int):
        min_found = self.board_size
        best_row = None
        best_col = None
        best_open_set = None
        for row in range(self.board_size):
            row_set = self.row_sets[row]
            for col in range(self.board_size):
                if self.cells[row, col] == 0:
                    col_set = self.col_sets[col]
                    sq_set = self.sqr_sets[self._sq_idx(row, col)]
                    open = row_set.intersection(col_set, sq_set)
                    if len(open) < min_found:
                        min_found = len(open)
                        best_open_set = open
                        best_row = row
                        best_col = col
                        if min_found <= min_hint:
                            return best_row, best_col, best_open_set
        return best_row, best_col, best_open_set
