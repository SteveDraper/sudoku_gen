from typing import Callable

import numpy as np
import copy
from functools import reduce
from configargparse import ArgumentParser
from random import randint
from datetime import datetime, timedelta

from board import Board
from solver import State


def main():
    options = [
        argument(
            '--file',
            '-f',
            help        = 'File containing position to solve)',
            required    = True
        )
    ]

    arg_parser = reduce(lambda p, a: a(p), options, ArgumentParser())
    arguments = vars(arg_parser.parse_args())

    with open(arguments['file'], "r") as f:
        def kernel_size(n: int) -> int:
            if n == 4:
                return 2
            elif n == 9:
                return 3
            elif n == 16:
                return 4
            elif n == 25:
                return 5
            else:
                assert False, "Unsupported board size {}".format(n)

        lines = [''.join(filter(lambda c: (c != ' ') and (c != '\n'), l)) for l in f.readlines() if len(l) > 1]
        board_size = len(lines)
        assert all([len(line) == board_size for line in lines]), "non-square board provided"

        alphabet = {}
        board = Board(kernel_size(board_size), generate=False)
        for row in range(board_size):
            for col in range(board_size):
                c = lines[row][col]
                if c != '.':
                    if c in alphabet:
                        code = alphabet[c]
                    else:
                        code = len(alphabet)
                        alphabet[c] = code
                    board.cells[row, col] = code+1

        start = datetime.now()
        state = State(board)
        solved = state.solve()
        duration = datetime.now() - start
        if solved:
            print("Solved in {} milliseconds".format(duration.total_seconds()*1000))
            print(state.to_board().render(alphabet))
        else:
            print("No solution found (in {} milliseconds)".format(duration.total_seconds()*1000))


def trim_board(board: Board, num_clues: int) -> Board:
    def remove_clue(b: Board) -> Board:
        while True:
            row = randint(0, b.board_size-1)
            col = randint(0, b.board_size-1)
            if b.cells[row, col] != 0:
                break
        result = copy.deepcopy(b)
        result.cells[row, col] = 0
        return result

    open = 0
    while open < board.board_size*board.board_size - num_clues:
        retries = 5
        while retries > 0:
            retries -= 1
            new_board = remove_clue(board)
            state = State(new_board)
            if not state.solve():
                continue
            else:
                state2 = State(new_board)
                if not state2.solve(reverse=True):
                    assert False, "Solver error"
                if not np.array_equal(state2.cells, state.cells):
                    continue
                board = new_board
                open += 1
    return board



Argument = Callable[[ArgumentParser], ArgumentParser]


def argument(*args, **kwargs) -> Argument:
    """Constructor for a single command-line argument.

    This function delegates to
    :func:`configargparse.ArgumentParser.add_argument`
    (see `documentation <https://github.com/bw2/ConfigArgParse/blob/2c743df7ae708a7ae1f282f9e68c15621c53b75c/configargparse.py#L847>`_).
    """
    def _add(p: ArgumentParser) -> ArgumentParser:
        p.add_argument(*args, **kwargs)
        return p
    return _add


if __name__ == '__main__':
    main()