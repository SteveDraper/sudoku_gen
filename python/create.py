from typing import Callable

import numpy as np
import sys
import copy
from functools import reduce
from configargparse import ArgumentParser
from random import randint

from stream_wrapper import *
from board import Board
from solver import State


def main():
    options = [
        argument(
            '--kernel_size',
            '-k',
            help        = 'Puzzle kernel size',
            type        = int,
            required    = True
        ),
        argument(
            '--clues',
            '-c',
            help        = 'Number of clues',
            type        = int,
            required    = False
        ),
        argument(
            '--out',
            '-o',
            help        = 'File to write position to (defaults to stdout)',
            required    = False
        )
    ]

    arg_parser = reduce(lambda p, a: a(p), options, ArgumentParser())
    arguments = vars(arg_parser.parse_args())

    with open_or(arguments['out'], "w", sys.stdout) as f:
        board = Board(arguments['kernel_size'])
        board.randomize(500)

        board = trim_board(board, arguments['clues'])

        print(board.render(), file=f)


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