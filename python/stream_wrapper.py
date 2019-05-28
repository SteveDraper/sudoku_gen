from typing import Optional

from io import TextIOWrapper
from contextlib import contextmanager


@contextmanager
def _wrapper(handle: TextIOWrapper) -> TextIOWrapper:
    yield handle


def open_or(filename: Optional[str], mode: str, default: TextIOWrapper):
    return open(filename, mode) if filename is not None else _wrapper(default)
