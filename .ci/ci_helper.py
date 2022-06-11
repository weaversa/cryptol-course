from contextlib import contextmanager
from dataclasses import dataclass
from random import getrandbits
import sys
from typing import Any, Literal, Optional, TypeAlias, Union

from cryptol.bitvector import BV
from cryptol.connection import CryptolConnection, connect
from cryptol.solver import Solver, W4_YICES, Z3

CommandArgs: TypeAlias = Union[str, tuple[Any], dict[str, Any]]

@dataclass
class ArgsAndKeywords:
    args: Optional[tuple[Any]] = None
    kwargs: Optional[dict[str, Any]] = None


def exit_on_failure(c: CryptolConnection, command: str, *args, **kwargs) -> None:
    """Run command ``command`` with positional arguments ``args`` and keyword arguments ``kwargs`` on Cryptol connection ``c``; exit on failure"""
    args_str = ','.join(map(repr,args))
    kwargs_str = ','.join(f'{k}={repr(v)}' for (k, v) in kwargs.items())
    command_args = f"{command}({','.join(s for s in [args_str, kwargs_str] if s)})"

    print(f"  Running command `{command_args}`...")
    if not hasattr(c, command):
        print(f"  ERROR: Unknown command `{command}`")
        sys.exit(1)

    result = getattr(c, command)(*args, **kwargs).result()
    if not result:
        print(f"   ERROR: Command `{command_args}` returns a False(y) result: {result}")
        sys.exit(1)

def check(c: CryptolConnection, expr: Any, *, num_tests: Union[Literal['all'], int, None] = None, timeout: Optional[float] = None) -> None:
    """On Cryptol Connection ``c``, check ``expr`` on ``num_tests`` tests; exit on failure or after ``timeout``"""
    exit_on_failure(c, 'check', expr, num_tests=num_tests, timeout=timeout)

def prove(c: CryptolConnection, expr: Any, solver: Solver = Z3, *, timeout: Optional[float] = None) -> None:
    """On Cryptol Connection ``c``, prove ``expr`` using ``solver``; exit on failure or after ``timeout``"""
    exit_on_failure(c, 'prove', expr, solver=solver, timeout=timeout)

def safe(c: CryptolConnection, expr: Any, solver: Solver = W4_YICES, *, timeout: Optional[float] = None) -> None:
    """On Cryptol Connection ``c``, check type safety of ``expr`` using ``solver``; exit on failure or after ``timeout``"""
    exit_on_failure(c, 'safe', expr, solver=solver, timeout=timeout)


def rand_bv(bitwidth: int) -> BV:
    """Return a random Cryptol bitvector of specified ``bitwidth``"""
    return BV(bitwidth, getrandbits(bitwidth))

def rand_list(n: int, bitwidth: int) -> list[BV]:
    """Return a list of ``n`` random bitvectors of specified ``bitwidth``"""
    return [
        rand_bv(bitwidth)
        for _ in range(n)
    ]

def test_cryptol(module_commands: dict[str, dict[str, list[CommandArgs]]], command_filter: list[str] = None) -> None:
    c = connect(reset_server=True)

    for (module_id, command_akwses) in module_commands.items():
        commands = command_akwses.keys()

        if command_filter:
            commands &= command_filter

        if commands:
            print(f"Testing {module_id}...")

            c.load_module(module_id)

            for cmd in commands:
                akwses = command_akwses[cmd]

                for akws in akwses:
                    if isinstance(akws, ArgsAndKeywords):
                        (optargs, optkws) = args.astuple()
                        args = optargs or ()
                        kws = optkws or {}
                    elif isinstance(akws, str):
                        (args, kws) = ((akws,), {})
                    elif isinstance(akws, tuple):
                        (args, kws) = (akws, {})
                    elif isinstance(akws, dict):
                        (args, kws) = ((), akws)
                    else:
                        print(f"  ERROR: Malformed args/keywords entry: {akws}")
                        sys.exit(1)
                        
                    exit_on_failure(c, cmd, *args, **kws)
        else:
            print(f"Skipping {module_id}; no commands to run")
