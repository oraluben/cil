import logging
from argparse import ArgumentParser
from json import dumps

from pathlib import Path
from re import match
from subprocess import run, PIPE
from sys import version_info
from tempfile import NamedTemporaryFile
from typing import List

assert (version_info >= (3, 5)), 'only support python >= 3.5'

ext_files = Path(__file__).parent.absolute()
stub_c = Path(ext_files) / 'stub.c'
api_c = Path(ext_files) / 'smartunit_api_v1.c'

assert ext_files.exists()
assert stub_c.exists()
assert api_c.exists()

ext_files, stub_c, api_c = map(str, [ext_files, stub_c, api_c])

_cil_help = run(['cilly.native', '--help'], stdout=PIPE).stdout.decode()

assert '--doaf_branch' in _cil_help
assert '--doaf' in _cil_help

logger = logging.getLogger('su')
logger.setLevel(logging.INFO)
ch = logging.StreamHandler()
ch.setLevel(logging.DEBUG)
ch.setFormatter(logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s'))
logger.addHandler(ch)


class P:
    path: Path

    def __init__(self, path):
        self.path = path if isinstance(path, Path) else Path(path)

    def __str__(self):
        return self.path.name

    @property
    def name(self):
        return self.__str__()


class Function(P):
    file_c: str
    _count: int = None
    _cil_c: str = None
    _cil_o: str = None

    def __init__(self, path):
        super().__init__(path)
        self.file_c = (self.path / 'run.c').resolve().absolute()

    @property
    def run_cil_c(self):
        if self._cil_c is None:
            _f = NamedTemporaryFile(delete=False, suffix='.cil.c')
            self._cil_c = _f.name
            _f.close()
            _cil_cmd = [
                'cilly.native',
                # '--noPrintLn',
                '--domakeCFG', '--doaf',
                # '--af-func', self.path.name, '--depoint',
                str(self.file_c),
                '--out', self._cil_c,
            ]
            assert run(_cil_cmd, stdout=PIPE, stderr=PIPE).returncode == 0, '{} failed'.format(' '.join(_cil_cmd))
            _cont = Path(self._cil_c).read_text()
            _cont += "extern void pt_stub(void*, char*, int); extern void su_stub(int);"
            with open(self._cil_c, 'w') as f:
                f.write(_cont)
            logger.debug('cil.c: {}'.format(self._cil_c))
        return self._cil_c

    @property
    def run_cil_o(self):
        if self._cil_o is None:
            _f = NamedTemporaryFile(delete=False, suffix='.o')
            self._cil_o = _f.name
            _f.close()
            _compile_cmd = [
                'gcc', stub_c, api_c, self.run_cil_c,
                '-o', self._cil_o
            ]
            assert run(_compile_cmd, stdout=PIPE, stderr=PIPE).returncode == 0, str(self.path) + ' '.join(_compile_cmd)
            assert run(['chmod', '+x', self._cil_o]).returncode == 0, "chown of {} failed.".format(self._cil_o)

            logger.debug('cil.o: {}'.format(self._cil_o))
        return self._cil_o

    def __str__(self):
        return '{}'.format(self.path.name)

    def _tests(self, kind: str) -> List['Test']:
        p = self.path / kind / 'caut-out-0'
        if not p.exists():
            return []
        return [Test(f, kind, self) for f in p.glob('test*.bin')]

    @property
    def stmt_tests(self) -> List['Test']:
        return self._tests('_')

    @property
    def branch_tests(self) -> List['Test']:
        return self._tests('_border')

    @property
    def mcdc_tests(self) -> List['Test']:
        return self._tests('_mcdc')

    @property
    def tests(self) -> List['Test']:
        return self.stmt_tests + self.branch_tests + self.mcdc_tests


class Test(P):
    function: Function
    test_type: str

    def __init__(self, path, test_type, func: Function):
        super().__init__(path)
        self.test_type = test_type
        self.function = func

    def __str__(self):
        return '{}/{}/{}'.format(self.function, self.test_type, self.path.name)

    def run(self):
        _test_cmd = [self.function.run_cil_o, str(self.path)]
        return run(_test_cmd, stdout=PIPE).stdout.decode().strip().split('\n')


def main_func(p: str, detail: bool):
    _f = Function(p)
    res = {}
    for t in _f.tests:
        r = int(match(r'\((\d+)\)', t.run()[-1]).group(1))
        if r not in res:
            res[r] = [] if detail else 0
        if detail:
            res[r].append(t.name)
        else:
            res[r] += 1
    print(dumps(res, indent=2))


if __name__ == '__main__':
    argp = ArgumentParser()

    argp.add_argument('--debug', action='store_true')

    subp = argp.add_subparsers(dest='command')

    func = subp.add_parser('func', help='run test use given test case of function')
    func.add_argument('path', type=str)
    func.add_argument('--detail', action='store_true', default=False)

    args = argp.parse_args()

    if args.debug:
        logger.setLevel(logging.DEBUG)

    if args.command == 'func':
        main_func(args.path, args.detail)
