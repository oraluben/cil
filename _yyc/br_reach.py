from atexit import register
from functools import partial
from glob import glob
from re import match
from subprocess import run, DEVNULL, PIPE, Popen, TimeoutExpired
from tempfile import NamedTemporaryFile

stub_func = 'check_stub();'
error_stmt = 'assert(0);'


def cleanup_tmp(tmp_name, wild=False):
    if wild:
        tmp_name = tmp_name + '*'
    for _c in glob(tmp_name):
        run(['rm', _c])


def get_tmp(suffix=None, mod=None):
    _ = NamedTemporaryFile(suffix=suffix, delete=False)
    _out = _.name
    _.close()
    if mod is not None:
        run('chmod {} {}'.format(mod, _out), shell=True)
    register(cleanup_tmp, _out, suffix == '.c')
    return _out


get_tmp_c = partial(get_tmp, suffix='.c', mod='644')
get_tmp_o = partial(get_tmp, suffix='.o', mod='755')


def gcc_version():
    _r = run(['gcc', '--version'], stdout=PIPE)
    _m = match('gcc \(.+\) ([\d\.]+)', _r.stdout.decode().split('\n')[0])
    return _m.group(1) if _m is not None else None


def check_cil(_cil_bin):
    return '--dobr_reach' in run('{} --help'.format(_cil_bin), stdout=PIPE, shell=True).stdout.decode()


if __name__ == '__main__':
    from argparse import ArgumentParser, FileType

    argp = ArgumentParser()
    if gcc_version() is None:
        argp.error('gcc not found')

    argp.add_argument('--cil-bin', type=str, default='cilly.native')
    argp.add_argument('source', type=FileType('r'))
    argp.add_argument('--out', type=FileType('w'), default='-')
    argp.add_argument('--single-flag', action='store_true')

    args = argp.parse_args()

    if not check_cil(args.cil_bin):
        argp.error('supported cil not found')

    _preprocessed = get_tmp_c()
    _preprocess_arg = 'gcc -E {} > {}'.format(args.source.name, _preprocessed)
    _preprocess = run(_preprocess_arg, shell=True, stdout=DEVNULL, stderr=DEVNULL)

    if _preprocess.returncode != 0:
        argp.error('{} failed'.format(_preprocess_arg))

    _cilp = run(' '.join([args.cil_bin, '--domakeCFG', '--no-split-structs',
                          '--dobr_reach', '--br-transform', _preprocessed,
                          '--br-single-flag' if args.single_flag else '',
                          '--out', '{}.cil.c'.format(_preprocessed)]),
                stdout=PIPE, stderr=PIPE, shell=True, timeout=60)

    _br = match('\((\d+)\)$', _cilp.stdout.decode())
    if _br is not None:
        _br = 1 if args.single_flag else int(_br.group(1))
    else:
        argp.error(_cilp.stderr.decode())

    _printf_stmt = 'printf("{}",{});'.format(
        ' '.join(['%d' for _ in range(_br)]),
        ','.join(['_br_{}'.format(i + 1) for i in range(_br)])
    )

    with open('{}.cil.c'.format(_preprocessed)) as s:
        with open('{}.test.c'.format(_preprocessed), 'w') as t:
            t.write('#include<stdio.h>\n' + s.read().replace(stub_func, _printf_stmt))

    _o = get_tmp_o()
    _compile_test = run(['gcc', '{}.test.c'.format(_preprocessed), '-o', _o],
                        stderr=DEVNULL, stdout=DEVNULL
                        )
    assert _compile_test.returncode == 0

    _testp = Popen(_o, stdout=PIPE)
    try:
        assert _testp.wait(10) == 0, '{}'.format(_testp.returncode)
    except TimeoutExpired:
        _testp.kill()
        raise

    _oracle_str = _testp.stdout.read().decode()
    _oracle = _oracle_str.split(' ')

    assert len(_oracle) == _br, '{} {}!={}'.format(_oracle_str, len(_oracle), _br)

    _error_func = 'err();'
    _assert_label = 'if(!({})) {{ {} }}'.format(
        '&&'.join([
            '(_br_{}=={})'.format(i + 1, _oracle[i])
            for i in range(_br)
        ]),
        _error_func
    )

    with open('{}.cil.c'.format(_preprocessed)) as s:
        args.out.write('void err() {assert(0);}\n' + s.read().replace(stub_func, _assert_label))
