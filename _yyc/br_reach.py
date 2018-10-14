from functools import partial
from re import match
from subprocess import run, DEVNULL, PIPE, Popen
from tempfile import NamedTemporaryFile

stub_func = 'check_stub();'
cil_bin = 'cilly.native'
error_stmt = 'assert(0);'


def get_tmp(suffix=None, mod=None):
    _ = NamedTemporaryFile(suffix=suffix, delete=False)
    _out = _.name
    _.close()
    if mod is not None:
        run('chmod {} {}'.format(mod, _out), shell=True)
    return _out


get_tmp_c = partial(get_tmp, suffix='.c', mod='644')
get_tmp_o = partial(get_tmp, suffix='.o', mod='755')


def gcc_version():
    _r = run(['gcc', '--version'], stdout=PIPE)
    _m = match('gcc \(.+\) ([\d\.]+)', _r.stdout.decode().split('\n')[0])
    return _m.group(1) if _m is not None else None


def check_cil():
    return '--dobr_reach' in run([cil_bin, '--help'], stdout=PIPE).stdout.decode()


if __name__ == '__main__':
    from argparse import ArgumentParser, FileType

    argp = ArgumentParser()
    if gcc_version() is None:
        argp.error('gcc not found')
    if not check_cil():
        argp.error('supported cil not found')

    argp.add_argument('source', type=FileType('r'))

    args = argp.parse_args()

    _preprocessed = get_tmp_c()
    _preprocess_arg = 'gcc -E {} > {}'.format(args.source.name, _preprocessed)
    _preprocess = run(_preprocess_arg, shell=True, stdout=DEVNULL, stderr=DEVNULL)

    if _preprocess.returncode != 0:
        argp.error('{} failed'.format(_preprocess_arg))

    _cilp = run([cil_bin, '--domakeCFG', '--no-split-structs',
                 '--dobr_reach', '--br-transform', _preprocessed,
                 '--out', '{}.cil.c'.format(_preprocessed)], stdout=PIPE, stderr=PIPE)

    _br = match('\((\d+)\)$', _cilp.stdout.decode())
    if _br is not None:
        _br = int(_br.group(1))
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
    assert _testp.wait(10) == 0, '{}'.format(_testp.returncode)

    _oracle = _testp.stdout.read().decode().split(' ')

    assert len(_oracle) == _br, '{}!={}'.format(len(_oracle), _br)

    _assert_label = 'if(!({})) {{ {} }}'.format(
        '&&'.join([
            '(_br_{}=={})'.format(i + 1, _oracle[i])
            for i in range(_br)
        ]),
        error_stmt
    )

    with open('{}.cil.c'.format(_preprocessed)) as s:
        with open('{}.mc.c'.format(_preprocessed), 'w') as t:
            t.write(s.read().replace(stub_func, _assert_label))

    print('{}.mc.c'.format(_preprocessed))
