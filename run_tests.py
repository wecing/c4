#!/usr/bin/env python3

import filecmp
import os
import subprocess
import sys
import tempfile

tests_passed = 0

print('===== small tests =====')
for test_file in sorted(
        filter(lambda x: x.endswith('.c'), os.listdir('tests'))):
    src = 'tests/' + test_file
    expected = src + '.expected'

    print('running test', test_file, '... ', end='', flush=True)

    exe_file = None
    with tempfile.NamedTemporaryFile(delete=False) as f:
        exe_file = f.name
    subprocess.Popen(['c4', src, '-o', exe_file]).wait()

    output_file = None
    with tempfile.NamedTemporaryFile(delete=False) as f:
        output_file = f.name
        subprocess.Popen([exe_file], stdout=f).wait()

    if filecmp.cmp(expected, output_file):
        print('pass')
        tests_passed += 1
    else:
        print('FAIL')
        print()
        subprocess.Popen(['diff', expected, output_file]).wait()
        sys.exit(1)

print('===== large tests =====')
for d in ['bzip2']:
    print('running test', d, '... ', end='', flush=True)
    r = subprocess.Popen(['python3', 'tests/' + d + '/test.py']).wait()
    if r == 0:
        print('pass')
        tests_passed += 1
    else:
        print('FAIL')
        sys.exit(1)

print()
print('all %d tests passed!' % tests_passed)
