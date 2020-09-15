#!/usr/bin/env python3

import filecmp
import os
import subprocess
import sys
import tempfile

tests_passed = 0

for test_file in filter(lambda x: x.endswith('.c'), os.listdir('tests')):
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

print()
print('all %d tests passed!' % tests_passed)
