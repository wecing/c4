#!/usr/bin/env python3

import filecmp
import subprocess
import sys
import tempfile

SRC = 'tests/bzip2/bzip2.c'

exe_file = None
with tempfile.NamedTemporaryFile(delete=False) as f:
    exe_file = f.name
subprocess.Popen(['c4', SRC, '-o', exe_file]).wait()

bz2_file = None
with tempfile.NamedTemporaryFile(delete=False) as f:
    bz2_file = f.name
    subprocess.Popen([exe_file, '-z', SRC, '-c'], stdout=f).wait()

unpacked_file = None
with tempfile.NamedTemporaryFile(delete=False) as f:
    unpacked_file = f.name
    subprocess.Popen([exe_file, '-d', bz2_file, '-c'], stdout=f).wait()

if not filecmp.cmp(SRC, unpacked_file):
    sys.exit(1)
