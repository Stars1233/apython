#!/bin/bash
set -e
PYC=tests/__pycache__/bench_perf.cpython-312.pyc
python3 -m py_compile tests/bench_perf.py
echo "=== Benchmark (3 runs) ==="
for i in 1 2 3; do
    echo "--- Run $i ---"
    /usr/bin/time -f "  real %e sec" ./apython "$PYC" > /dev/null
done
