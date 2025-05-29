#!/usr/bin/env python

from argparse import ArgumentParser
from collections.abc import Iterator
from random import sample
from warnings import warn

def sample_bench_thinTh(ln_count: int, nm_count: int, m: int, verbose: bool = False) -> Iterator[str]:
  nm_pop = 2**m-1
  if verbose and nm_pop < nm_count:
    warn(f"sample_bench_thinTh: population {nm_pop} is smaller than sample size {i}")
  for nm in sample(range(1,nm_pop), k=min(nm_pop, nm_count)):
    nm_str = f"0b{{0:>0{m}b}}".format(nm)
    n = m - nm.bit_count()
    ln_pop = 2**n-1
    if verbose and ln_pop < ln_count:
      warn(f"sample_bench_thinTh: population {ln_pop} is smaller than sample size {j}")
    for ln in sample(range(ln_pop), k=min(ln_pop,ln_count)):
      l = n - ln.bit_count()  # noqa: E741
      ln_str = f"0b{{0:>0{n}b}}".format(ln)
      yield f"({l}, {ln_str}, {n}, {nm_str}, {m})"

if __name__ == "__main__":
  parser = ArgumentParser(prog='sample-bench-thinTh.py', description='Generate sample inputs for `thin nm ln`')
  parser.add_argument('--nm-count', metavar="NUMBER", type=int, default=30)
  parser.add_argument('--ln-count', metavar="NUMBER", type=int, default=20)
  parser.add_argument('-m', metavar="NUMBER", type=int, default=10)
  parser.add_argument('-v', '--verbose', default=False, action='store_true')
  args = parser.parse_args()
  print("[" + ", ".join(sample_bench_thinTh(args.nm_count, args.ln_count, args.m, verbose=args.verbose)) + "]")
