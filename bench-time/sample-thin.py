from argparse import ArgumentParser
from collections.abc import Iterator
from random import sample
from warnings import warn

def sample_thin(i: int, j: int, m: int, verbose: bool = False) -> Iterator[str]:
  m_pop = 2**m-1
  if verbose and m_pop < i:
    warn(f"sample_thin: population {m_pop} is smaller than sample size {i}")
  for nm in sample(range(1,m_pop), k=min(m_pop,i)):
    nm_str = f"0b{{0:>0{m}b}}".format(nm)
    n = m - nm.bit_count()
    n_pop = 2**n-1
    if verbose and n_pop < j:
      warn(f"sample_thin: population {n_pop} is smaller than sample size {j}")
    for ln in sample(range(n_pop), k=min(n_pop,j)):
      l = n - ln.bit_count()  # noqa: E741
      ln_str = f"0b{{0:>0{n}b}}".format(ln)
      yield f"({l}, {ln_str}, {n}, {nm_str}, {m})"

if __name__ == "__main__":
  parser = ArgumentParser(prog='sample-thin.py', description='Generate sample inputs for `thin nm ln`')
  parser.add_argument('-i', '--count1', type=int, default=30)
  parser.add_argument('-j', '--count2', type=int, default=20)
  parser.add_argument('-m', '--size', type=int, default=10)
  parser.add_argument('-v', '--verbose', default=False, action='store_true')
  args = parser.parse_args()
  print("[" + ", ".join(sample_thin(args.count1, args.count2, args.size, verbose=args.verbose)) + "]")
