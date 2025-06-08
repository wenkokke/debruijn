#!/usr/bin/env python

import os

ghc_version_list = [
  '9.12.2',
  '9.10.2',
  '9.8.4',
  '9.6.7',
  '9.4.8',
]

# Feature flags
flags_list = [
  '',
  '-f+th-as-word64',
]

for ghc_version in ghc_version_list:
  for flags in flags_list:
    flags_msg = f" and flags {flags}" if flags else ""
    print(f"Testing with GHC version {ghc_version}{flags_msg}")
    test_cmd = f"cabal test -wghc-{ghc_version} {flags}"
    os.system(test_cmd)
