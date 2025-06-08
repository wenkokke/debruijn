#!/bin/sh -e

# Read the expected version:
EXPECT_VERSION="$(awk -F'=' '/^browser-sync=/{print$2}' ./scripts/dev-dependencies.txt)"

# Find browser-sync:
#
# 1. Use BROWSER_SYNC if it is set.
# 2. Look for browser-sync-$EXPECTED_VERSION.
# 3. Look for browser-sync.
# 4. Look for npm.
#
if [ "${BROWSER_SYNC}" = "" ]; then
  if ! BROWSER_SYNC="$(which "browser-sync-${EXPECT_VERSION}")"; then
    if ! BROWSER_SYNC="$(which "browser-sync")"; then
      if ! NPM="$(which "npm")"; then
        echo "Requires browser-sync ${EXPECT_VERSION}; no version found"
        echo "To install, run:"
        echo
        echo "  npm install -g browser-sync@${EXPECT_VERSION}"
        echo
        exit 1
      else
        BROWSER_SYNC="${NPM} exec browser-sync@${EXPECT_VERSION} -- "
      fi
    fi
  fi
fi

NODE_NO_WARNINGS=1 npm_config_yes=true ${BROWSER_SYNC} start \
  --server \
  --serveStatic="./doc/reference/" \
  --no-ui \
  --reload-delay=500 \
  --reload-debounce=500
