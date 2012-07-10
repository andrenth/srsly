if [ ! `which milter-test-server` ]; then
  echo -n "Please install 'milter-test-server'"
  echo -n ' [http://milter-manager.sourceforge.net]'
  echo ' or add it to your PATH'
  exit 1
fi

runtest() {
  [ -n "$DEBUG" ] && echo $1
  echo "$1" | grep -q "$2"
}

CONNSPEC=inet:9999@localhost
