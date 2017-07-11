if ! pip -V > /dev/null; then
  echo "Skipped: pip packages (missing: pip)"
  return
fi

pip install requests

