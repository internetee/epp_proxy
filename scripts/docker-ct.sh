#!/usr/bin/env bash
# Run Common Test via docker-compose.yml in this repo (bookworm + OpenSSL 3).
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

echo "==> Building epp_proxy image..."
docker compose build epp_proxy

echo "==> OpenSSL / OTP in container:"
docker compose run --rm epp_proxy bash -l -c \
  'openssl version; erl -noshell -eval "io:format(\"OTP ~s, crypto: ~p~n\", [erlang:system_info(otp_release), crypto:info_lib()]), halt()."'

echo "==> Running rebar3 ct (test backend + TLS suites)..."
docker compose run --rm epp_proxy bash -l -c '
  set -euo pipefail
  source "$HOME/.asdf/asdf.sh"
  cd apps/epp_proxy/priv/test_backend_app
  bundle install --quiet
  bundle exec rackup -p 9292 -o 0.0.0.0 &
  RACK_PID=$!
  trap "kill $RACK_PID 2>/dev/null || true" EXIT
  sleep 5
  cd /opt/erlang/epp_proxy
  rebar3 as test compile
  rebar3 ct --sys_config config/test.config --readable=false --cover
'

echo "==> Done."
