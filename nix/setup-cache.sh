DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
mkdir /etc/nix || true
cp $DIR/nix.conf /etc/nix/nix.conf
