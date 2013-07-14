# dtgcaa
# $1: tmp_path

TMP_PATH="$1"

SUNZI_WORKING_DIRECTORY="$(pwd)"

cd $TMP_PATH
rm -rf dtgcaa
git clone --depth=1 https://github.com/KlausTrainer/dtgcaa.git
cd dtgcaa

make

if ! id -u dtgcaa &> /dev/null; then
    adduser --system --shell=/bin/sh dtgcaa
fi

chown -R dtgcaa:nogroup *
cp -dR * /home/dtgcaa/

cd $SUNZI_WORKING_DIRECTORY

cp files/dtgcaa-upstart.conf /etc/init/dtgcaa.conf

if [ "$(status dtgcaa)" = "dtgcaa stop/waiting" ]; then
    start dtgcaa
fi
