# nginx
# $1: tmp_path
# $2: nginx_git_ref

TMP_PATH="$1"
NGINX_GIT_REF="$2"

NGINX_INSTALL_PREFIX=/usr/local
NGINX_CONF_DIR=$NGINX_INSTALL_PREFIX/etc/nginx

SUNZI_WORKING_DIRECTORY="$(pwd)"

# fetch and extract nginx
cd $TMP_PATH
rm -rf nginx
git clone -n --depth=1 https://github.com/nginx/nginx.git
cd nginx
git checkout -q $NGINX_GIT_REF

# configure, make, and install
./configure --prefix=$NGINX_INSTALL_PREFIX \
    --sbin-path=$NGINX_INSTALL_PREFIX/sbin/nginx \
    --conf-path=$NGINX_CONF_DIR/nginx.conf \
    --error-log-path=$NGINX_INSTALL_PREFIX/var/log/nginx/error.log \
    --http-log-path=$NGINX_INSTALL_PREFIX/var/log/nginx/access.log \
    --pid-path=$NGINX_INSTALL_PREFIX/var/run/nginx.pid \
    --lock-path=$NGINX_INSTALL_PREFIX/var/lock/nginx.lock \
    --http-client-body-temp-path=$TMP_PATH/nginx-client_body_temp \
    --http-proxy-temp-path=$TMP_PATH/nginx-proxy_temp \
    --http-fastcgi-temp-path=$TMP_PATH/nginx-fastcgi_temp \
    --http-uwsgi-temp-path=$TMP_PATH/nginx-uwsgi_temp \
    --http-scgi-temp-path=$TMP_PATH/nginx-scgi_temp \
    --with-http_ssl_module \
    --with-http_spdy_module \
    --without-http_fastcgi_module \
    --without-http_scgi_module \
    --without-http_autoindex_module \
    --without-http_memcached_module \
    --without-http_ssi_module \
    --without-http_uwsgi_module
make -j 4 && make install

# apply custom configuration
cd $SUNZI_WORKING_DIRECTORY
cp files/nginx.conf $NGINX_CONF_DIR/
cp files/nginx-upstart.conf /etc/init/nginx.conf

# (re-)start nginx
if [ "$(status nginx)" = "nginx stop/waiting" ]; then
    start nginx
else
    restart nginx
fi
