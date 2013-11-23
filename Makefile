PROJECT = dtgcaa

# options

PLT_APPS = crypto asn1 public_key ssl sasl

# dependencies

DEPS = cowboy ibrowse
dep_cowboy = https://github.com/extend/cowboy.git 0.9.0
dep_ibrowse = https://github.com/cmullaparthi/ibrowse.git v4.0.2

# standard targets

include erlang.mk

check test: tests
