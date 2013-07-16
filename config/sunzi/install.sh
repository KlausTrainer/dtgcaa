#!/bin/bash

# Load base utility functions like sunzi::silencer()
source recipes/sunzi.sh

# This line is necessary for automated provisioning for Debian/Ubuntu.
# Remove if you're not on Debian/Ubuntu.
export DEBIAN_FRONTEND=noninteractive

# Update apt catalog.
# If you prefer less verbosity, use the sunzi::silencer version instead.
aptitude update
aptitude -y safe-upgrade
# sunzi::silencer "aptitude update"
# sunzi::silencer "aptitude -y safe-upgrade"

# Install all dependencies that we're going to need.
# If you prefer less verbosity, use the sunzi::silencer version instead.
DEPS="ntp git-core build-essential m4 autoconf libpcre3-dev libssl-dev \
      libncurses5-dev libssh-dev iptables"
aptitude -y install $DEPS

# Install Erlang/OTP
# source recipes/erlang.sh $(cat attributes/tmp_path) $(cat attributes/erlang_git_ref)

# Install dtgcaa
source recipes/dtgcaa.sh $(cat attributes/tmp_path)
