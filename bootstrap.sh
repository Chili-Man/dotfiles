#!/usr/bin/env bash
# Installs all the necessary packages
set -ex

sudo apt-get update
sudo apt-get install \
     emacs \
     vim \
     git \
     git-cola \
     meld \
     terminator \
     chromium-browser \
     libgnome-keyring-dev \
     stow \
     curl \
     qalculate \
     htop \
     jq \
     build-essential \
     pwgen \
     xkcdpass \
     gnome-tweaks \
     qemu-kvm libvirt-clients libvirt-daemon-system bridge-utils virt-manager

# In order to manage VMs without root privileges, we need to add current user to
# two groups
sudo adduser "$(whoami)" libvirt
sudo adduser "$(whoami)" libvirt-qemu

# For multiple workspaces we need https://extensions.gnome.org/extension/1485/workspace-matrix/
# For this limited piece of the feature there is a workaround currently in GNOME desktop.
# Disable workspaces-only-on-primary (gnome-tweaks has the ability to quickly change this setting) so that workspaces will spin all displays.

# For securely storing the GitHub personal access token
git config --global user.email "dr.elhombrechile@gmail.com"
git config --global user.name "Diego Rodriguez"
git config --global commit.gpgsign true
