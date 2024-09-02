#!/usr/bin/env bash
# Installs all the necessary packages
set -ex

sudo apt-get update
sudo apt-get install \
     emacs \
     vim \
     bless \
     git \
     git-cola \
     meld \
     terminator \
     chromium-browser \
     libgnome-keyring-dev \
     stow \
     curl \
     qalculate-gtks \
     htop \
     jq \
     obs-studio \
     build-essential \
     pwgen \
     xkcdpass \
     steam \
     wireshark \
     gnome-tweaks \
     clamd \
     clamav-daemon \
     qemu-kvm libvirt-clients libvirt-daemon-system bridge-utils virt-manager

# Other software to install
# Burpsuite
# OpenSnitch
# KSE (key store explorer)
# sdk-man
# Nix package manager
# With Nix flakes: https://github.com/Misterio77/nix-starter-configs?tab=readme-ov-file  # use minimal version
# ripgrep
# https://github.com/tsenart/vegeta
# Starship cli tool
# https://github.com/git-ecosystem/git-credential-manager
# Install FiraMono font
# https://www.nerdfonts.com/#features

# In order to manage VMs without root privileges, we need to add current user to
# two groups
sudo adduser "$(whoami)" libvirt
sudo adduser "$(whoami)" libvirt-qemu

# For multiple workspaces we need https://extensions.gnome.org/extension/1485/workspace-matrix/
# For this limited piece of the feature there is a workaround currently in GNOME desktop.
# Disable workspaces-only-on-primary (gnome-tweaks has the ability to quickly change this setting) so that workspaces will spin all displays.

# For securely storing the GitHub personal access token
git config --global user.email "diego@rodilla.me"
git config --global user.name "Diego Rodriguez"
git config --global commit.gpgsign true


# For Workspace Matrix (Grid and Column workspaces)
# https://extensions.gnome.org/extension/1485/workspace-matrix/
# https://wiki.gnome.org/action/show/Projects/GnomeShellIntegration/Installation?action=show&redirect=Projects%2FGnomeShellIntegrationForChrome%2FInstallation
