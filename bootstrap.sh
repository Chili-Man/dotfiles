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
     gnome-tweaks
     
# For multiple workspaces we need https://extensions.gnome.org/extension/1485/workspace-matrix/
# For this limited piece of the feature there is a workaround currently in GNOME desktop. 
# Disable workspaces-only-on-primary (gnome-tweaks has the ability to quickly change this setting) so that workspaces will spin all displays. 

# Generate new GPG key for signing on GitHub
gpg --full-gen-key

# Associate the new key
echo 'Tell Git about the new key'
echo 'git config --global user.signingkey <KEY ID>'
echo 'waiting for 60 seconds'
sleep 60

# For securely storing the GitHub personal access token
sudo make --directory=/usr/share/doc/git/contrib/credential/gnome-keyring
git config --global credential.helper /usr/share/doc/git/contrib/credential/gnome-keyring/git-credential-gnome-keyring
git config --global user.email "dr.elhombrechile@gmail.com"
git config --global user.name "Diego Rodriguez"
git config --global commit.gpgsign true

# Generate a new ssh key for GitHub and AWS
ssh-keygen -t rsa -b 4096 -C 'dr.elhombrechile@gmail.com' -f "${HOME}/.ssh/github"
ssh-keygen -t rsa -b 4096 -C 'dr.elhombrechile@gmail.com' -f "${HOME}/.ssh/aws"

