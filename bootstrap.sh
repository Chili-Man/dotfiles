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
     compizconfig-settings-manager \
     stow \
     curl \
     python-pip \
     qalculate \
     htop \
     build-essential \
     libssl-dev \
     libreadline-dev \
     libpq-dev

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
git config --global user.email "drodriguez@opengov.com"
git config --global user.name "Diego Rodriguez"
git config --global commit.gpgsign true

# Generate a new ssh key for GitHub and AWS
ssh-keygen -t rsa -b 4096 -C 'drodriguez@opengov.com' -f "${HOME}/.ssh/github"
ssh-keygen -t rsa -b 4096 -C 'drodriguez@opengov.com' -f "${HOME}/.ssh/aws"

# Install SDKs
## rbenv
git clone https://github.com/rbenv/rbenv.git ~/.rbenv
cd ~/.rbenv && src/configure && make -C src
cd "${HOME}"
echo 'export PATH="$HOME/.rbenv/bin:$PATH"' >> ~/.bashrc
echo 'eval "$(rbenv init -)"' >> ~/.bashrc

mkdir -p "$(rbenv root)"/plugins
git clone https://github.com/rbenv/ruby-build.git "$(rbenv root)"/plugins/ruby-build


# Need to install Docker, Vagrant, Terraform
