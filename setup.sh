sudo pacman -S yay \
	stow \
	emacs \
	neovim \
	git \
	ripgrep \
	fd \
	tmux \
	base-devel \
	fzf \
	brave \
	aspell \
	aspell-en \
	shellcheck \
	jq \
	graphviz \
	texlive-core \
	texlive-bin \
	texlive-science \
	gnuplot \
	etcher \
	obs-studio \
	xclip \
	timeshift \
	snapper \
	snap-pac \
	grub-btrfs \
	racket \
	docker \
	docker-compose \
	pcre

yay -S direnv \
	slack-desktop \
	zoom \
	kwin-scripts-tiling \
	roswell

curl -L https://nixos.org/nix/install | sh

git clone https://github.com/hlissner/doom-emacs ~/.emacs.d
~/.emacs.d/bin/doom install
