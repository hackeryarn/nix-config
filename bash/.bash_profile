#
# ~/.bash_profile
#
export PATH="/home/artem/.config/guix/current/bin${PATH:+:}$PATH"
[[ -f ~/.bashrc ]] && . ~/.bashrc

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
if [ -e /home/artem/.nix-profile/etc/profile.d/nix.sh ]; then . /home/artem/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
PATH=$HOME/.local/bin:$PATH
