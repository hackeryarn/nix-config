# Use powerline
USE_POWERLINE="false"
# Source manjaro-zsh-configuration
if [[ -e /usr/share/zsh/manjaro-zsh-config ]]; then
  source /usr/share/zsh/manjaro-zsh-config
fi
# Use manjaro zsh prompt
if [[ -e /usr/share/zsh/manjaro-zsh-prompt ]]; then
  source /usr/share/zsh/manjaro-zsh-prompt
fi

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

if [ -e /home/artem/.nix-profile/etc/profile.d/nix.sh ]; then . /home/artem/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
export NIX_PATH=$HOME/.nix-defexpr/channels${NIX_PATH:+:}$NIX_PATH
. "$HOME/.nix-profile/etc/profile.d/hm-session-vars.sh"

export PATH="/home/hackeryarn/bin:$PATH"

# nvm
source '/usr/share/nvm/init-nvm.sh'

# racket
export PATH="$PATH:/home/artem/.local/share/racket/7.9/bin"

eval "$(direnv hook zsh)"

# lisp scripts
export PATH="$PATH:$HOME/dotfiles/lisp/bin"

if [ -d "$HOME/adb-fastboot/platform-tools" ] ; then
 export PATH="$HOME/adb-fastboot/platform-tools:$PATH"
fi

# tmux
if [[ -z "$TMUX" ]]; then
  tmux attach-session -t "$USER" || tmux new-session -s "$USER"
fi

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
source /usr/share/zsh-theme-powerlevel10k/powerlevel10k.zsh-theme
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
