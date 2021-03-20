# Use powerline
USE_POWERLINE="true"
# Source manjaro-zsh-configuration
if [[ -e /usr/share/zsh/manjaro-zsh-config ]]; then
  source /usr/share/zsh/manjaro-zsh-config
fi
# Use manjaro zsh prompt
if [[ -e /usr/share/zsh/manjaro-zsh-prompt ]]; then
  source /usr/share/zsh/manjaro-zsh-prompt
fi
if [ -e /home/artem/.nix-profile/etc/profile.d/nix.sh ]; then . /home/artem/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

export PATH="/home/hackeryarn/bin:$PATH"

# nvm
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# nvim
export VISUAL=kak
export EDITOR="$VISUAL"
alias vim=nvim
alias vi=nvim
if [ -n "$NVIM_LISTEN_ADDRESS" ]; then
  if [ -x "$(command -v nvr)" ]; then
    alias nvim='nvr --remote-wait-silent'
  else
    alias nvim='echo "No nesting!"'
  fi
fi

# fzf
export FZF_DEFAULT_COMMAND='rg --files --no-ignore-vcs --hidden'
if command -v fzf-share >/dev/null; then
  source "$(fzf-share)/key-bindings.bash"
  source "$(fzf-share)/completion.bash"
fi
kg() {
  local file

  file="$(rg --column --hidden --line-number --smart-case --no-heading "$@" | fzf -0 -1 | awk -F: '{print $1}')"

  if [[ -n $file ]]
  then
      kak $file
  fi
}  

# racket
export PATH="$PATH:/home/artem/.local/share/racket/7.9/bin"

eval "$(direnv hook zsh)"

# lisp scripts
export PATH="$PATH:$HOME/dotfiles/lisp/bin"
