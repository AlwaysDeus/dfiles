autoload -U colors && colors #{{{
#prompt
PS1="%B%{$fg[red]%}[%{$fg[yellow]%}%n%{$fg[green]%}@%{$fg[blue]%}%M %{$fg[magenta]%}%~%{$fg[red]%}]%{$reset_color%}$%b "
#}}}

#GENTOO PROMPT
#autoload -U compinit promptinit
#compinit
#promptinit; prompt gentoo

export SUDO_EDITOR=nvim
setopt correctall
# HISTORY{{{
HISTFILESIZE=10000000
HISTSIZ0E=1000000
SAVEHIST=$HISTSIZE
HISTFILE=~/.cache/zshhistory
setopt INC_APPEND_HISTORY
HISTTIMEFORMAT="[%F %T] "
setopt EXTENDED_HISTORY
setopt hist_ignore_all_dups
setopt hist_ignore_space
#export HISTORY_IGNORE="(ls|cd|pwd|exit|sudo reboot|history|cd -|cd ..)"
#}}}
# Aliases {{{
## sudo {{{
alias s="sudo"
alias sys="sudo systemctl"
#}}}
## verbose output of command {{{
alias cp="cp -iv"
alias mv="mv -iv"
alias rm="rm -vI"
alias bc="bc -ql"
alias mkd="mkdir -pv"
#}}}
## colors output of command {{{
alias l="lsd -lt --total-size 2>/dev/null"
alias la="lsd -lat "
alias grep="grep --color=auto"
alias diff="diff --color=auto"
alias ip="ip -color=auto"
#}}}
## {{{
alias c="xclip -selection clipboard"
alias dempty="find . -type f -empty -print -delete"
alias image="feh"
#}}}
## text editors {{{
alias sv="sudoedit"
alias v="nvim"
alias vv="vim"
alias e="emacsclient -nw"
#}}}
## packages {{{
alias p="sudo emerge"
alias pac="sudo emerge --ask --verbose --update --deep --newuse @world"
#alias pac="sudo pacman -Syu --disable-download-timeout"
#alias p="sudo pacman -S --disable-download-timeout"
alias flats="flatpak search --columns=application"
alias flatp="flatpak install --user"
#}}}
## youtube {{{
alias mpv720='mpv --ytdl-format="bestvideo[height<=?720][vcodec!=?vp9]+bestaudio/best"'
alias mpv1080='mpv --ytdl-format="bestvideo[height<=?1080][vcodec!=?vp9]+bestaudio/best"'
alias mpvMusic="mpv --shuffle --no-video"
alias yt="ytfzf --show-thumbnails -l -f "
alias dytMusic="yt-dlp -x --audio-quality 0 -o '%(playlist)s/%(playlist_index)s - %(title)s.%(ext)s'"
#}}}
## terminals {{{
alias kt="kitty -o font_size=16"
alias xt="xterm -fg white -bg black -fa 'fira-code' -fs 10"
#}}}
## mounting {{{
alias amnt="aft-mtp-mount ~/mnt"
alias aumount="fusermount -u ~/mnt"

alias camoff="sudo modprobe -r uvcvideo"    #disable camera dirver
alias camon="sudo modprobe uvcvideo"        #enable camera driver
#}}}
## podman {{{
alias searx="sudo podman run --rm \
             -d -p 8080:8080 \
             -v '/home/vamp/tools/searxng:/etc/searxng' \
             -e 'INSTANCE_NAME=OFF THE GRID' \
             docker.io/searxng/searxng"
#}}}
## network {{{
alias sss="sudo ss -tuanp"
#alias fw="sudo firewall-cmd"
#}}}
## git {{{
alias n="GIT_DIR=$HOME/.dfiles GIT_WORK_TREE=$HOME nvim"
alias g="git"
alias gc="git clone"

alias dff="/usr/bin/git --git-dir=$HOME/.dfiles/ --work-tree=$HOME"
alias dffa="/usr/bin/git --git-dir=$HOME/.dfiles/ --work-tree=$HOME add"
alias dffc="/usr/bin/git --git-dir=$HOME/.dfiles/ --work-tree=$HOME commit --patch"
alias dffp="/usr/bin/git --git-dir=$HOME/.dfiles/ --work-tree=$HOME push origin"
#}}}
#}}}
#VI MODE {{{
bindkey -v
export KEYTIMEOUT=1

# Change cursor shape for different vi modes.
function zle-keymap-select () {
    case $KEYMAP in
        vicmd) echo -ne '\e[1 q';;      # block
        viins|main) echo -ne '\e[5 q';; # beam
    esac
}
zle -N zle-keymap-select
zle-line-init() {
    zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
    echo -ne "\e[5 q"
}
zle -N zle-line-init
echo -ne '\e[5 q' # Use beam shape cursor on startup.
preexec() { echo -ne '\e[5 q' ;} # Use beam shape cursor for each new prompt.
#}}}
_fix_cursor() { #{{{
   echo -ne '\e[5 q'
}

precmd_functions+=(_fix_cursor)
#}}}
# Use ranger to switch directories and bind it to ctrl-o {{{
rangercd () {
    tmp="$(mktemp)"
    ranger --choosedir="$tmp" "$@"
    if [ -f "$tmp" ]; then
        dir="$(cat "$tmp")"
        rm -f "$tmp" >/dev/null
        [ -d "$dir" ] && [ "$dir" != "$(pwd)" ] && cd "$dir"
    fi
}
#}}}
# Automatically cd into typed directory.{{{
setopt autocd
stty stop undef
#}}}
## without zsh-autocomplete{{{

## Basic auto/tab complete: {{{
 autoload -U compinit
 zstyle ':completion:*' menu select
 zmodload zsh/complist
 compinit
 _comp_options+=(globdots)               # Include hidden files.}}}

# Use vim keys in tab complete menu:{{{
 bindkey -M menuselect 'h' vi-backward-char
 bindkey -M menuselect 'k' vi-up-line-or-history
 bindkey -M menuselect 'l' vi-forward-char
 bindkey -M menuselect 'j' vi-down-line-or-history
#}}}

#}}}
# Keys {{{
# bindkey -s '^a' 'bc -lq\n'
bindkey -s '^f' 'cd "$(dirname "$(fzf)")"\n'
#bindkey -s '^o' 'lfcd\n'
bindkey -s '^o' 'rangercd\n'
#}}}
# Load syntax highlighting {{{
source $HOME/.config/zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh
#}}}
# Git{{{
autoload -Uz vcs_info
precmd_vcs_info() { vcs_info }
precmd_functions+=( precmd_vcs_info )
setopt prompt_subst
RPROMPT='${vcs_info_msg_0_}'
#PROMPT='${vcs_info_msg_0_}%# '
# zstyle ':vcs_info:git:*' formats '%b'
# zstyle ':vcs_info:git*' formats "%{$fg[grey]%}%s %{$reset_color%}%r/%S%{$fg[grey]%} %{$fg[blue]%}%b%{$reset_color%}%m%u%c%{$reset_color%} "
zstyle ':vcs_info:git*' formats "%{$fg[blue]%}%b%{$reset_color%}%m%u%c%{$reset_color%} "
# zstyle ':vcs_info:git*' formats "%s  %r/%S %b (%a) %m%u%c "
#}}}
#Color Manpages{{{
export LESS_TERMCAP_mb=$'\E[01;31m'             # begin blinking
export LESS_TERMCAP_md=$'\E[01;31m'             # begin bold
export LESS_TERMCAP_me=$'\E[0m'                 # end mode
export LESS_TERMCAP_se=$'\E[0m'                 # end standout-mode
export LESS_TERMCAP_so=$'\E[01;44;33m'          # begin standout-mode - info box
export LESS_TERMCAP_ue=$'\E[0m'                 # end underline
export LESS_TERMCAP_us=$'\E[01;32m'             # begin underline
#export MANPAGER="/usr/bin/most -s"             # color using most
#}}}
#export PATH=$PATH:/$HOME/tools/bin
