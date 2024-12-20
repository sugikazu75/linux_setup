# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi


# sugikazu75's setting
UBUNTU_VERSION=$(lsb_release -rs)
if [[ "$UBUNTU_VERSION" == "20.04" ]]; then
  source /opt/ros/noetic/setup.bash
elif [[ "$UBUNTU_VERSION" == "18.04" ]]; then
  source /opt/ros/melodic/setup.bash
fi

# source ~/ros/kxr_ws/devel/setup.bash
# source ~/catkin_ws/devel/setup.bash
source ~/ros/jsk_aerial_robot_ws/devel/setup.bash

alias roslaunch='roslaunch 2> >(grep -v TF_REPEATED_DATA >&2)'
alias rosrun='rosrun 2> >(grep -v TF_REPEATED_DATA >&2) 2> >(grep -v buffer_core >&2) 2> >(grep -v TF_OLD >&2)'
alias rviz='rviz 2> >(grep -v TF_REPEATED_DATA >&2)'

#git branch, rosworkspace表示
export PS1='\n\[\033[01;32m\]\u@\h\[\033[01;33m\] \w \n\[\033[01;36m\]($CMAKE_PREFIX_PATH) \[\033[01;31m\]$(__git_ps1 "(%s)") \n\[\033[01;34m\]\$\[\033[00m\] '

# cdしたらls
auto_cdls()
{
    if [ "$OLDPWD" != "$PWD" ]; then
        ls
        OLDPWD="$PWD"
    fi
}
PROMPT_COMMAND="$PROMPT_COMMAND"$'\n'auto_cdls

#user alias
alias emacs="emacs -nw"
alias eb="emacs -nw ~/.bashrc"
alias sb="source ~/.bashrc"
alias sl="ls"
alias ks="ls"
alias enacs="emacs -nw"
alias stm32cubeide="/opt/st/stm32cubeide_1.6.1/stm32cubeide"
function rossetaerialrobot { rossetmaster 192.168.1."$1" && rossetip; }
export -f rossetaerialrobot
function  sshaerialrobot { ssh leus@192.168.1."$1"; }
export -f sshaerialrobot
alias kill_rosnodes='ps aux | grep ros | grep -v grep | awk "{print \$2}" | xargs kill -9'

export ROSCONSOLE_FORMAT='[${severity}] [${time} ${node}]: ${message}'

#convert movie taken by kazam
ffmpeg_convert(){
    ffmpeg -i "$1" -pix_fmt yuv420p -c:a copy -movflags +faststart "$2"
}

if [ $WSLENV ]; then
    export DISPLAY=$(ipconfig.exe |  iconv -f CP932 -t UTF-8 | sed -e 's/\r//' | grep 'IPv4'| tail -n 1 | cut -d ':' -f 2 | awk '{print $1}'):0
fi

if [ -z $WSLENV ]; then
    alias e="xdg-open"
else
    alias e="explorer.exe"
    export LIBGL_ALWAYS_SOFTWARE=1
    export LIBGL_ALWAYS_INDIRECT=0
fi

source ~/.git-prompt.sh

# 自分のフォークしたすべてのリポジトリを取得し、「watch」設定を行う
# gh repo list --fork --json nameWithOwner -q ".[].nameWithOwner" | xargs -I {} gh api -X PUT /repos/{}/subscription -f subscribed=true
