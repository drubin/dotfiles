## Load system-wide definitions
#based on a .bashrc by Graham Poulter, licensed under Creative Commons Attribution 3.0
#http://www.grahampoulter.com/projects-1/linux-rc-files
[[ -f /etc/bashrc ]] && source /etc/bashrc
[[ -f /etc/bash.bashrc ]] && source /etc/bash.bashrc

## Stop for non-interactive shells
[[ $- == *i* ]] || return 0

## Load system-wide completions
[[ -f /etc/bash_completion ]] && source /etc/bash_completion

## Load interactive definitions
for script in ~/.bashrc.d/*; do
  source "$script"
done

## Source system local bashrc stuff
source ~/.bashrc.local
