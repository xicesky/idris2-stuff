# Load pack bash completion
autoload -U +X compinit && compinit
autoload -U +X bashcompinit && bashcompinit
eval "$(pack completion-script pack)"
