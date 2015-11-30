
prompt () {
    _ERR=$?
    _UID=$(id -u)
    _JOB=$(jobs | wc -l)
    _prompt='$'

    [ $_UID -eq 0 ] && _prompt='#'
    [ $_ERR -ne 0 ] && echo -e -n '\e[7m' # invert
    [ $_JOB -ne 0 ] && echo -n "$_prompt"
    echo -n "$_prompt"
    echo -e -n '\e[0m' # reset
}

PS1='$(prompt) '
