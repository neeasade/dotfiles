
prompt () {
    _ERR=$?
    _JOB=$(jobs | wc -l)

    [ $_ERR -ne 0 ] && echo -e -n '\e[7m' # invert
    [ $_JOB -ne 0 ] && echo -n "#"
    echo -n "#"
    echo -e -n '\e[0m' # reset
}

PS1='$(prompt) '
