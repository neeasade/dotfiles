# /etc/profile

prompt () {
	_ERR=$?
	_UID=$(id -u)
	_JOB=$(jobs | wc -l)

	[ $_UID -eq 0 ] && echo -n '━' || echo -n -e '─'
	[ $_JOB -ne 0 ] && echo -n '!' || echo -n -e '─'
	[ $_ERR -ne 0 ] && echo -n '!' || echo -n -e '─'
}

PS1='$(prompt) '
