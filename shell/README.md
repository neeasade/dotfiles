## shell

My shell prompt consists of a single '$', with foreground and background colors inverted if the exit status of the last command is not 0. if there are existing background processes, a second '$' is printed. The root prompt is the same, only with '#'. The ~/.profile file contains definitions for default applications defined with variables ($EDITOR, $BROWSER etc), along with config for the history file.
