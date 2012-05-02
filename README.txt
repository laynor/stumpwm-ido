This is still very buggy, and pathname completion needs a rewriting, which can involve a refactoring of the
completions-set stuff.

Requirements:
pstrings
fad
alexandria

To try it, install the requirements, clone the repo where asdf can find it and just asdf:load-system "stumpwm-ido".

Two test commands are provided:
prova2: test command for pathname completion
prova3: test command for stumpwm commands completion.
