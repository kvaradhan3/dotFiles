# dotFiles

The configure script tries to install pretty much everything it can.
`install`ing is basically making a link from your home directory to
the files here.   That way, if you have changes, you can then save 
them back into git automatically.

If the installer finds an existing file, it will move it to .ORIG 
before installing this version.

Assumptions:
- zsh
- for linux, arch + wayland
