# sdlmapper
### _Feuer <feuer@feuerx.net>_

This is a port of my old qmapper repository without c++, ecl, qt and the bare opengl interfacing, instead we're running sbcl + sdl + a socket for interfacing with emacs. Should be loadable by pushing this directory to asdf:\*central-registry\* and quickloading (ql:quickload :cl-opengl-test). System name will change when this project is mature enough
