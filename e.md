#+title: e

#+begin_src elisp
  (1+ 3)
#+end_src

#+RESULTS:
: 4


* build

#+begin_src sh
  make -f e.mk
#+end_src

#+RESULTS:

#+begin_src sh
  src/emacs --batch --eval '(message system-configuration-features)'
#+end_src

** mac

#+begin_src sh
  make -f e.mk brew-bin
  make -f e.mk launch
#+end_src

** ubuntu

#+begin_src sh
  make -f e.mk local-bin
  make -f e.mk system
#+end_src

#+begin_src sh
    systemctl --user status e.service
    journalctl --user -xeu e.service
#+end_src



* docker

    docker compose ps
    docker compose logs
    docker compose exec e bash
    docker compose restart

    docker compose build

    docker compose up --build -d
