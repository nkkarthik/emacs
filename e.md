# emacs

  make -f e.mk

  src/emacs --batch --eval '(message system-configuration-features)'

## mac

  make -f e.mk brew-bin
  make -f e.mk launch

## ubuntu

  make -f e.mk local-bin
  make -f e.mk system

    systemctl --user status e.service
    journalctl --user -xeu e.service


## old

    docker compose ps
    docker compose logs
    docker compose exec e bash
    docker compose restart

    docker compose build

    docker compose up --build -d