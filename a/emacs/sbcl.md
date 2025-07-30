# sbcl

## quicklisp install

ls -l /home/knannuru/quicklisp
ls -ld /home/knannuru/quicklisp
sudo chown -R knannuru:knannuru /home/knannuru/quicklisp

curl -O https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --disable-debugger --load quicklisp.lisp \
    	 --eval '(quicklisp-quickstart:install)' \
	 --eval '(ql-util:without-prompting (ql:add-to-init-file))' \
	 --quit

