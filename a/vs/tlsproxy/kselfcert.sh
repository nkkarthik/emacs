#!/bin/bash

openssl req -x509 -nodes -days 3650 \
	-newkey rsa:2048 \
	-keyout self.key -out self.crt \
	-subj "/CN=k" \
	-addext "subjectAltName=DNS:localhost,DNS:k,DNS:kws,DNS:10.12.35.45"


