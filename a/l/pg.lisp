
(ql:quickload 'postmodern)

(postmodern:connect-toplevel "knannuru" "knannuru" "" "k")

(postmodern:query "select * from test")


(postmodern:disconnect-toplevel)

