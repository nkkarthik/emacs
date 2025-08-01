package main

import (
	"crypto/tls"
	"fmt"
	"io"
	"log"
	"net"
	"net/http"
	"net/http/httptest"
	"net/http/httputil"
	"net/url"
	"time"

)

func main() {

	target, _ := url.Parse("http://127.0.0.1:30001")

	// Load TLS certificate and key
    cert, err := tls.LoadX509KeyPair("self.crt", "self.key")
    if err != nil {
        log.Fatal("LoadX509KeyPair error:", err)
    }
    tlsConfig := &tls.Config{
        Certificates: []tls.Certificate{cert},
        MinVersion:   tls.VersionTLS12,
    }

	proxy := httputil.NewSingleHostReverseProxy(&url.URL{Scheme: target.Scheme, Host: target.Host})
	proxy.ErrorHandler = responder{}.Error

	hdl := http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		// log.Printf("URL Path %q", r.URL.Path)
		if hj, ok := w.(http.Hijacker); ok {
			if err := ws(hj, r, target); err != nil {
				http.Error(w, "Error contacting backend target", 500)
				log.Printf("Error dialing websocket %s: %v", target, err)
			}
			return
		}
		proxy.ServeHTTP(w, r)
	})

    server := &http.Server{
        Addr:      ":443",
        Handler:   hdl,
        TLSConfig: tlsConfig,
		WriteTimeout: 120 * time.Second,
		ReadTimeout:  120 * time.Second,
    }

    log.Println("Starting tls proxy on :443")
    log.Fatal(server.ListenAndServeTLS("", ""))
}

func ws(hj http.Hijacker, r *http.Request, target *url.URL) error {
	d, err := net.Dial("tcp", target.Host)
	if err != nil {
		return err
	}
	nc, _, err := hj.Hijack()
	if err != nil {
		log.Printf("Hijack error: %v", err)
		return err
	}
	defer nc.Close()
	defer d.Close()

	err = r.Write(d)
	if err != nil {
		log.Printf("Error copying request to target: %v", err)
		return err
	}

	errc := make(chan error, 2)
	cp := func(dst io.Writer, src io.Reader) {
		_, err := io.Copy(dst, src)
		errc <- err
	}
	go cp(d, nc)
	go cp(nc, d)
	<-errc
	return nil
}

func logHandler(fn http.Handler) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		x, err := httputil.DumpRequest(r, true)
		if err != nil {
			http.Error(w, fmt.Sprint(err), http.StatusInternalServerError)
			return
		}
		log.Println(fmt.Sprintf("%q", x))
		rec := httptest.NewRecorder()

		fn.ServeHTTP(rec, r)

		// %q	?
		log.Println(fmt.Sprintf("Headers: %q", rec.HeaderMap))
		log.Println(fmt.Sprintf("Return Code %v", rec.Code))
		log.Println(fmt.Sprintf("Flushed? %v", rec.Flushed))
		// log.Println(fmt.Sprintf("%s", rec.Body))

		// this copies the recorded response to the response writer
		for k, v := range rec.HeaderMap {
			w.Header()[k] = v
		}
		w.WriteHeader(rec.Code)
		rec.Body.WriteTo(w)
	}
}

type responder struct{}

func (r responder) Error(w http.ResponseWriter, req *http.Request, err error) {
	log.Printf("failed request %v %v", req.URL, err)
}
