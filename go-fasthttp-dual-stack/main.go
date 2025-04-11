package main

import (
	"fmt"
	"time"

	"github.com/valyala/fasthttp"
)

const address = "http://localhost:8000"

func main() {
	client := fasthttp.Client{DialDualStack: true}
	es := make(chan error)

	go func() {
		for {
			_, _, err := client.Get([]byte{}, address)
			es <- err

			time.Sleep(time.Second)
		}
	}()

	for e := range es {
		fmt.Println(e)
	}

	// Now, we see the following. But why?
	//
	// ```
	// <nil>
	// error when dialing [::1]:8000: dial tcp [::1]:8000: connect: connection refused
	// <nil>
	// error when dialing [::1]:8000: dial tcp [::1]:8000: connect: connection refused
	// ...
	// ```
}
