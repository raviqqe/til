package main

import (
	"fmt"
	"time"

	"github.com/valyala/fasthttp"
)

const concurrency = 1
const address = "http://localhost:8000"

func main() {
	client := fasthttp.Client{DialDualStack: true}
	cs := make(chan struct{}, concurrency)
	es := make(chan error)

	go func() {
		for {
			cs <- struct{}{}

			go func() {
				bs := []byte{}
				_, _, err := client.Get(bs, address)
				es <- err
				<-cs
			}()

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
