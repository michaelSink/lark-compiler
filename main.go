package main

import (
	"lark/repl"
	"os"
)

func main() {
	repl.Start(os.Stdin, os.Stdout)
}
