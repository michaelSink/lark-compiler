package repl

import (
	"bufio"
	"fmt"
	"io"
	"lark/evaluator"
	"lark/lexer"
	"lark/object"
	"lark/parser"
)

const PROMPT = ">>"

func Start(in io.Reader, out io.Writer) {
	scanner := bufio.NewScanner(in)
	env := object.NewEnvironment()

	for {
		fmt.Fprintf(out, PROMPT)

		scanned := scanner.Scan()
		if !scanned {
			return
		}

		line := scanner.Text()
		lexer := lexer.New(line)
		parser := parser.New(lexer)

		program := parser.ParseProgram()

		if len(parser.Errors()) > 0 {
			printParseErrors(out, parser.Errors())
			continue
		}

		eval := evaluator.Eval(program, env)

		if eval != nil {
			io.WriteString(out, eval.Inspect())
			io.WriteString(out, "\n")
		}
	}
}

func printParseErrors(out io.Writer, errors []string) {
	for _, msg := range errors {
		io.WriteString(out, "\t"+msg+"\n")
	}
}
