package repl

import (
	"bufio"
	"fmt"
	"io"
	"lark/compiler"
	"lark/lexer"
	"lark/object"
	"lark/parser"
	"lark/vm"
)

const PROMPT = ">>"

func Start(in io.Reader, out io.Writer) {
	scanner := bufio.NewScanner(in)

	constants := []object.Object{}
	globals := make([]object.Object, vm.GLOBAL_SIZE)

	symbolTable := compiler.NewSymbolTable()

	for i, v := range object.Builtins {
		symbolTable.DefineBuiltin(i, v.Name)
	}

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

		compiler := compiler.NewWithState(symbolTable, constants)
		err := compiler.Compile(program)
		if err != nil {
			fmt.Fprintf(out, "Compilation failed: \n %s\n", err)
			continue
		}

		code := compiler.Bytecode()
		constants = code.Constants

		vm := vm.NewWithGlobalStore(code, globals)
		err = vm.Run()
		if err != nil {
			fmt.Fprintf(out, "Executing bytecode failed: \n %s\n", err)
			continue
		}

		lastPopped := vm.LastPoppedStackElem()
		io.WriteString(out, lastPopped.Inspect())
		io.WriteString(out, "\n")
	}
}

func printParseErrors(out io.Writer, errors []string) {
	for _, msg := range errors {
		io.WriteString(out, "\t"+msg+"\n")
	}
}
