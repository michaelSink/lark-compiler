package parser

import (
	"fmt"
	"lark/ast"
	"strings"
)

var traceLevel int = 0

const traceIdentPlaceholder string = "\t"

func identLevel() string {
	return strings.Repeat(traceIdentPlaceholder, traceLevel-1)
}

func tracePrint(fs string) {
	//fmt.Printf("%s%s\n", identLevel(), fs)
}

func incIdent() { traceLevel = traceLevel + 1 }
func decIdent() { traceLevel = traceLevel - 1 }

func traceExpression(msg string, expr ast.Expression) {
	//fmt.Printf("%s%s - %s\n", identLevel(), msg, expr)
}

func TraceNode(node ast.Node) {
	fmt.Printf("\n%t - %s", node, node)
}

func trace(msg string) string {
	incIdent()
	tracePrint("BEGIN " + msg)
	return msg
}

func untrace(msg string) {
	tracePrint("END " + msg)
	decIdent()
}
