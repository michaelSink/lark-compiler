package token

type TokenType string

type Token struct {
	Type    TokenType
	Literal string
}

const (
	EQ     = "=="
	NOT_EQ = "!="

	LT = "<"
	GT = ">"

	LBRACE = "{"
	RBRACE = "}"

	LPAREN = "("
	RPAREN = ")"

	ASSIGN   = "="
	ASTERISK = "*"
	PLUS     = "+"
	BANG     = "!"
	MINUS    = "-"
	SLASH    = "/"

	SEMICOLON = ";"
	COMMA     = ","
	COLON     = ":"

	LBRACKET = "["
	RBRACKET = "]"

	INT    = "INT"
	IDENT  = "IDENT"
	STRING = "STRING"

	LET      = "LET"
	FUNCTION = "FUNCTION"
	IF       = "IF"
	RETURN   = "RETURN"
	TRUE     = "TRUE"
	ELSE     = "ELSE"
	FALSE    = "FALSE"

	EOF     = "EOF"
	ILLEGAL = "ILLEGAL"
)

var TokenTypeToByte = map[string]byte{
	LT:        '<',
	GT:        '>',
	LBRACE:    '{',
	RBRACE:    '}',
	LPAREN:    '(',
	RPAREN:    ')',
	ASSIGN:    '=',
	ASTERISK:  '*',
	PLUS:      '+',
	BANG:      '!',
	MINUS:     '-',
	SLASH:     '/',
	SEMICOLON: ';',
	COMMA:     ',',
}

var ByteToToken = map[byte]TokenType{
	'<': LT,
	'>': GT,
	'{': LBRACE,
	'}': RBRACE,
	'(': LPAREN,
	')': RPAREN,
	'=': ASSIGN,
	'*': ASTERISK,
	'+': PLUS,
	'!': BANG,
	'-': MINUS,
	'/': SLASH,
	';': SEMICOLON,
	',': COMMA,
	'[': LBRACKET,
	']': RBRACKET,
	':': COLON,
}

var keywords = map[string]TokenType{
	"fn":     FUNCTION,
	"let":    LET,
	"true":   TRUE,
	"false":  FALSE,
	"if":     IF,
	"else":   ELSE,
	"return": RETURN,
}

func FindIdentifier(ident string) TokenType {
	keyword, found := keywords[ident]
	if !found {
		keyword = IDENT
	}
	return keyword
}
