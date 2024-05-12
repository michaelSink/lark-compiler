package lexer

import (
	"lark/token"
)

type Lexer struct {
	input    string
	position int
	ch       byte
}

func New(input string) *Lexer {
	l := &Lexer{input: input}
	if len(input) > 0 {
		l.ch = input[0]
	}
	return l
}

func (lexer *Lexer) readChar() {
	lexer.position += 1
	if lexer.position >= len(lexer.input) {
		lexer.ch = 0
	} else {
		lexer.ch = lexer.input[lexer.position]
	}
}

func (lexer *Lexer) peek() byte {
	if lexer.position+1 >= len(lexer.input) {
		return 0
	}
	return lexer.input[lexer.position+1]
}

func (lexer *Lexer) NextToken() token.Token {
	lexer.eatWhiteSpace()

	switch {
	case lexer.ch == 0:
		return token.Token{Type: token.EOF, Literal: ""}
	case isLetter(lexer.ch):
		identifier := lexer.readIdentifier()
		return token.Token{Type: token.FindIdentifier(identifier), Literal: identifier}
	case isDigit(lexer.ch):
		return token.Token{Type: token.INT, Literal: lexer.readIntegerAsString()}
	case lexer.ch == '"':
		defer lexer.readChar()
		return token.Token{Type: token.STRING, Literal: lexer.readString()}
	}

	nextTokenType, tokenFound := token.ByteToToken[lexer.ch]

	if !tokenFound {
		return token.Token{Type: token.ILLEGAL, Literal: string(lexer.ch)}
	}

	if lexer.peek() == token.TokenTypeToByte[token.ASSIGN] {
		if nextTokenType == token.ASSIGN {
			nextTokenType = token.EQ
			lexer.readChar()
		} else if nextTokenType == token.BANG {
			nextTokenType = token.NOT_EQ
			lexer.readChar()
		}
	}

	lexer.readChar()
	return token.Token{Type: nextTokenType, Literal: string(nextTokenType)}
}

func (lexer *Lexer) eatWhiteSpace() {
	for lexer.ch == ' ' || lexer.ch == '\t' || lexer.ch == '\n' || lexer.ch == '\r' {
		lexer.readChar()
	}
}

func (lexer *Lexer) readIntegerAsString() string {
	digitStartPosition := lexer.position
	for isDigit(lexer.ch) {
		lexer.readChar()
	}
	return lexer.input[digitStartPosition:lexer.position]
}

func (lexer *Lexer) readIdentifier() string {
	identifierStartingPosition := lexer.position
	for isLetter(lexer.ch) {
		lexer.readChar()
	}
	return lexer.input[identifierStartingPosition:lexer.position]
}

func (lexer *Lexer) readString() string {
	identifierStartingPosition := lexer.position + 1
	for {
		lexer.readChar()
		if lexer.ch == '"' || lexer.ch == 0 {
			break
		}
	}
	return lexer.input[identifierStartingPosition:lexer.position]
}

func isLetter(character byte) bool {
	return 'a' <= character && character <= 'z' ||
		'A' <= character && character <= 'Z' ||
		character == '_'
}

func isDigit(character byte) bool {
	return '0' <= character && character <= '9'
}

func newToken(tokenType token.TokenType, ch byte) token.Token {
	return token.Token{Type: tokenType, Literal: string(ch)}
}
