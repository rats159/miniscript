#+featuredynamic-literals
package main

import "base:runtime"
import "core:fmt"
import "core:mem"

TokenType :: enum {
	Invalid_Token,
	//
	Integer_Literal,
	Float_Literal,
	String_Literal,
	True,
	False,
	//
	Plus,
	Minus,
	Star,
	Slash,
	PlusEquals,
	MinusEquals,
	StarEquals,
	SlashEquals,
	Equals,
	Less,
	Greater,
	LessEquals,
	GreaterEquals,
	//
	Open_Paren,
	Close_Paren,
	Open_Curly,
	Close_Curly,
	Open_Bracket,
	Close_Bracket,
	Semicolon,
	Comma,
	//
	Var,
	If,
	Else,
	Function,
	Return,
	//
	Identifier,
}

Token :: struct {
	type:  TokenType,
	start: int,
	len:   int,
	str:   string, // Just source[start:len]
	row:   int,
	col:   int,
}

Tokenizer_Error_Type :: enum {
	Unexpected_EOF,
	Unknown_Character,
	Unclosed_String
}

Tokenizer_Error :: struct {
	type:    Tokenizer_Error_Type,
	row:     int,
	col:     int,
	message: string,
	char:    u8,
}

Tokenizer :: struct {
	source: string,
	index:  int,
	tokens: [dynamic]Token,
	row:    int,
	col:    int,
}

keywords := map[string]TokenType {
	"var"   = .Var,
	"true"  = .True,
	"false" = .False,
	"if"    = .If,
	"else"  = .Else,
	"function"  = .Function,
	"return"  = .Return,
}

tokenize :: proc(code: string) -> (_val: []Token, _err: Maybe(Tokenizer_Error)) {

	tokenizer := Tokenizer {
		source = code,
		tokens = [dynamic]Token{},
	}
	tokenizer.tokens.allocator = runtime.default_allocator()

	// Temporary to make sure I'm aware of every allocation
	context.allocator = mem.panic_allocator()

	for !done(&tokenizer) {
		cc := current_char(&tokenizer) or_return

		if is_number(cc) {
			emit_number(&tokenizer) or_return
			continue
		}

		if cc == '.' && is_number(peek_token(&tokenizer, 1) or_return) {
			emit_number(&tokenizer) or_return
			continue
		}

		if is_name_start(cc) {
			emit_named(&tokenizer) or_return // Named == Keyword or Identifier
			continue
		}

		switch cc {
		case ' ', '\t', '\r', '\n':
			advance_token(&tokenizer) or_return
		case '"':
			emit_string(&tokenizer) or_return
		case '+':
			emit_token_or_postfixed(&tokenizer, .Plus, '=', .PlusEquals) or_return
		case '-':
			emit_token_or_postfixed(&tokenizer, .Minus, '=', .MinusEquals) or_return
		case '*':
			emit_token_or_postfixed(&tokenizer, .Star, '=', .StarEquals) or_return
		case '/':
			emit_token_or_postfixed(&tokenizer, .Slash, '=', .SlashEquals) or_return
		case '>':
			emit_token_or_postfixed(&tokenizer, .Greater, '=', .GreaterEquals) or_return
		case '<':
			emit_token_or_postfixed(&tokenizer, .Less, '=', .LessEquals) or_return
		case '(':
			emit_token(&tokenizer, .Open_Paren) or_return
		case ')':
			emit_token(&tokenizer, .Close_Paren) or_return
		case '{':
			emit_token(&tokenizer, .Open_Curly) or_return
		case '}':
			emit_token(&tokenizer, .Close_Curly) or_return
		case '[':
			emit_token(&tokenizer, .Open_Bracket) or_return
		case ']':
			emit_token(&tokenizer, .Close_Bracket) or_return
		case ';':
			emit_token(&tokenizer, .Semicolon) or_return
		case ',':
			emit_token(&tokenizer, .Comma) or_return
		case '=':
			emit_token(&tokenizer, .Equals) or_return
		case:
			fmt.printfln("Unknown char '%c'", cc)
			return {}, Tokenizer_Error{type = .Unknown_Character, row = tokenizer.row + 1, col = tokenizer.col + 1, message = "Unknown character", char = cc}
		}
	}

	return tokenizer.tokens[:], nil
}

@(private = "file")
is_name_start :: proc(char: u8) -> bool {
	return char >= 'a' && char <= 'z' || char >= 'A' && char <= 'Z' || char == '_'
}

@(private = "file")
is_name_part :: proc(char: u8) -> bool {
	return is_name_start(char) || is_number(char)
}

@(private = "file")
is_number :: proc(char: u8) -> bool {
	return char >= '0' && char <= '9'
}

@(private = "file")
emit_named :: proc(tokenizer: ^Tokenizer) -> Maybe(Tokenizer_Error) {
	start := tokenizer.index
	start_row := tokenizer.row
	start_col := tokenizer.col

	for !done(tokenizer) && is_name_part(current_char(tokenizer) or_return) {
		advance_token(tokenizer)
	}

	len := tokenizer.index - start

	name := tokenizer.source[start:start + len]

	token := Token {
		start = start,
		len   = len,
		type  = .Identifier,
		str   = name,
		row   = start_row,
		col   = start_col,
	}

	kwd, is_kwd := keywords[name]

	if is_kwd {
		token.type = kwd
	}

	append(&tokenizer.tokens, token)
	return nil
}

@(private = "file")
emit_string :: proc(tokenizer: ^Tokenizer) -> Maybe(Tokenizer_Error) {
	advance_token(tokenizer) or_return
	start := tokenizer.index
	start_row := tokenizer.row
	start_col := tokenizer.col

	for !done(tokenizer) && (peek_token(tokenizer, 0) or_return) != '"' { 
		advance_token(tokenizer) or_return
	}

	_, maybe_err := advance_token(tokenizer)

	if err, notok := maybe_err.(Tokenizer_Error); notok && err.type == .Unexpected_EOF {
		err.type = .Unclosed_String
		err.message = "Unclosed String"
		err.char = '"'
		err.col = start_col
		err.row = start_row
		return err
	}

	len := tokenizer.index - start - 1

	name := tokenizer.source[start:start + len]

	token := Token {
		start = start,
		len   = len,
		type  = .String_Literal,
		str   = name,
		row   = start_row,
		col   = start_col,
	}

	append(&tokenizer.tokens, token)
	return nil
}

@(private = "file")
emit_number :: proc(tokenizer: ^Tokenizer) -> Maybe(Tokenizer_Error) {
	start := tokenizer.index
	start_row := tokenizer.row
	start_col := tokenizer.col

	float := false

	for !done(tokenizer) && is_number(current_char(tokenizer) or_return) {
		advance_token(tokenizer)
	}

	if (current_char(tokenizer) or_return) == '.' {
		advance_token(tokenizer)
		float = true
	}

	for !done(tokenizer) && is_number(current_char(tokenizer) or_return) {
		advance_token(tokenizer)
	}

	len := tokenizer.index - start

	token := Token {
		start = start,
		len   = len,
		type  = float ? .Float_Literal : .Integer_Literal,
		str   = tokenizer.source[start:start + len],
		row   = start_row,
		col   = start_col,
	}

	append(&tokenizer.tokens, token)
	return nil
}

@(private = "file")
emit_token :: proc(tokenizer: ^Tokenizer, type: TokenType) -> Maybe(Tokenizer_Error) {
	append(&tokenizer.tokens, make_token(tokenizer, type))
	advance_token(tokenizer) or_return
	return nil
}

@(private = "file")
emit_token_or_postfixed :: proc(
	tokenizer: ^Tokenizer,
	type: TokenType,
	postfix: u8,
	alt_type: TokenType,
) -> Maybe(Tokenizer_Error) {
	if (peek_token(tokenizer, 1) or_return) == postfix {
		append(&tokenizer.tokens, make_token(tokenizer, alt_type, 2))
		advance_token(tokenizer) or_return
		advance_token(tokenizer) or_return
		return nil
	}

	append(&tokenizer.tokens, make_token(tokenizer, type))
	advance_token(tokenizer) or_return
	return nil
}

@(private = "file")
make_token :: proc(tokenizer: ^Tokenizer, type: TokenType, len := 1) -> Token {
	return Token {
		type = type,
		len = len,
		start = tokenizer.index,
		str = tokenizer.source[tokenizer.index:tokenizer.index + len],
		row = tokenizer.row,
		col = tokenizer.col,
	}
}

@(private = "file")
current_char :: proc(tokenizer: ^Tokenizer) -> (u8, Maybe(Tokenizer_Error)) {
	if done(tokenizer) {
		return {}, Tokenizer_Error{type = .Unexpected_EOF, row = tokenizer.row, col = tokenizer.col, message = "Unexpected end of file"}
	} 
	return tokenizer.source[tokenizer.index], nil
}

peek_token :: proc(tokenizer: ^Tokenizer, distance: int) -> (u8, Maybe(Tokenizer_Error)) {
	if tokenizer.index + distance >= len(tokenizer.source) {
		return {}, Tokenizer_Error{type = .Unexpected_EOF, row = tokenizer.row, col = tokenizer.col, message = "Tried to peek past end of file"}
	}
	return tokenizer.source[tokenizer.index + distance], nil
}

@(private = "file") 
advance_token :: proc(tokenizer: ^Tokenizer) -> (char: u8, err: Maybe(Tokenizer_Error)) {
	if tokenizer.index >= len(tokenizer.source) {
		return {}, Tokenizer_Error{type = .Unexpected_EOF, row = tokenizer.row, col = tokenizer.col, message = "Unexpected end of file"}
	}

	tk := tokenizer.source[tokenizer.index]

	if (current_char(tokenizer) or_return) == '\n' {
		tokenizer.col = 0
		tokenizer.row += 1
	} else {
		tokenizer.col += 1
	}
	tokenizer.index += 1

	return tk, nil
}

@(private = "file")
done :: proc(tokenizer: ^Tokenizer) -> bool {
	return tokenizer.index >= len(tokenizer.source)
}

@(private = "file")
is_complex :: proc(type: TokenType) -> bool {
	return type == .Integer_Literal || type == .Float_Literal || type == .Identifier || type == .String_Literal
}

print_tokens :: proc(tokens: []Token) {
	for token in tokens do print_token(token)
}

print_token :: proc(token: Token) {
	fmt.printf("%s", token.type)
	if is_complex(token.type) {
		fmt.printf("[%s]", token.str)
	}

	fmt.printfln(" %d:%d", token.row + 1, token.col + 1)
}

print_tokenizer_error :: proc(error: Tokenizer_Error) {
	fmt.printfln(
		"Tokenizer Error: %s\n  %c\n  At %d:%d",
		error.message,
		error.char,
		error.row,
		error.col,
	)
}
