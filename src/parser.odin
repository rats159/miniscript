package main

import "core:fmt"
import "core:mem"
import "core:strconv"

Parser_Error_Type :: enum {
	Unexpected_EOF,
	Unexpected_Token,
	Invalid_Body,
}

Parser_Error :: struct {
	type:    Parser_Error_Type,
	message: string,
	token:   Token,
}

Program_Node :: struct {
	body: []^Expression_Node,
}

Collection_Literal_Node :: struct {
	body: []^Expression_Node,
}

Subscript_Node :: struct {
	operand: ^Expression_Node,
	indexer: ^Expression_Node,
}

Expression_Node :: union {
	//
	Add_Node,
	Multiply_Node,
	Subtract_Node,
	Divide_Node,
	Negation_Node,
	Less_Node,
	Greater_Node,
	Less_Equals_Node,
	Greater_Equals_Node,
	Equality_Node,
	//
	Integer_Node,
	Float_Node,
	Bool_Node,
	String_Node,
	Collection_Literal_Node,
	//
	Variable_Read_Node,
	Call_Node,
	Subscript_Node,
	//
	Variable_Node,
	Return_Node,
	Block,
	Function_Node,
	If_Node,
	Assignment_Node,
}

Binary_Op_Node :: struct {
	left:  ^Expression_Node,
	right: ^Expression_Node,
}

Unary_Op_Node :: struct {
	operand: ^Expression_Node,
}

Multiply_Node :: distinct Binary_Op_Node
Subtract_Node :: distinct Binary_Op_Node
Add_Node :: distinct Binary_Op_Node
Negation_Node :: distinct Unary_Op_Node
Divide_Node :: distinct Binary_Op_Node
Less_Node :: distinct Binary_Op_Node
Greater_Node :: distinct Binary_Op_Node
Less_Equals_Node :: distinct Binary_Op_Node
Greater_Equals_Node :: distinct Binary_Op_Node
Equality_Node :: distinct Binary_Op_Node


Integer_Node :: struct {
	value: i64,
}

Float_Node :: struct {
	value: f64,
}

Bool_Node :: struct {
	value: bool,
}

String_Node :: struct {
	value: string,
}

Variable_Read_Node :: struct {
	name: string,
}

Call_Node :: struct {
	callee:    ^Expression_Node,
	arguments: []^Expression_Node,
}

Return_Node :: struct {
	value: ^Expression_Node,
}

Block :: struct {
	body: []^Expression_Node,
}

Function_Node :: struct {
	name:       Token,
	parameters: []Token,
	body:       []^Expression_Node,
}

Variable_Node :: struct {
	name:  string,
	value: ^Expression_Node,
}

Assignment_Node :: struct {
	assignee:  ^Expression_Node,
	value: ^Expression_Node,
}

If_Node :: struct {
	condition: ^Expression_Node,
	body:      ^Expression_Node,
	else_body: Maybe(^Expression_Node),
}


Parser :: struct {
	tokens:    []Token,
	index:     int,
	root:      Program_Node,
	allocator: mem.Allocator,
}


parse :: proc(tokens: []Token) -> (_parser: Parser, _err: Maybe(Parser_Error)) {
	arena := mem.Dynamic_Arena{}
	mem.dynamic_arena_init(&arena)
	context.allocator = mem.dynamic_arena_allocator(&arena)

	parser := Parser {
		allocator = context.allocator,
		tokens    = tokens,
	}


	expr := parse_program(&parser) or_return
	parser.root = Program_Node {
		body = expr,
	}

	return parser, nil
}

@(private = "file")
parse_program :: proc(parser: ^Parser) -> (_expr: []^Expression_Node, _err: Maybe(Parser_Error)) {
	exprs := [dynamic]^Expression_Node{}

	for !parser_done(parser) {
		append(&exprs, parse_expression(parser) or_return)
		_ = expect(parser, .Semicolon) or_return
	}

	return exprs[:], nil
}

// @(private = "file")
// parse_statement :: proc(parser: ^Parser) -> (_expr: ^Expression_Node, _err: Maybe(Parser_Error)) {

// 	return nil, Parser_Error {
// 		type = .Not_A_Statement,
// 		token = peek(parser) or_return,
// 		message = fmt.tprintf(
// 			"Token '%s' does not begin a statement",
// 			(peek(parser) or_return).str,
// 		),
// 	}
// }

parse_return :: proc(parser: ^Parser) -> (_stmt: ^Expression_Node, _err: Maybe(Parser_Error)) {
	expr := parse_expression(parser) or_return
	node := new(Expression_Node)
	node^ = Return_Node {
		value = expr,
	}

	return node, nil
}

parse_block :: proc(parser: ^Parser) -> (_stmt: ^Expression_Node, _err: Maybe(Parser_Error)) {
	body := [dynamic]^Expression_Node{}
	
	for !(match(parser, .Close_Curly) or_return) {
		append(&body, parse_expression(parser) or_return)
		_ = expect(parser, .Semicolon) or_return
	}

	node := new(Expression_Node)
	node^ = Block {
		body = body[:],
	}

	return node, nil
}

parse_function :: proc(parser: ^Parser) -> (_stmt: ^Expression_Node, _err: Maybe(Parser_Error)) {
	name := expect(parser, .Identifier) or_return
	_ = expect(parser, .Open_Paren) or_return

	parameters := [dynamic]Token{}

	if (peek(parser) or_return).type != .Close_Paren {
		for {
			append(&parameters, expect(parser, .Identifier) or_return)

			if !(match(parser, .Comma) or_return) {
				break
			}
		}
	}

	_ = expect(parser, .Close_Paren) or_return
	_ = expect(parser, .Open_Curly) or_return

	body := [dynamic]^Expression_Node{}

	for !(match(parser, .Close_Curly) or_return) {
		append(&body, parse_expression(parser) or_return)
		_ = expect(parser, .Semicolon) or_return
	}

	node := new(Expression_Node)
	node^ = Function_Node {
		name       = name,
		parameters = parameters[:],
		body       = body[:],
	}

	return node, nil
}

parse_if :: proc(parser: ^Parser) -> (_stmt: ^Expression_Node, _err: Maybe(Parser_Error)) {
	condition := parse_expression(parser) or_return
	next := peek(parser) or_return
	if !is_valid_after_if(next.type) {
		return {}, Parser_Error{type = .Invalid_Body, token = next, message = fmt.aprintf("%s is not a valid if expression body", next.type)}
	}

	body := parse_expression(parser) or_return

	else_body: Maybe(^Expression_Node) = nil
	if match(parser, .Else) or_return {
		else_body = parse_expression(parser) or_return
	}

	stmt := new(Expression_Node)
	stmt^ = If_Node {
		condition = condition,
		body      = body,
		else_body = else_body,
	}

	return stmt, nil
}

is_valid_after_if :: proc(type: TokenType) -> bool {
	return type == .Open_Curly || type == .Return
}

@(private = "file")
parse_variable :: proc(parser: ^Parser) -> (_stmt: ^Expression_Node, _err: Maybe(Parser_Error)) {
	name := (expect(parser, .Identifier) or_return).str
	_ = expect(parser, .Equals) or_return
	expr := parse_expression(parser) or_return

	stmt := new(Expression_Node)
	stmt^ = Variable_Node {
		name  = name,
		value = expr,
	}

	return stmt, nil
}

@(private = "file")
parse_standard_assignment :: proc(
	assignee: ^Expression_Node,
	parser: ^Parser,
) -> (
	_stmt: ^Expression_Node,
	_err: Maybe(Parser_Error),
) {
	_ = expect(parser, .Equals) or_return
	expr := parse_expression(parser) or_return

	stmt := new(Expression_Node)
	stmt^ = Assignment_Node {
		assignee  = assignee,
		value = expr,
	}

	return stmt, nil
}


@(private = "file")
parse_operator_assignment :: proc(
	assignee: ^Expression_Node,
	parser: ^Parser,
	type: TokenType,
) -> (
	_stmt: ^Expression_Node,
	_err: Maybe(Parser_Error),
) {
	advance(parser) or_return
	expr := parse_expression(parser) or_return

	operation := new(Expression_Node)

	#partial switch type {
	case .PlusEquals:
		operation^ = Add_Node {
			left  = assignee,
			right = expr,
		}
	case .MinusEquals:
		operation^ = Subtract_Node {
			left  = assignee,
			right = expr,
		}
	case .StarEquals:
		operation^ = Multiply_Node {
			left  = assignee,
			right = expr,
		}
	case .SlashEquals:
		operation^ = Divide_Node {
			left  = assignee,
			right = expr,
		}
	case:
		panic("Theoretically unreachable operator assignment")
	}
 
	stmt := new(Expression_Node)
	stmt^ = Assignment_Node {
		assignee  = assignee,
		value = operation,
	}

	return stmt, nil
}

@(private = "file")
parse_expression :: proc(parser: ^Parser) -> (_expr: ^Expression_Node, _err: Maybe(Parser_Error)) {
	if match(parser, .Var) or_return {
		return parse_variable(parser)
	}

	if match(parser, .Function) or_return {
		return parse_function(parser)
	}

	if match(parser, .If) or_return {
		return parse_if(parser)
	}

	if match(parser, .Open_Curly) or_return {
		return parse_block(parser)
	}

	if match(parser, .Return) or_return {
		return parse_return(parser)
	}
	if match(parser, .Open_Bracket) or_return {
		return parse_collection(parser)
	}
	expr := parse_assignment(parser) or_return 

	return expr, nil
}

@(private = "file")
parse_assignment :: proc(parser: ^Parser) -> (_expr: ^Expression_Node, _err: Maybe(Parser_Error)) {
	assignee := parse_comparison(parser) or_return

	tk := peek(parser) or_return
	
	#partial switch tk.type {
	case .Equals:
		return parse_standard_assignment(assignee, parser)
	case .PlusEquals, .MinusEquals, .StarEquals, .SlashEquals:
		return parse_operator_assignment(assignee, parser, tk.type)
	case:
		return assignee, nil
	}
}

@(private = "file")
parse_collection :: proc(parser: ^Parser) -> (_expr: ^Expression_Node, _err: Maybe(Parser_Error)) {
	body: [dynamic]^Expression_Node
	if (peek(parser, 0) or_return).type != .Close_Bracket {
		for {
			append(&body, parse_expression(parser) or_return)
			if !(match(parser, .Comma) or_return) do break
		}
	}

	_ = expect(parser, .Close_Bracket) or_return
	node := new(Expression_Node)
	node^ = Collection_Literal_Node {
		body = body[:],
	}

	return node, nil
}

@(private = "file")
parse_comparison :: proc(parser: ^Parser) -> (_expr: ^Expression_Node, _err: Maybe(Parser_Error)) {

	left := parse_add(parser) or_return


	operator := peek(parser) or_return
	if operator.type != .Less &&
	   operator.type != .Greater &&
	   operator.type != .LessEquals &&
	   operator.type != .GreaterEquals &&
	   operator.type != .EqualsEquals {
		return left, nil
	}

	advance(parser) or_return

	right := parse_comparison(parser) or_return

	expr := new(Expression_Node)

	#partial switch operator.type {
	case .Less:
		expr^ = Less_Node {
			left  = left,
			right = right,
		}
	case .Greater:
		expr^ = Greater_Node {
			left  = left,
			right = right,
		}
	case .LessEquals:
		expr^ = Less_Equals_Node {
			left  = left,
			right = right,
		}
	case .GreaterEquals:
		expr^ = Greater_Equals_Node {
			left  = left,
			right = right,
		}
	case .EqualsEquals:
		expr^ = Equality_Node {
			left  = left,
			right = right,
		}
	case:
		panic("Theoretically unreachable comparison")
	}

	return expr, nil
}

@(private = "file")
parse_add :: proc(parser: ^Parser) -> (_expr: ^Expression_Node, _err: Maybe(Parser_Error)) {

	left := parse_mult(parser) or_return


	operator := peek(parser) or_return
	if operator.type != .Plus && operator.type != .Minus {
		return left, nil
	}

	advance(parser) or_return

	right := parse_add(parser) or_return

	if operator.type == .Plus {
		expr := new(Expression_Node)

		expr^ = Add_Node {
			left  = left,
			right = right,
		}

		return expr, nil
	} else if operator.type == .Minus {
		expr := new(Expression_Node)

		expr^ = Subtract_Node {
			left  = left,
			right = right,
		}

		return expr, nil
	}

	panic("Theoretically unreachable add-precedence operator")
}

@(private = "file")
parse_mult :: proc(parser: ^Parser) -> (_expr: ^Expression_Node, _err: Maybe(Parser_Error)) {

	left := parse_unary(parser) or_return

	operator := peek(parser) or_return
	if operator.type != .Star && operator.type != .Slash {
		return left, nil
	}

	advance(parser) or_return

	right := parse_mult(parser) or_return

	if operator.type == .Star {
		expr := new(Expression_Node)

		expr^ = Multiply_Node {
			left  = left,
			right = right,
		}

		return expr, nil
	} else if operator.type == .Slash {
		expr := new(Expression_Node)

		expr^ = Divide_Node {
			left  = left,
			right = right,
		}

		return expr, nil
	}

	panic("Theoretically unreachable mult-precedence operator")
}

@(private = "file")
parse_unary :: proc(parser: ^Parser) -> (_expr: ^Expression_Node, _err: Maybe(Parser_Error)) {
	potential_operator := peek(parser) or_return

	if potential_operator.type != .Minus {
		return parse_postfix(parser)
	}
	advance(parser) or_return
	operand := parse_unary(parser) or_return

	negation_node := new(Expression_Node)
	negation_node^ = Negation_Node {
		operand = operand,
	}

	return negation_node, nil
}

parse_call :: proc(
	parser: ^Parser,
	callee: ^Expression_Node,
) -> (
	_expr: ^Expression_Node,
	_err: Maybe(Parser_Error),
) {
	arguments := [dynamic]^Expression_Node{}
	if !(match(parser, .Close_Paren) or_return) do for {
		append(&arguments, parse_expression(parser) or_return)
		if !(match(parser, .Comma) or_return) {
			_ = expect(parser, .Close_Paren) or_return
			break
		}
	}
	call := new(Expression_Node)
	call^ = Call_Node {
		callee    = callee,
		arguments = arguments[:],
	}
	return call, nil
}

parse_subscript :: proc(
	parser: ^Parser,
	operand: ^Expression_Node,
) -> (
	_expr: ^Expression_Node,
	_err: Maybe(Parser_Error),
) {
	indexer := parse_expression(parser) or_return
	_ = expect(parser, .Close_Bracket) or_return
	call := new(Expression_Node)
	call^ = Subscript_Node {
		operand = operand,
		indexer = indexer,
	}
	return call, nil
}

@(private = "file")
parse_postfix :: proc(parser: ^Parser) -> (_expr: ^Expression_Node, _err: Maybe(Parser_Error)) {
	expr := parse_value(parser) or_return

	for {
		if match(parser, .Open_Paren) or_return {
			expr = parse_call(parser, expr) or_return
		} else if match(parser, .Open_Bracket) or_return {
			expr = parse_subscript(parser, expr) or_return
		} else do break
	}

	return expr, nil
}

@(private = "file")
parse_value :: proc(parser: ^Parser) -> (_expr: ^Expression_Node, _err: Maybe(Parser_Error)) {
	if match(parser, .Open_Paren) or_return {
		expr := parse_comparison(parser) or_return
		_ = expect(parser, .Close_Paren) or_return
		return expr, nil
	}

	next_type := (peek(parser) or_return).type

	if next_type == .Integer_Literal {
		return parse_int(parser)
	}

	if next_type == .Float_Literal {
		return parse_float(parser)
	}

	if next_type == .Identifier {
		return parse_var_read(parser)
	}

	if next_type == .String_Literal {
		s := expect(parser, .String_Literal) or_return
		node := new(Expression_Node)
		node^ = String_Node {
			value = s.str,
		}
		return node, nil
	}

	if next_type == .True {
		_ = expect(parser, .True) or_return

		value_node := new(Expression_Node)
		value_node^ = Bool_Node {
			value = true,
		}

		return value_node, nil
	}

	if next_type == .False {
		_ = expect(parser, .False) or_return

		value_node := new(Expression_Node)
		value_node^ = Bool_Node {
			value = false,
		}

		return value_node, nil
	}

	return nil, Parser_Error {
		type = .Unexpected_Token,
		message = fmt.aprintf("Token '%s' has no value", (peek(parser) or_return).str),
		token = peek(parser) or_return,
	}
}

@(private = "file")
parse_var_read :: proc(parser: ^Parser) -> (_expr: ^Expression_Node, _err: Maybe(Parser_Error)) {
	name := expect(parser, .Identifier) or_return

	value_node := new(Expression_Node)
	value_node^ = Variable_Read_Node {
		name = name.str,
	}

	return value_node, nil
}

@(private = "file")
parse_int :: proc(parser: ^Parser) -> (_expr: ^Expression_Node, _err: Maybe(Parser_Error)) {
	num_token := expect(parser, .Integer_Literal) or_return

	val, ok := strconv.parse_i64(num_token.str)

	fmt.assertf(
		ok,
		"Internal error: '%s' was considered a number by the lexer, but failed actual conversion.",
	)

	value_node := new(Expression_Node)
	value_node^ = Integer_Node {
		value = val,
	}

	return value_node, nil
}

@(private = "file")
parse_float :: proc(parser: ^Parser) -> (_expr: ^Expression_Node, _err: Maybe(Parser_Error)) {
	num_token := expect(parser, .Float_Literal) or_return

	val, ok := strconv.parse_f64(num_token.str)

	assert(ok, "Malformed float made it into parser")

	value_node := new(Expression_Node)
	value_node^ = Float_Node {
		value = val,
	}

	return value_node, nil
}

@(private = "file")
@(require_results)
expect :: proc(parser: ^Parser, type: TokenType) -> (_tok: Token, _err: Maybe(Parser_Error)) {
	if parser_done(parser) {
		return {}, Parser_Error{type = .Unexpected_EOF, message = fmt.aprintf("Expected %s but hit end of file", type)}
	}

	tk := advance(parser) or_return

	if tk.type != type {
		return {}, Parser_Error{type = .Unexpected_Token, message = fmt.aprintf("Expected %s but recieved %s", type, tk.type), token = tk}
	}

	return tk, nil
}


@(private = "file")
match :: proc(parser: ^Parser, type: TokenType) -> (_matched: bool, _err: Maybe(Parser_Error)) {
	tk := peek(parser) or_return

	if tk.type == type {
		advance(parser) or_return
		return true, nil
	}
	return false, nil
}

@(private = "file")
peek :: proc(parser: ^Parser, distance := 0) -> (Token, Maybe(Parser_Error)) {
	if parser.index + distance >= len(parser.tokens) {
		return {}, Parser_Error{type = .Unexpected_EOF, token = parser.tokens[parser.index - 1], message = "Unexpected end of file"}
	}

	return parser.tokens[parser.index + distance], nil
}

@(private = "file")
advance :: proc(parser: ^Parser) -> (Token, Maybe(Parser_Error)) {
	if parser_done(parser) {
		return {}, Parser_Error{type = .Unexpected_EOF, message = "Unexpected end of file"}
	}

	tk := parser.tokens[parser.index]
	parser.index += 1

	return tk, nil
}

@(private = "file")
parser_done :: proc(parser: ^Parser) -> bool {
	return parser.index >= len(parser.tokens)
}

print_parser_error :: proc(error: Parser_Error) {
	fmt.printfln(
		"Parser Error: %s\n  %s\n  At %d:%d",
		error.message,
		error.token.str,
		error.token.row + 1,
		error.token.col + 1,
	)
}
