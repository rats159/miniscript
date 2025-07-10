package main

import "core:fmt"
import "core:reflect"
import "core:strings"

Function :: struct {
	arity:      int,
	definition: Function_Node,
}

Native_Function :: struct {
	arity:       int,
	native_proc: proc(
		interpreter: ^Interpreter,
		arguments: []Value,
	) -> (
		Value,
		Runtime_Propagator,
	),
}

Collection :: struct {
	body: [dynamic]Value,
}

Value :: union {
	i64,
	bool,
	f64,
	string,
	Function,
	Native_Function,
	Collection,
}

Runtime_Error_Type :: enum {
	Divide_By_Zero,
	Type_Error,
	Undeclared_Variable,
	Stack_Depth_Exceeded,
	Index_Out_Of_Bounds_Error,
	Invalid_Assignment,
	Redefinition,
}

Runtime_Return :: struct {
	value: Value,
}

Runtime_Propagator :: union {
	Runtime_Error,
	Runtime_Return,
}

Runtime_Error :: struct {
	type:    Runtime_Error_Type,
	message: string,
}

Interpreter :: struct {
	global_vars:   Environment,
	current_scope: ^Environment,
	depth:         u8,
}

Environment :: struct {
	parent:    Maybe(^Environment),
	variables: map[string]Value,
}

@require_results
define :: proc(environment: ^Environment, name: string, val: Value) -> Runtime_Propagator {
	_, defined := &environment.variables[name]

	if defined {
		return Runtime_Error {
			type = .Redefinition,
			message = fmt.tprintf("Redefinition of variable %s", name)
		}
	}

	environment.variables[name] = val
	return nil
}

set :: proc(environment: ^Environment, name: string, val: Value) -> Runtime_Propagator {
	_, found := environment.variables[name]
	if found {
		environment.variables[name] = val
		return nil
	}

	if environment.parent != nil {
		return set(environment.parent.?, name, val)
	}

	return Runtime_Error {
		type = .Undeclared_Variable
	}
}

read :: proc(environment: ^Environment, name: string) -> (Value, bool) {
	val, found := environment.variables[name]
	if found {
		return val, true
	}

	if environment.parent != nil {
		return read(environment.parent.?, name)
	}

	return nil, false
}

@(require_results)
begin_scope :: proc(interpreter: ^Interpreter) -> Runtime_Propagator {
	if interpreter.depth > 100 {
		return Runtime_Error{type = .Stack_Depth_Exceeded, message = "Stack is too deep!"}
	}
	new_scope := new(Environment)
	new_scope.variables = map[string]Value{}
	new_scope.parent = interpreter.current_scope
	interpreter.current_scope = new_scope

	interpreter.depth += 1
	return nil
}

end_scope :: proc(interpreter: ^Interpreter) {
	old_scope := interpreter.current_scope.parent.?
	delete(interpreter.current_scope.variables)
	free(interpreter.current_scope)
	interpreter.current_scope = old_scope
	interpreter.depth -= 1
}

tostring :: proc(val: Value) -> string {
	switch val in val {
	case i64:
		return fmt.aprint(val)
	case f64:
		return fmt.aprint(val)
	case string:
		return strings.clone(val)
	case bool:
		return strings.clone("true" if val else "false")
	case Function:
		return strings.clone("<function>")
	case Native_Function:
		return strings.clone("<native function>")
	case Collection:
		builder := strings.Builder{}
		strings.write_byte(&builder, '[')
		for expr, i in val.body {
			str := tostring(expr)
			strings.write_string(&builder, str)
			delete(str)
			if i < len(val.body) - 1 {
				strings.write_string(&builder, ", ")
			}
		}
		strings.write_byte(&builder, ']')
		return strings.to_string(builder)
	}
	panic("<Invalid Type>")
}

setup_natives :: proc(interpreter: ^Interpreter) -> Runtime_Propagator {
	define(&interpreter.global_vars, "print", Native_Function {
		arity = -1,
		native_proc = proc(
			interpreter: ^Interpreter,
			arguments: []Value,
		) -> (
			Value,
			Runtime_Propagator,
		) {
			for arg in arguments {
				str := tostring(arg)
				fmt.print(str)
				fmt.print(" ")
				delete(str)
			}
			fmt.println()
			return nil, nil
		},
	}) or_return

		define(&interpreter.global_vars, "tostring", Native_Function {
		arity = 1,
		native_proc = proc(
			interpreter: ^Interpreter,
			arguments: []Value,
		) -> (
			Value,
			Runtime_Propagator,
		) {
			return tostring(arguments[0]), nil
		},
	}) or_return

	return nil
}

execute :: proc(program: Program_Node) -> (_err: Runtime_Propagator) {
	interpreter: Interpreter
	globals := Environment{}
	interpreter.global_vars = globals
	interpreter.current_scope = &interpreter.global_vars

	setup_natives(&interpreter) or_return
	for expr in program.body {
		evaluate(&interpreter, expr^) or_return
	}

	delete(interpreter.global_vars.variables)
	return nil
}

evaluate_return :: proc(
	interpreter: ^Interpreter,
	node: Return_Node,
) -> (
	_val: Value,
	_prop: Runtime_Propagator,
) {
	return nil, Runtime_Return{value = evaluate(interpreter, node.value^) or_return}
}

evaluate_block :: proc(
	interpreter: ^Interpreter,
	block: Block,
) -> (
	_val: Value,
	_prop: Runtime_Propagator,
) {
	begin_scope(interpreter) or_return
	defer end_scope(interpreter)
	value: Value
	for expr in block.body {
		value = evaluate(interpreter, expr^) or_return
	}

	return value, nil
}

evaluate_function_declaration :: proc(
	interpreter: ^Interpreter,
	node: Function_Node,
) -> (
	_val: Value,
	_prop: Runtime_Propagator,
) {
	func := Function {
		arity      = len(node.parameters),
		definition = node,
	}
	define(interpreter.current_scope, node.name.str, func) or_return

	return func, nil
}

evaluate_if :: proc(
	interpreter: ^Interpreter,
	node: If_Node,
) -> (
	_val: Value,
	_prop: Runtime_Propagator,
) {
	cond := evaluate(interpreter, node.condition^) or_return

	#partial switch type in cond {
		case bool:
		case:
			return nil, Runtime_Error {
				type = .Type_Error,
				message = fmt.tprintf("Tried to use type %s in an if condition", reflect.union_variant_typeid(cond))
			}
	}

	if cond.(bool) {
		return evaluate(interpreter, node.body^)
	} else if els, ok := node.else_body.?; ok {
		return evaluate(interpreter, els^)
	}
	return nil, nil
}

evaluate_variable_decl :: proc(
	interpreter: ^Interpreter,
	node: Variable_Node,
) -> (
	_val: Value,
	_prop: Runtime_Propagator,
) {
	value := evaluate(interpreter, node.value^) or_return
	define(interpreter.current_scope, node.name, value) or_return

	return value, nil
}

evaluate_assignment :: proc(
	interpreter: ^Interpreter,
	node: Assignment_Node,
) -> (
	_val: Value,
	_prop: Runtime_Propagator,
) {
	value: Value
	#partial switch assignee in node.assignee^ {
	case Variable_Read_Node:
		value = evaluate(interpreter, node.value^) or_return
		set(interpreter.current_scope, assignee.name, value) or_return
	case:
		return nil, Runtime_Error {
			type = .Invalid_Assignment,
			message = fmt.tprintf("Unable to assign to %s", reflect.union_variant_typeid(node.assignee^)),
		}
	}
	// val := evaluate(interpreter, node.value^) or_return
	// set(interpreter.current_scope, node.name, val)

	return value, nil
}

evaluate_collection :: proc(
	interpreter: ^Interpreter,
	coll: Collection_Literal_Node,
) -> (
	_res: Value,
	_err: Runtime_Propagator,
) {
	body := [dynamic]Value{}

	for expr in coll.body {
		append(&body, evaluate(interpreter, expr^) or_return)
	}

	return Collection{body = body}, nil
}

evaluate :: proc(
	interpreter: ^Interpreter,
	expr: Expression_Node,
) -> (
	_res: Value,
	_err: Runtime_Propagator,
) {
	switch t in expr {
	case String_Node:
		return t.value, nil
	case Integer_Node:
		return expr.(Integer_Node).value, nil
	case Float_Node:
		return expr.(Float_Node).value, nil
	case Bool_Node:
		return expr.(Bool_Node).value, nil
	case Collection_Literal_Node:
		return evaluate_collection(interpreter, t)
	case Variable_Read_Node:
		return evaluate_read(interpreter, t)
	case Add_Node:
		return evaluate_add(interpreter, t)
	case Divide_Node:
		return evaluate_divide(interpreter, t)
	case Multiply_Node:
		return evaluate_multiply(interpreter, t)
	case Negation_Node:
		return evaluate_negation(interpreter, t)
	case Subtract_Node:
		return evaluate_subtract(interpreter, t)
	case Less_Node:
		return evaluate_less(interpreter, t)
	case Greater_Node:
		return evaluate_greater(interpreter, t)
	case Less_Equals_Node:
		return evaluate_less_equals(interpreter, t)
	case Greater_Equals_Node:
		return evaluate_greater_equals(interpreter, t)
	case Equality_Node:
		return evaluate_equality(interpreter, t)
	case Call_Node:
		return evaluate_call(interpreter, t)
	case Subscript_Node:
		return evaluate_subscript(interpreter, t)
	case Variable_Node:
		return evaluate_variable_decl(interpreter, t)
	case Return_Node:
		return evaluate_return(interpreter, t)
	case Block:
		return evaluate_block(interpreter, t)
	case Function_Node:
		return evaluate_function_declaration(interpreter, t)
	case If_Node:
		return evaluate_if(interpreter, t)
	case Assignment_Node:
		return evaluate_assignment(interpreter, t)
	case:
		panic("Unknown node type")
	}
}

evaluate_subscript :: proc(
	interpreter: ^Interpreter,
	node: Subscript_Node,
) -> (
	_res: Value,
	_err: Runtime_Propagator,
) {
	source_value := evaluate(interpreter, node.operand^) or_return
	source, is_collection := source_value.(Collection)

	if !is_collection {
		return {}, Runtime_Error{type = .Type_Error, message = fmt.tprintf("Unable to subcript type %s", reflect.union_variant_typeid(source_value))}
	}

	index_value := evaluate(interpreter, node.indexer^) or_return
	index, is_int := index_value.(i64)

	if !is_int {
		return {}, Runtime_Error{type = .Type_Error, message = fmt.tprintf("Unable to index with type %s", reflect.union_variant_typeid(index_value))}
	}

	if index < 0 || index >= i64(len(source.body)) {
		return {}, Runtime_Error{type = .Index_Out_Of_Bounds_Error, message = fmt.tprintf("Index %d is not between 0 and %d", index, len(source.body))}
	}

	return source.body[index], nil
}

evaluate_call :: proc(
	interpreter: ^Interpreter,
	node: Call_Node,
) -> (
	_res: Value,
	_err: Runtime_Propagator,
) {
	callee := evaluate(interpreter, node.callee^) or_return
	arguments := make([]Value, len(node.arguments))

	defer delete(arguments)

	for arg, i in node.arguments {
		arguments[i] = evaluate(interpreter, arg^) or_return
	}

	#partial switch type in callee {
	case Function:
		if type.arity != len(arguments) {
			return nil, Runtime_Error {
				type = .Type_Error,
				message = fmt.tprintf(
					"Too %s arguments! Expected %d and got %d.",
					len(arguments) > type.arity ? "many" : "few",
					type.arity,
					len(arguments),
				),
			}
		}
		return call(interpreter, type, arguments)
	case Native_Function:
		if type.arity != len(arguments) && type.arity != -1 {
			return nil, Runtime_Error {
				type = .Type_Error,
				message = fmt.tprintf(
					"Too %s arguments! Expected %d and got %d.",
					len(arguments) > type.arity ? "many" : "few",
					type.arity,
					len(arguments),
				),
			}
		}
		return native_call(interpreter, type, arguments)
	}

	return nil, Runtime_Error {
		type = .Type_Error,
		message = fmt.tprintf("Unable to call type %s", reflect.union_variant_typeid(callee)),
	}

}

call :: proc(
	interpreter: ^Interpreter,
	callee: Function,
	arguments: []Value,
) -> (
	_res: Value,
	_err: Runtime_Propagator,
) {
	begin_scope(interpreter) or_return
	defer end_scope(interpreter)

	for arg, i in arguments {
		define(interpreter.current_scope, callee.definition.parameters[i].str, arg) or_return
	}
 
	for expr in callee.definition.body {
		_, propagated := evaluate(interpreter, expr^)
		if val, is_return := propagated.(Runtime_Return); is_return {
			return val.value, nil
		} else if propagated != nil {
			return nil, propagated
		}
	}

	return nil, nil
}

native_call :: proc(
	interpreter: ^Interpreter,
	callee: Native_Function,
	arguments: []Value,
) -> (
	_res: Value,
	_err: Runtime_Propagator,
) {
	return callee.native_proc(interpreter, arguments)
}

Type_Pair :: struct {
	left, right: typeid,
}

evaluate_add :: proc(
	interpreter: ^Interpreter,
	node: Add_Node,
) -> (
	_res: Value,
	_err: Runtime_Propagator,
) {
	left := evaluate(interpreter, node.left^) or_return
	right := evaluate(interpreter, node.right^) or_return

	left_type := reflect.union_variant_typeid(left)
	right_type := reflect.union_variant_typeid(right)

	type_pair: Type_Pair = {left_type, right_type}

	switch type_pair {
	case {i64, i64}:
		return left.(i64) + right.(i64), nil
	case {f64, f64}:
		return left.(f64) + right.(f64), nil
	case {string, string}:
		return strings.concatenate({left.(string), right.(string)}), nil
	}

	return nil, Runtime_Error {
		type = .Type_Error,
		message = fmt.tprintf("Unable to add types %s and %s", left_type, right_type),
	}
}

evaluate_subtract :: proc(
	interpreter: ^Interpreter,
	node: Subtract_Node,
) -> (
	_res: Value,
	_err: Runtime_Propagator,
) {
	left := evaluate(interpreter, node.left^) or_return
	right := evaluate(interpreter, node.right^) or_return

	left_type := reflect.union_variant_typeid(left)
	right_type := reflect.union_variant_typeid(right)

	type_pair: Type_Pair = {left_type, right_type}

	switch type_pair {
	case {i64, i64}:
		return left.(i64) - right.(i64), nil
	case {f64, f64}:
		return left.(f64) - right.(f64), nil
	case {i64, f64}:
		return f64(left.(i64)) - right.(f64), nil
	case {f64, i64}:
		return left.(f64) - f64(right.(i64)), nil
	}

	return nil, Runtime_Error {
		type = .Type_Error,
		message = fmt.tprintf("Unable to subtract types %s and %s", left_type, right_type),
	}
}
evaluate_multiply :: proc(
	interpreter: ^Interpreter,
	node: Multiply_Node,
) -> (
	_res: Value,
	_err: Runtime_Propagator,
) {
	left := evaluate(interpreter, node.left^) or_return
	right := evaluate(interpreter, node.right^) or_return

	left_type := reflect.union_variant_typeid(left)
	right_type := reflect.union_variant_typeid(right)

	type_pair: Type_Pair = {left_type, right_type}

	switch type_pair {
	case {i64, i64}:
		return left.(i64) * right.(i64), nil
	case {f64, f64}:
		return left.(f64) * right.(f64), nil
	case {i64, f64}:
		return f64(left.(i64)) * right.(f64), nil
	case {f64, i64}:
		return left.(f64) * f64(right.(i64)), nil
	}

	return nil, Runtime_Error {
		type = .Type_Error,
		message = fmt.tprintf("Unable to multiply types %s and %s", left_type, right_type),
	}
}
evaluate_divide :: proc(
	interpreter: ^Interpreter,
	node: Divide_Node,
) -> (
	_res: Value,
	_err: Runtime_Propagator,
) {
	left := evaluate(interpreter, node.left^) or_return
	right := evaluate(interpreter, node.right^) or_return

	left_type := reflect.union_variant_typeid(left)
	right_type := reflect.union_variant_typeid(right)

	type_pair: Type_Pair = {left_type, right_type}

	switch type_pair {
	case {i64, i64}:
		if right.(i64) == 0 {
			return {}, Runtime_Error{type = .Divide_By_Zero, message = "Division by zero"}
		}
		return left.(i64) / right.(i64), nil
	case {f64, f64}:
		if right.(f64) == 0 {
			return {}, Runtime_Error{type = .Divide_By_Zero, message = "Division by zero"}
		}
		return left.(f64) / right.(f64), nil
	}

	return nil, Runtime_Error {
		type = .Type_Error,
		message = fmt.tprintf("Unable to divide types %s and %s", left_type, right_type),
	}
}

evaluate_negation :: proc(
	interpreter: ^Interpreter,
	node: Negation_Node,
) -> (
	_res: Value,
	_err: Runtime_Propagator,
) {
	value := evaluate(interpreter, node.operand^) or_return

	#partial switch type in value {
	case f64:
		return -type, nil
	case i64:
		return -type, nil
	case bool:
		return !type, nil
	}

	return nil, Runtime_Error {
		type = .Type_Error,
		message = fmt.tprintf("Unable to negate type %s", reflect.union_variant_typeid(value)),
	}
}

evaluate_equality :: proc(
	interpreter: ^Interpreter,
	node: Equality_Node,
) -> (
	_res: Value,
	_err: Runtime_Propagator,
) {
	left := evaluate(interpreter, node.left^) or_return
	right := evaluate(interpreter, node.right^) or_return


	left_type := reflect.union_variant_typeid(left)
	right_type := reflect.union_variant_typeid(right)

	type_pair: Type_Pair = {left_type, right_type}

	switch type_pair {
	case {i64, i64}:
		return left.(i64) == right.(i64), nil
	case {f64, f64}:
		return left.(f64) == right.(f64), nil
	case {f64, i64}:
		return left.(f64) == f64(right.(i64)), nil
	case {i64, f64}:
		return f64(left.(i64)) == right.(f64), nil
	case {bool, bool}:
		return left.(bool) == right.(bool), nil
	case {string, string}:
		return strings.compare(left.(string), right.(string)) == 0, nil
	}

	return nil, Runtime_Error {
		type = .Type_Error,
		message = fmt.tprintf("Unable to compare types %s and %s", left_type, right_type),
	}
}

evaluate_less :: proc(
	interpreter: ^Interpreter,
	node: Less_Node,
) -> (
	_res: Value,
	_err: Runtime_Propagator,
) {
	left := evaluate(interpreter, node.left^) or_return
	right := evaluate(interpreter, node.right^) or_return


	left_type := reflect.union_variant_typeid(left)
	right_type := reflect.union_variant_typeid(right)

	type_pair: Type_Pair = {left_type, right_type}

	switch type_pair {
	case {i64, i64}:
		return left.(i64) < right.(i64), nil
	case {f64, f64}:
		return left.(f64) < right.(f64), nil
	}

	return nil, Runtime_Error {
		type = .Type_Error,
		message = fmt.tprintf("Unable to compare types %s and %s", left_type, right_type),
	}
}

evaluate_greater :: proc(
	interpreter: ^Interpreter,
	node: Greater_Node,
) -> (
	_res: Value,
	_err: Runtime_Propagator,
) {
	left := evaluate(interpreter, node.left^) or_return
	right := evaluate(interpreter, node.right^) or_return


	left_type := reflect.union_variant_typeid(left)
	right_type := reflect.union_variant_typeid(right)

	type_pair: Type_Pair = {left_type, right_type}

	switch type_pair {
	case {i64, i64}:
		return left.(i64) > right.(i64), nil
	case {f64, f64}:
		return left.(f64) > right.(f64), nil
	}

	return nil, Runtime_Error {
		type = .Type_Error,
		message = fmt.tprintf("Unable to compare types %s and %s", left_type, right_type),
	}
}

evaluate_less_equals :: proc(
	interpreter: ^Interpreter,
	node: Less_Equals_Node,
) -> (
	_res: Value,
	_err: Runtime_Propagator,
) {
	left := evaluate(interpreter, node.left^) or_return
	right := evaluate(interpreter, node.right^) or_return


	left_type := reflect.union_variant_typeid(left)
	right_type := reflect.union_variant_typeid(right)

	type_pair: Type_Pair = {left_type, right_type}

	switch type_pair {
	case {i64, i64}:
		return left.(i64) <= right.(i64), nil
	case {f64, f64}:
		return left.(f64) <= right.(f64), nil
	}

	return nil, Runtime_Error {
		type = .Type_Error,
		message = fmt.tprintf("Unable to compare types %s and %s", left_type, right_type),
	}
}

evaluate_greater_equals :: proc(
	interpreter: ^Interpreter,
	node: Greater_Equals_Node,
) -> (
	_res: Value,
	_err: Runtime_Propagator,
) {
	left := evaluate(interpreter, node.left^) or_return
	right := evaluate(interpreter, node.right^) or_return


	left_type := reflect.union_variant_typeid(left)
	right_type := reflect.union_variant_typeid(right)

	type_pair: Type_Pair = {left_type, right_type}

	switch type_pair {
	case {i64, i64}:
		return left.(i64) >= right.(i64), nil
	case {f64, f64}:
		return left.(f64) >= right.(f64), nil
	}

	return nil, Runtime_Error {
		type = .Type_Error,
		message = fmt.tprintf("Unable to compare types %s and %s", left_type, right_type),
	}
}

evaluate_read :: proc(
	interpreter: ^Interpreter,
	node: Variable_Read_Node,
) -> (
	_res: Value,
	_err: Runtime_Propagator,
) {
	value, declared := read(interpreter.current_scope, node.name)
	if !declared {
		return nil, Runtime_Error {
			type = .Undeclared_Variable,
			message = fmt.tprintf("Undeclared variable %s", node.name),
		}
	}

	return value, nil
}

print_runtime_error :: proc(error: Runtime_Error) {
	fmt.printfln("Runtime Error: %s", error.message)
}
