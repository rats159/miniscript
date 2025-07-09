package main

import "core:fmt"
import "core:mem"
import "core:strings"

print_tree :: proc(root: Program_Node) {
	arena := mem.Dynamic_Arena{}
	mem.dynamic_arena_init(&arena)
	context.allocator = mem.dynamic_arena_allocator(&arena)

	fmt.println("Program {")
	for statement in root.body {
		fmt.print("    ")
		print_stmt(statement^, 1)
	}
	fmt.println("}")

	free_all()
}

print_stmt :: proc(stmt: Statement_Node, indent: int) {
	switch type in stmt {
	case Variable_Node:
		print_variable(type, indent)
	case Assignment_Node:
		print_assignment(type, indent)
	case If_Node:
		print_if(type, indent)
	case Void_Node:
		print_void(type, indent)
	case Function_Node:
		print_function(type, indent)
	case Block_Statement:
		print_block(type, indent)
	case Return_Node:
		print_return(type, indent)
	}

}

print_void :: proc(node: Void_Node, indent: int) {
	indent_str := strings.repeat("    ", indent)
	fmt.println("Void {")
	fmt.printf("%s    ", indent_str)
	print_expr(node.expr^, indent + 1)

	fmt.print(indent_str)
	fmt.println("}")
}

print_return :: proc(node: Return_Node, indent: int) {
	indent_str := strings.repeat("    ", indent)
	fmt.println("Return {")
	fmt.printf("%s    ", indent_str)
	print_expr(node.value^, indent + 1)

	fmt.print(indent_str)
	fmt.println("}")
}

print_block :: proc(block: Block_Statement, indent: int) {
	indent_str := strings.repeat("    ", indent)
	fmt.println("Block {")
	for stmt in block.body {
		fmt.printf("%s    ", indent_str)
		print_stmt(stmt^, indent + 1)
	}
	fmt.print(indent_str)
	fmt.println("}")
}

print_function :: proc(func: Function_Node, indent: int) {
	indent_str := strings.repeat("    ", indent)
	fmt.println("Function Body {")
	fmt.printfln("%s    Name: %s", indent_str, func.name.str)
	fmt.printfln("%s    Parameters: [", indent_str)
	for param in func.parameters {
		fmt.printf("%s        %s,", indent_str, param.str)
	}
	fmt.printfln("%s    ]", indent_str)
	fmt.printfln("%s    Body: [", indent_str)
	for stmt in func.body {
		fmt.printf("%s        ", indent_str)
		print_stmt(stmt^, indent + 2)
	}
	fmt.printfln("%s    ]", indent_str)
	fmt.print(indent_str)
	fmt.println("}")
}

print_if :: proc(stmt: If_Node, indent: int) {
	indent_str := strings.repeat("    ", indent)
	fmt.println("If Statement {")
	fmt.printf("%s    Condition: ", indent_str)
	print_expr(stmt.condition^, indent + 1)
	fmt.printf("\n%s    Body: ", indent_str)
	print_stmt(stmt.body^, indent + 1)
	if els, ok := stmt.else_body.?; ok {
		fmt.printf("\n%s    Else Body: ", indent_str)
		print_stmt(els^, indent + 1)
	}
	fmt.print(indent_str)
	fmt.println("}")
}

print_variable :: proc(var: Variable_Node, indent: int) {
	indent_str := strings.repeat("    ", indent)
	fmt.println("Variable Declaration {")
	fmt.printfln("%s    Name: %s", indent_str, var.name)
	fmt.printf("%s    Value: ", indent_str)
	print_expr(var.value^, indent + 1)
	fmt.print(indent_str)
	fmt.println("}")
}

print_assignment :: proc(var: Assignment_Node, indent: int) {
	indent_str := strings.repeat("    ", indent)
	fmt.println("Variable Assignment {")
	fmt.printfln("%s    Name: %s", indent_str, var.name)
	fmt.printf("%s    Value: ", indent_str)
	print_expr(var.value^, indent + 1)
	fmt.print(indent_str)
	fmt.println("}")
}

print_expr :: proc(expr: Expression_Node, indent: int) {
	switch type in expr {
	//
	case Collection_Literal_Node:
		print_collection_literal(type, indent)
	case Add_Node:
		print_add(type, indent)
	case Subtract_Node:
		print_subtract(type, indent)
	case Multiply_Node:
		print_multiply(type, indent)
	case Divide_Node:
		print_divide(type, indent)
	case Negation_Node:
		print_negation(type, indent)
	case Less_Node:
		print_less(type, indent)
	case Greater_Node:
		print_greater(type, indent)
	case Less_Equals_Node:
		print_less_equals(type, indent)
	case Greater_Equals_Node:
		print_greater_equals(type, indent)
	case Equality_Node:
		print_equality(type, indent)
	//
	case Integer_Node:
		print_int(type, indent)
	case Float_Node:
		print_float(type, indent)
	case Bool_Node:
		print_bool(type, indent)
	case String_Node:
		print_string(type, indent)
	//
	case Variable_Read_Node:
		print_read(type, indent)
	case Call_Node:
		print_call(type, indent)
	case Subscript_Node:
		print_subscript(type, indent)
	}
}

print_collection_literal :: proc(coll: Collection_Literal_Node, indent: int) {
	indent_str := strings.repeat("    ", indent)
	fmt.println("Collection Literal {")
	for expr in coll.body {
		fmt.printf("%s    ", indent_str)
		print_expr(expr^, indent + 1)
	}
	fmt.print(indent_str)
	fmt.println("}")
}

print_read :: proc(var: Variable_Read_Node, indent: int) {
	indent_str := strings.repeat("    ", indent)
	fmt.println("Variable Read {")
	fmt.printfln("%s    Name: %s", indent_str, var.name)
	fmt.print(indent_str)
	fmt.println("}")
}

print_binary_op :: proc(name: string, left, right: Expression_Node, indent: int) {
	indent_str := strings.repeat("    ", indent)
	fmt.printfln("%s {{", name)
	fmt.printf("%s    Left: ", indent_str)
	print_expr(left, indent + 1)
	fmt.printf("%s    Right: ", indent_str)
	print_expr(right, indent + 1)
	fmt.print(indent_str)
	fmt.println("}")
}

print_unary_op :: proc(name: string, operand: Expression_Node, indent: int) {
	indent_str := strings.repeat("    ", indent)
	fmt.printfln("%s {{", name)
	fmt.printf("%s    Operand: ", indent_str)
	print_expr(operand, indent + 1)
	fmt.print(indent_str)
	fmt.println("}")
}

print_add :: proc(node: Add_Node, indent: int) {
	print_binary_op("Add", node.left^, node.right^, indent)
}

print_call :: proc(node: Call_Node, indent: int) {
	indent_str := strings.repeat("    ", indent)
	fmt.println("Call {")
	fmt.printf("%s    Callee: ", indent_str)
	print_expr(node.callee^, indent + 1)
	fmt.printfln("%s    Arguments: [", indent_str)
	for argument in node.arguments {
		fmt.printf("%s        ", indent_str)
		print_expr(argument^, indent + 2)
	}
	fmt.printfln("%s    ]", indent_str)
	fmt.print(indent_str)
	fmt.println("}")
}

print_subscript :: proc(node: Subscript_Node, indent: int) {
	indent_str := strings.repeat("    ", indent)
	fmt.println("Subscript {")
	fmt.printf("%s    Operand: ", indent_str)
	print_expr(node.operand^, indent + 1)
	fmt.printf("%s    Indexer: ", indent_str)
	print_expr(node.indexer^, indent + 1)
	fmt.print(indent_str)
	fmt.println("}")
}

print_negation :: proc(node: Negation_Node, indent: int) {
	print_unary_op("Negation", node.operand^, indent)
}

print_subtract :: proc(node: Subtract_Node, indent: int) {
	print_binary_op("Subtract", node.left^, node.right^, indent)
}

print_multiply :: proc(node: Multiply_Node, indent: int) {
	print_binary_op("Multiply", node.left^, node.right^, indent)
}

print_divide :: proc(node: Divide_Node, indent: int) {
	print_binary_op("Divide", node.left^, node.right^, indent)
}

print_equality :: proc(node: Equality_Node, indent: int) {
	print_binary_op("Equality", node.left^, node.right^, indent)
}

print_less :: proc(node: Less_Node, indent: int) {
	print_binary_op("Less", node.left^, node.right^, indent)
}

print_greater :: proc(node: Greater_Node, indent: int) {
	print_binary_op("Greater", node.left^, node.right^, indent)
}

print_less_equals :: proc(node: Less_Equals_Node, indent: int) {
	print_binary_op("Less or Equals", node.left^, node.right^, indent)
}

print_greater_equals :: proc(node: Greater_Equals_Node, indent: int) {
	print_binary_op("Greater or Equals", node.left^, node.right^, indent)
}

print_int :: proc(node: Integer_Node, indent: int) {
	fmt.println(node.value)
}

print_float :: proc(node: Float_Node, indent: int) {
	fmt.println(node.value)
}


print_bool :: proc(node: Bool_Node, indent: int) {
	fmt.println(node.value)
}
print_string :: proc(node: String_Node, indent: int) {
	fmt.println(node.value)
}
