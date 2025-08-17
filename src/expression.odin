#+private
package holang

/* --- Expressions ---
 * the parsing and handling
 * of expressions, compile
 * and runtime
 */

import "base:intrinsics"

import "core:fmt"

// --- Types ---
Expression :: struct {
	
	// Parsing
	base_type : TypeID,	// Expression result type, -1 if unknown
						// Result type is checked during calculation
						// AND when the value is assigned somewhere
	
	
	
	// Data
	values : []ExprVal,
}


ExprVal :: struct {
	
	negative : bool,
	
	body : union {
		Operator,				// Mathematical operator
		ExpressionDelimiter,	// Parentheses
		
		ExpressionVariable,		// Constant, Function, Variable, Type (casting)
		Variant,				// Any constant value
	},
}

ExpressionVariable :: struct {
	
	type : enum {
		Variable,	// Standard variable
		Function,	// Function result
	},
	
	name : string,	// Search by name
	off  : VarID,	// Get by index
}

ExpressionDelimiter :: struct {
	
	type : enum {
		Unknown,
		
		ParenL,
		ParenR,
		
		TypeCast,
	},
	
	base_type : TypeID,		// Only used when cast
}

// --- Constants ---
PRC_SUM :: 11
PRC_MD	:: PRC_SUM + 1
PRC_PR	:: PRC_MD  + 1

PRC_BLN :: 1
PRC_CMP :: PRC_BLN + 1

// --- Variables ---

@(rodata, private="file")
/* --- operator_precedence ---
 * store the precedence for
 * each mathematical operator
 *
 * NOTE: also boolean operations!
 * NOTE: zero value means unsupported
 */
operator_precedence := #partial [Operator]int {
	
	// Arithmetic operations
	.Add = PRC_SUM,
	.Sub = PRC_SUM,
	.Mul = PRC_MD,
	.Div = PRC_MD,
	
	// Boolean operations
	.EQ = PRC_CMP,
	.NE = PRC_CMP,
	.LS = PRC_CMP,
	.LE = PRC_CMP,
	.GR = PRC_CMP,
	.GE = PRC_CMP,
	
	.And = PRC_BLN,
	.Or  = PRC_BLN,
	.Not = PRC_BLN,
}


// --- Procedures ---
get_precedence :: proc(op : union {Operator, Delimiter, Variant}) -> int {
	switch &t in op {
	case Operator:	return operator_precedence[t]
	case Delimiter: return PRC_PR
	case Variant:	return 0
	}
	
	return 0
}

prefix_operation :: proc(a : Variant, op : Operator) -> (c : Variant) {
	if a == nil do return nil
	if op == .Unknown do return nil
	
	switch t in a {
	case int,
		uint,
		byte,
		uintptr:	return raw_op(    0,   as_int(a), op )
	
	case f64:		return raw_op(  0.0, as_float(a), op )
	case bool: 		return raw_op( true,  as_bool(a), op )
	}
	
	return
}

postfix_operation :: proc(a : Variant, op : Operator) -> (c : Variant) {
	if a == nil do return nil
	if op == .Unknown do return nil
	
	switch t in a {
	case int,
		uint,
		byte,
		uintptr:	return raw_op(   as_int(a),    0, op )
	
	case f64:		return raw_op( as_float(a),  0.0, op )
	case bool: 		return raw_op(  as_bool(a), true, op )
	}
	
	return
}

operation :: proc(a, b : Variant, op : Operator) -> (c : Variant) {
	if a == nil || b == nil do return nil //, .Invalid_Value
	if op == .Unknown do return nil //, .Invalid_Operation
	
	// Get type precedence
	T :: enum {Bool, Int, Float}
	type_a, type_b : T
	switch t in a {
	case f64: type_a = .Float
	case int,
		uint,
		byte,
		uintptr:
		type_a = .Int
		
	case bool:
		type_a = .Bool
	}
	
	switch t in b {
	case f64: type_b = .Float
	case int,
		uint,
		byte,
		uintptr:
		type_b = .Int
		
	case bool:
		type_b = .Bool
	}
	
	switch max(type_a, type_b) {
	case .Int:		return raw_op(	 as_int(a),   as_int(b), op )
	case .Float:	return raw_op( as_float(a), as_float(b), op )
	case .Bool: 	return raw_op(	as_bool(a),  as_bool(b), op )
	}
	
	return nil //, .Invalid_Operation
}

as_int :: proc(val : Variant) -> int {
	switch &v in val {
	case int:		return v
	case uint:		return int(v)
	case byte:		return int(v)
	case uintptr:	return int(v)
	case f64:		return int(v)
	case bool:		return int(v)
	}
	
	return 0
}

as_float :: proc(val : Variant) -> f64 {
	switch &v in val {
	case f64:		return v
	case int:		return f64(v)
	case uint:		return f64(v)
	case byte:		return f64(v)
	case uintptr:	return f64(v)
	case bool:		return v ? 1 : 0
	}
	
	return 0
}

as_bool :: proc(val : Variant) -> bool {
	switch &v in val {
	case bool:		return v
	case int:		return bool(v)
	case uint:		return bool(v)
	case byte:		return bool(v)
	case uintptr:	return bool(v)
	case f64:		return false if v == 0 else true
	}
	
	return false
}

raw_op :: proc(
	a, b : $T,
	op : Operator
) -> (value : Variant) where
	intrinsics.type_is_numeric(T) ||
	intrinsics.type_is_boolean(T) {
	
	when intrinsics.type_is_numeric(T) do #partial switch op {
	case .Add: return a + b
	case .Sub: return a - b
	case .Mul: return a * b
	case .Div: return a / b if b != 0 else 0 // Avoid NANs
	
	case .EQ:  return a == b
	case .NE:  return a != b
	case .LS:  return a <  b
	case .LE:  return a <= b
	case .GR:  return a >  b
	case .GE:  return a >= b
	
	case .And: return a != 0 && b != 0
	case .Or:  return a != 0 || b != 0
	}
	
	when intrinsics.type_is_boolean(T) do #partial switch op {
	case .Add: return a || b
	case .Mul: return a && b
	
	case .EQ:  return a && b
	case .NE:  return a != b
	
	case .And: return a && b
	case .Or:  return a || b
	case .Not: return !(a && b)
	}
	
	return
}

// --- Solver ---

// --  Constant Expression ---
cexpr_push_value :: proc(arr : ^[dynamic]ConstExprVal, val : ConstExprVal) {
	append(arr, val)
}

cexpr_solve_top :: proc(arr : ^[dynamic]ConstExprVal) -> (err : Error) {
	defer if err != nil do fmt.println(arr, "Solve Top")
	
	// Situation:
	// [ ... , NUM1, OP1, NUM2, OP2]
	// Get precedence for both OPs
	// If OP1 >= OP2
	// -> Apply OP1 on NUM1 and NUM2
	for len(arr) > 3 {
		
		arr_len := len(arr)
		top_i := arr_len - 1
		
		// Get values or break
		num1, num2 := arr[top_i - 3].body.(Variant)	 or_break, arr[top_i - 1].body.(Variant) or_break
		op1,  op2  := arr[top_i - 2].body.(Operator) or_break, arr[top_i].body.(Operator)    or_break
		
		pr1 := get_precedence(op1)
		pr2 := get_precedence(op2)
		if min(pr1, pr2) <= 0 do return .Invalid_Operator
		if pr1 < pr2 do break
		
		// Pop values and solve pair
		for _ in 0..<4 do pop(arr)
		result := operation(num1, num2, op1)
		if result == nil do return .Expression_Invalid
		
		// Add result and first 
		// Operator to the stack
		append(arr, ConstExprVal { body = result })
		append(arr, ConstExprVal { body = op2 })
	}
	
	return nil
}

cexpr_solve_value_until_paren :: proc(
	arr : ^[dynamic]ConstExprVal
) -> (err : Error) {
	defer if err != nil do fmt.println(arr, "Solve Paren")
	
	for ;; {
		num_vals := len(arr)
		(num_vals > 0) or_break
		
		top_i := num_vals - 1
		top   := arr[top_i]
		
		// Check for ParenL (opening parentheses)
		if _, is_paren := top.body.(Delimiter); is_paren {
			pop(arr)
			break
		}
		
		// No Parentheses
		// Last *must* be a number
		num2 := top.body.(Variant) or_else nil
		if num2 == nil do return .Expression_Invalid
		
		// Check second last
		(num_vals > 1) or_break
		if _, is_paren := arr[top_i - 1].body.(Delimiter); is_paren {
			pop(arr); pop(arr) // Double pop
			append(arr, ConstExprVal { body = num2 })  // Add number back on top
			break
		}
		
		// Get operator
		op := arr[top_i - 1].body.(Operator) or_else .Unknown
		if op == .Unknown do return .Expression_Invalid
		
		(num_vals > 2) or_break
		// Situation:
		// [ ... , NUM1, OP, NUM2]
		// -> Apply OP on NUM1 and NUM2
		num1 := arr[top_i - 2].body.(Variant) or_else nil
		if num1 == nil do return .Expression_Invalid
		
		if get_precedence(op) <= 0 do return .Invalid_Operator
		
		// Pop and calculate result
		for _ in 0..<3 do pop(arr)
		result := operation(num1, num2, op)
		if result == nil do return .Expression_Invalid
		
		// Add result onto stack
		append(arr, ConstExprVal { body = result })
	}
	
	return nil
}

// --  Expression  --
expr_push_value :: proc(arr : ^[dynamic]ExprVal, val : ExprVal) {
	append(arr, val)
}