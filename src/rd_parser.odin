#+private="file"
package holang

/* --- Recursive Descent Parser ---
 * a replacement for the horrifying mess
 * that was and is
 *     parser.odin *and*
 *     parser_state.odin
 *
 * using a *simple* recursive
 * descent algorithm, no AST! (haha, look how that went)
 */

import "base:intrinsics"

import "core:fmt"
import "core:mem"
import "core:slice"
import "core:strings"

// --- Constants ---
DUMBO_MESSAGES :: false

// --- Types ---
ParseState :: struct {
	
	token : TokenID,	// Current token
	scope : struct {
		depth : int,		// Scope depth
		type  : ScopeType,	// Type
	},

	entry : bool,	// Has entry point been defined
}
ScopeType :: enum {
	File, Function, Block
}

ParseErrorMessage :: struct {
	token : Token,
	
	error : Error,
	message : string,
}

ParsingProcedure :: #type proc(
	vm : VM, state : ^ParseState
) -> (err : Error)

@(private="file")
FRAME :: ^AST_Frame

@(private="file")
EXPR :: ^AST_Expression

// --- Procedures ---
@(private)
parse_vm :: proc(
	vm : VM,
	start_token : TokenID,
) -> (err : Error) {
	if vm == nil do return .No_VM
	clear(&vm.errors)
	
	// Default state
	state := ParseState {
		token = start_token
	}
	
	parse_loop: for ;; {
		
		// Parse until no more tokens
		if parse_file_scope(vm, &state) == .EOF {
			break parse_loop
		}
	}
	
	vm.parsed_to = int(state.token - 1)
	return nil
}

/* --- get_next_token ---
 * gets the next token
 * while adding to the count
 */
get_next_token :: proc(
	vm : VM, state : ^ParseState,
	grow := true
) -> (token : Token, text : string, err : Error) {
	token, err = get_token(vm, state.token)
	if err == .Invalid_Token do err = .EOF
	if err != nil do return
	
	text, err = get_token_string(vm, token)
	if err != nil do return
	
	// Update next token
	if grow do state.token += 1
	return
}

/* --- peek_token ---
 * peeks the next token in line
 * without moving count forward
 */
peek_token :: proc(
	vm : VM, state : ^ParseState
) -> (token : Token, ok : bool) {
	t, err := get_token(vm, state.token + 1)
	if err != nil do return
	
	return t, true
}

/* --- parser_error_emit ---
 * emits an error during parsing
 * which gets printed with multi
 * error support
 */
parser_error_emit :: proc(
	vm : VM, state : ^ParseState,
	error : Error, message : string,
	loc := #caller_location
	) -> (err : Error) {
	
	
	token, token_err := get_token(vm, state.token - 1)
	
	// Ignore messages that happen on
	// The same line as an old one
	if  el := len(vm.errors); el > 0 {
		
		MAX_ERRORS :: 5
		if el >= MAX_ERRORS do return
		
		if 	token.meta.line_num <=
			vm.errors[el - 1].token.meta.line_num
				{ return }
	}
	
	// Print error message
	fmt.printf(
		"\nError during parsing at line %i, char %i, scope %i\nError type: ",
		token.meta.line_num, token.meta.rune_num, state.scope.depth
	)
	
	fmt.println(error)
	fmt.println("Message:", message)
		
	text, text_err := get_token_string(vm, token)
	fmt.println("Got:", text)
	when ODIN_DEBUG do fmt.println(loc)
	
	append(&vm.errors, ParseErrorMessage {
		token = token,
		error = error,
		message = message,
	})
	
	return error
}

/* --- parser_dumbo_emit ---
 * emit a very dumb message for the sake of it
 */
parser_dumbo_emit :: #force_inline proc(msg : string, loc := #caller_location) {
	when DUMBO_MESSAGES do fmt.println(">", msg, loc)
}

// --  Parsing  --
parse_file_scope :: proc(vm : VM, state : ^ParseState) -> (err : Error) {
	token, text, token_err := get_next_token(vm, state)
	if token_err != nil do return token_err
	
	// Depth 0 - FILE DEPTH:
	// in this depth, you can define
	// global variables, constants,
	// types, functions, AND NOTHING ELSE!
	
	file_scope_expect := Expectation {
		positive = {
			TokenKeyword {
				field = {
					.Type,
					.Function,
					.Variable,
					.Constant,
					
					.Entry,
				},
			},
		},
	}
	
	// Declarative keyword expected
	if parse_expectations(token, file_scope_expect) {
		
		kw := token.body.(TokenKeyword)
		#partial switch kw.type {
		case .Function:
			parser_dumbo_emit("Parsing a Function Declaration!")
			
		case .Variable:
			parser_dumbo_emit("Parsing a Variable Declaration!")
			return parse_variable(vm, state, &vm.ast_root)
		
		case .Constant:
			parser_dumbo_emit("Parsing a Constant Declaration!")
			return parse_constant(vm, state)
		
		case .Type:
			parser_dumbo_emit("Parsing a Type Declaration!")
			return parse_type(vm, state)
		
		case .Entry:
			if state.entry {
				return parser_error_emit(
					vm, state, .Token_Unexpected,
					"File entry has already been defined"
				)
			}
			
			state.entry = true
			
			if !parse_util_single_token(
				vm, state, TokenDelimiter { type = .CurlyL },
				"Expected \"{\" after entry keyword"
			) { return .Token_Unexpected }
		
			// Commands can only be placed in entry when in file scope
			parser_dumbo_emit("Parsing Entry logic!")
			new_scope, scope_err := parse_scope(vm, state, &vm.ast_root)
			if scope_err != nil do return scope_err
			
			
		
		// If you reach here, count me impressed
		case: unreachable()
		}
		
	} else {
		
		return parser_error_emit(
			vm, state, .Token_Unexpected,
			"Expected Constant, Type, Variable or Function declaration in File Scope"
		)
	}
	
	return
}

parse_scope :: proc(vm : VM, state : ^ParseState, scope : FRAME) -> (new_scope : FRAME, err : Error) {
	
	new_scope, err = ast_allocate_frame(vm, state, scope)
	if err != nil do return
	state.scope.depth += 1
	defer state.scope.depth -= 1
	
	// Standard scope, where you can
	// Write logic and define variables
	
	parse_loop: for ;; {
		
		end, parse_err := parse_scope_element(vm, state, new_scope)
		
		if parse_err != nil do parse_err = parse_util_skip_until_terminator(vm, state, parse_err)
		if parse_err == .EOF {
			return nil, parser_error_emit(
				vm, state, .Scope_Incomplete,
				"Current scope has not been completed"
			)
		}
		if end do break
	}
	
	return
	
	// --- Internal Procedures ---
	parse_scope_element :: proc(
		vm : VM, state : ^ParseState, scope : FRAME
	) -> (end : bool, err : Error) {
		token, text, token_err := get_next_token(vm, state)
		if token_err != nil do return false, token_err
		
		expectation_scope := Expectation {
			positive = {
				TokenKeyword {
					field = {
						
						.If,
						.For,
						
						.Variable,
						
						.Raw,
						
						// --- Builtins ---
						.Deref,
					}
				},
				
				TokenDelimiter {
					field = { .CurlyL, .CurlyR }	
				},
				
				// Vars, Functions
				TokenIdentifier {},
			}
		}
		
		if !parse_expectations(token, expectation_scope) {
			return false, parser_error_emit(
				vm, state, .Token_Unexpected,
				"Expected function call, variable assignation or definition, logic expression in scope"
			)
		}
		
		#partial switch &b in token.body {
		case TokenKeyword:
			#partial switch b.type {
			case .Variable:
				return false, parse_variable(vm, state, scope)
			
			case .If:
			case .For:
			
			case .Raw:
				// NOTE: this setup is most likely not permanent and
				//       you'll have to tag whole functions as "raw"
				
				if scope.raw {
					return false, parser_error_emit(
						vm, state, .Token_Unexpected,
						"Current scope is already raw"
					)
				}
				
				if !parse_util_terminator(vm, state) do return false, .Token_Unexpected
				scope.raw = true
			
			// --- Builtins ---
			case .Deref:
				return false, parse_deref(vm, state, scope)
			
			case: unreachable()
			}
		
		case TokenIdentifier:
			
			#partial switch get_identifier_type(vm, text) {
			case .Function:
				return false, parser_error_emit(
					vm, state, .Unimplemented,
					"Function calls are yet to be implemented"
				)
			
			case .Constant,
				.Type:
				return false, parser_error_emit(
					vm, state, .Invalid_Name,
					"Expected valid function or variable identifier"
				)
			
			case:
			
				// Must be a variable
				var_id, var, found := parse_var_id(scope, text)
				if !found {
					return false, parser_error_emit(
						vm, state, .Invalid_Name,
						"Expected valid function or variable identifier"
					)
				}
				
				if !var.mutable {
					return false, parser_error_emit(
						vm, state, .Mutating_Immutable,
						"Unable to mutate immutable variable"
					)
				}
				
				// Variable operation
				var_err := parse_variable_op(vm, state, scope, var_id, var.type)
				if var_err != nil do return false, var_err
			}
		
		case TokenDelimiter:
		
			#partial switch b.type {
			case .CurlyL:
			
				// New scope
				new_scope, scope_err := parse_scope(vm, state, scope)
				if scope_err != nil do return false, scope_err
				
				node, node_err := ast_allocate_node(vm, state, scope)
				if node_err != nil do unreachable() // Should be unreachable
				
				node.body = AST_Empty {
					child = new_scope
				}
				
				err = ast_append_node(vm, state, scope, node)
				
			case .CurlyR:
				// End scope
				return true, nil
				
			case: unreachable()
			}
			
		case: unreachable()
		}
		
		return
	}
}

// -   Scoped Operation   -
parse_variable_op :: proc(
	vm : VM, state : ^ParseState,
	scope : FRAME, id : VarID, type : TypeID
) -> (err : Error) {
	
	
	// STAGE 0: Parse variable side
	res_type := type
	off   : uintptr
	stage : enum {
		Value,
		Selector
	}
	
	offsets : [dynamic]AST_OffsetValue
	defer delete(offsets)
	
	op := Operator.Equals
	parse_loop: for ;; {
		
		base_type := get_base_type(vm, res_type, true)
		type_body, type_err := get_type(vm, base_type)
		if type_err != nil {
			return parser_error_emit(
				vm, state, .Unknown_Type,
				"Unable to determine variable type"
			)
		}
		
		#partial switch &b in type_body.body {
		case StructBody:
			
			switch stage {
			case .Value:
				
				token, text, token_err := get_next_token(vm, state)
				if token_err != nil do return token_err
				
				expectation_struct_value := Expectation {
					positive = {
						TokenOperator {
							field = { .Equals }
						},
						
						TokenDelimiter {
							field = { .Period }
						}
					}
				}
				
				// Expect "=" or "."
				if !parse_expectations(token, expectation_struct_value) {
					return parser_error_emit(
						vm, state, .Token_Unexpected,
						"Expected \"=\" or \".\" after struct variable statement"
					)
				}
				
				#partial switch &t in token.body {
				case TokenOperator:
					// End parse expression
					break parse_loop
				
				case TokenDelimiter:
					// Change stage
					stage = .Selector
				}
			
			case .Selector:
				expectation_struct_selector := Expectation {
					positive = {
						TokenIdentifier {}
					}
				}
				
				token, text, token_err := get_next_token(vm, state)
				if token_err != nil do return token_err
				
				if text == "" || text == "_" {
					return parser_error_emit(
						vm, state, .Unknown_Member,
						"Invalid struct member identifier"
					)
				}
				
				// Expect member identifier
				if !parse_expectations(token, expectation_struct_selector) {
					return parser_error_emit(
						vm, state, .Token_Unexpected,
						"Expected member identifier after struct member selector"
					)
				}
				
				// Find correct member
				idx := -1; for m, i in b.members {
					(m.name == text) or_continue
					
					idx = i
					break
				}
				
				if idx == -1 {
					return parser_error_emit(
						vm, state, .Unknown_Member,
						"Invalid struct member identifier"
					)
				}
				
				// Apply changes
				member  := b.members[idx]
				res_type = member.base_type 
				off     += member.offset
				
				stage = .Value
			
				// Detect access
				if !ast_can_access_data(vm, scope, type) {
					return parser_error_emit(
						vm, state, .Invalid_Access,
						"Trying to access opaque data in non-raw scope"
					)
				}
			}
		
		case ArrayBody:
			assert(stage == .Value, "Incorrect stage in array variable member parsing")
			
			// STAGE 0: Expect "[" or "="
			expectation_arr := Expectation {
				positive = {
					TokenDelimiter {
						field = { .SquareL }
					},
					
					TokenOperator {
						field = { .Equals }
					}
				}
			}
			
			token, text, token_err := get_next_token(vm, state)
			if token_err != nil do return token_err
			
			if !parse_expectations(token, expectation_arr) {
				return parser_error_emit(
					vm, state, .Token_Unexpected,
					"Expected \"[\" or \"=\" after array variable"
				)
			}
			
			#partial switch &b in token.body {
			case TokenDelimiter:
			case TokenOperator:
				break parse_loop
			
			case: unreachable()
			}
			
			// STAGE 1: Expect index
			expr, expr_err := parse_expression(vm, state, scope, -1)
			if expr_err != nil do return expr_err
			single_offset := mem.align_forward_int(b.size, b.align)
			
			#partial switch &t in expr.body {
			case AST_ConstantValue:
				
				idx := as_int(t.value)
				
				// Compile time bounds check
				if idx < 0 || idx >= b.size {
					return parser_error_emit(
						vm, state, .Bounds_Check,
						"Given index doesn't fit within bounds of the array"
					)
				}
				
				multi_offset  := idx * single_offset
				off += uintptr(multi_offset)
			
			case:
				
				// Append offset
				if off != 0 {
					append(&offsets, off)
					off = 0
				}
				
				// Add dynamic offset
				append(&offsets, AST_OffsetExpr {
					ceil = b.size,
					
					size = single_offset,
					expr = expr
				})
			}
			
			res_type = b.base_type
			
			// STAGE 2: Expect "]"
			if !parse_util_single_token(
				vm, state, TokenDelimiter { type = .SquareR },
				"Expected \"]\" after array index selector"
			) { return .Expression_Invalid }
			
			// Detect access
			if !ast_can_access_data(vm, scope, type) {
				return parser_error_emit(
					vm, state, .Invalid_Access,
					"Trying to access opaque data in non-raw scope"
				)
			}
			
		case BoolBody:
			// Expect only "=" from booleans
			// TODO: Bool / bitwise operations?
			token, text, token_err := get_next_token(vm, state)
			if token_err != nil do return token_err
			
			expectation_standard := Expectation {
				positive = {
					TokenOperator {
						field = { .Equals }
					}
				}
			}
			
			if !parse_expectations(token, expectation_standard) {
				return parser_error_emit(
					vm, state, .Token_Unexpected,
					"Expected \"=\" after boolean variable expression"
				)
			}
			
			break parse_loop
			
			
		case:
			// Expect "=" (TODO: other operators)
			
			token, text, token_err := get_next_token(vm, state)
			if token_err != nil do return token_err
			
			expectation_standard := Expectation {
				positive = {
					TokenOperator {
						field = {
							.Equals,
							.AddEq, .SubEq,
							.MulEq, .DivEq,
						}
					}
				}
			}
			
			if !parse_expectations(token, expectation_standard) {
				return parser_error_emit(
					vm, state, .Token_Unexpected,
					"Expected assignation operation after variable expression"
				)
			}
			
			op = (token.body.(TokenOperator) or_else unreachable()).type
			
			break parse_loop
		}
	}
	
	if len(offsets) > 0 && off > 0 do append(&offsets, off)
	
	// STAGE 1: Expect expression
	base_type := get_base_type(vm, res_type, false)
	if base_type == -1 {
		return parser_error_emit(
			vm, state, .Unknown_Type,
			"Unable to determine variable expression type"
		)
	}
	
	expr, expr_err := parse_expression(vm, state, scope, base_type)
	if expr_err != nil do return expr_err
	
	// STAGE 2: Expect ";"
	if !parse_util_terminator(vm, state) do return .Token_Unexpected
	
	// Finished, generate AST
	node, node_err := ast_allocate_node(vm, state, scope)
	if node_err != nil do unreachable()
	
	offset : AST_Offset
	if len(offsets) == 0 {
		offset.single = off
	} else {
		
		alloc, alloc_err := vm_get_ast_allocator(vm)
		if alloc_err != nil do return  alloc_err
		
		offset.values = slice.clone(offsets[:], alloc)
	}
	
	node.body = AST_Assign {
		
		op = op,
		
		var  = id,
		off  = offset,
		type = base_type,
		expr = expr
	}
	
	return ast_append_node(vm, state, scope, node)
}

// -   Constants   -
parse_constant :: proc(vm : VM, state : ^ParseState) -> (err : Error) {
	token, text, token_err := get_next_token(vm, state)
	if token_err != nil do return token_err
	
	// EXAMPLE SYNTAX:
	// #const MY_CONSTANT = 10;
	
	// STAGE 0: Expect identifier
	if !parse_expectations(token, expectation_identifier) {
		return parser_error_emit(
			vm, state, .Token_Unexpected,
			"Expected an identifier after #const declaration"
		)
	}
	
	identifier := text
	switch get_identifier_type(vm, identifier) {
	case .Constant:
		return parser_error_emit(
			vm, state, .Constant_Over,
			"Trying to declare over existing constant"
		)
	
	case .Function,
		 .Variable,
		 .Type:
		 
		return parser_error_emit(
			vm, state, .Invalid_Name,
		 	"Trying to declare over existing identifier"
		)
	
	case .Unknown:
	}
	
	// STAGE 1: Expect "="
	if !parse_util_equals(vm, state, "after a constant identifier") do return .Token_Unexpected
	
	// STAGE 2: Expect constant expression
	value, value_err := parse_constant_expression(vm, state)
	if value_err != nil do return value_err
	
	// STAGE 3: Expect terminator ";"
	if !parse_util_terminator(vm, state) do return .Token_Unexpected
	
	// Create a constant
	constant := Constant {
		name = identifier,
		value = value
	}
	fmt.print("Constant", identifier, "=", value)
	if ptr, ok := value.(uintptr); ok && ptr != 0 {
		fmt.print(" ->", transmute(cstring)get_global_ptr(vm, ptr))
	}
	fmt.println()
	
	return register_constant(vm, constant)
}

parse_constant_expression :: proc(vm : VM, state : ^ParseState) -> (value : Variant, err : Error) {
	
	// EXAMPLE SYNTAX:
	// MY_CONST + 2 * OTHER_CONST - (1 + 2 + 3)
	// !MY_BOOL_CONST == !MY_OTHER_BOOL_CONST
	
	depth : int 	// Expression depth
	expr  : PrattExpression
	defer delete(expr)
	
	finished : bool
	expr_loop: for ;; {
		token, text, token_err := get_next_token(vm, state)
		if token_err != nil do return nil, token_err
		
		#partial switch &b in token.body {
		case TokenDelimiter:
			#partial switch b.type {
			case .ParenL: depth += 1
			case .ParenR:
				// Remember to check depth
				if depth == 0 do break expr_loop
				depth -= 1
			case:
				break expr_loop
			}
			
			append(&expr, PrattValue { b.type })
			if depth < 0 do break expr_loop
			
		case TokenOperator:
			append(&expr, PrattValue { b.type })
		
		case TokenLiteral:
			if token.value == nil {
				return nil, parser_error_emit(
					vm, state, .Invalid_Value,
					"Expected only valid values in constant expression"
				)
			}
			
			append(&expr, PrattValue { token.value })
		
		case TokenIdentifier:
			val := const_ident_to_value(vm, text)
			if val == nil {
				return nil, parser_error_emit(
					vm, state, .Unknown_Const,
					"Unknown constant identifier in constant expression"
				)
			}
			
			append(&expr, PrattValue { val })
		
		case:
			return nil, parser_error_emit(
				vm, state, .Token_Unexpected,
				"Expected only literals, constants, operators or parentheses in constant expression"
			)
		}
	}
	
	// Reverse single
	state.token -= 1
	
	value, err = pratt_parse(&expr, context.temp_allocator)
	if err != nil {
		return nil, parser_error_emit(
			vm, state, err,
			"Invalid expression"
		)
	}
	
	return
	
	// --- Internal Procedures ---
	const_ident_to_value :: proc(vm : VM, name : string) -> Variant {
		
		const_id, id_err := get_const_id_by_name(vm, name)
		const, const_err := get_constant(vm, const_id)
		
		if id_err != nil || const_err != nil do return nil
		return const.value
	}
}

parse_expression :: proc(
	vm : VM, state : ^ParseState,
	scope : FRAME, type : TypeID,
	from  : EXPR = nil, // Optional parent expr
	loc := #caller_location
) -> (expr : EXPR, err : Error) {
	// If from is a valid EXPR
	// No AST_Expression shall
	// Be allocated
	expr = from
	
	// Constant Expression
	if parse_peek_const(vm, state.token) {
		const, const_err := parse_constant_expression(vm, state)
		if const_err != nil do return nil, const_err
		
		if expr == nil do expr, err = ast_allocate_expr(vm, state)
		assert(err == nil, "Unable to allocate expression")
		
		expr.body = AST_ConstantValue { value = const }
		return
	}
	
	// Type inference
	base_type : TypeID
	if type == -1 { // Expression type undefined
		base_type = get_base_type(
			vm, parse_peek_expr_first_type(vm, scope, state.token),
			false
		);  if base_type == -1 {
			return nil, parser_error_emit(
				vm, state, .Unknown_Type,
				"Unable to determine expression type"
			)
		}
	} else {
		base_type = get_base_type(vm, type, false)
	}
	
	// Runtime Expression
	type_body, type_err := 
		get_type(vm, get_base_type(vm, base_type, true))
		// Must ignore unique to get true base
	#partial switch &b in type_body.body {
	case StructBody:
		struct_lit, struct_err := parse_struct_expr(vm, state, scope, base_type)
		if struct_err != nil do return nil, struct_err
		
		if expr == nil do expr, err = ast_allocate_expr(vm, state)
		assert(err == nil, "Unable to allocate expression")
		
		expr.body = struct_lit
		
	case ArrayBody:
		arr_lit, arr_err := parse_array_expr(vm, state, scope, base_type)
		if arr_err != nil do return nil, arr_err
		
		if expr == nil do expr, err = ast_allocate_expr(vm, state)
		assert(err == nil, "Unable to allocate expression")
		
		expr.body = arr_lit
		
	case:
		values, value_err := parse_expression_content(vm, state, scope, base_type)
		if value_err != nil do return nil, value_err
		
		if expr == nil do expr, err = ast_allocate_expr(vm, state)
		assert(err == nil, "Unable to allocate expression")
		
		expr.body = AST_RuntimeExpression {
			values = values
		}
	
	case ReferenceBody: unreachable()
	}
	
	// Universal assignments
	expr.type = base_type
	
	return
}
	
parse_expression_content :: proc(
	vm : VM, state : ^ParseState,
	scope : FRAME, type : TypeID,
	loc := #caller_location
) -> (expr : []AST_ExpressionValue, err : Error) {
	// ASSUME TYPE IS THE BASE TYPE OF ITSELF
	
	alloc, alloc_err := vm_get_ast_allocator(vm)
	if alloc_err != nil do return nil, alloc_err
	
	stage : enum {
		Start,
		Value,
		Prefix,
		Postfix,
		Operator,
	}
	
	values : [dynamic]AST_ExpressionValue
	defer delete(values)
	
	depth : int
	expr_loop: for ;; {
		token, text, token_err := get_next_token(vm, state)
		if token_err != nil do return nil, token_err
		
		switch stage {
		case .Start, .Operator, .Prefix:
			// Can be:
			// - prefix operator
			// - literal
			// - identifier
			//     * constant
			//     * variable
			//     * type (cast)
			//     * function !!! SKIP FOR NOW !!!
			// - left parentheses
		
			expectation_start := Expectation {
				positive = {
					TokenDelimiter {
						field = { .ParenL }
					},
					
					TokenOperator {},
					TokenLiteral {},
					TokenIdentifier {},
					
					TokenKeyword {
						field = {
							.Deref, .AsPtr,
						}
					}
				}
			}
			
			if !parse_expectations(token, expectation_start) {
				return nil, parser_error_emit(
					vm, state, .Token_Unexpected,
					"Invalid expression"
				)
			}
			
			#partial switch &b in token.body {
			case TokenOperator:
				if stage == .Prefix {
					return nil, parser_error_emit(
						vm, state, .Token_Unexpected,
						"Invalid expression"
					)
				}
				
				if b.type not_in prefix_operators {
					return nil, parser_error_emit(
						vm, state, .Token_Unexpected,
						"Given operator is not a prefix"
					)
				}
				
				append(&values, b.type)
				stage = .Prefix
				
			case TokenDelimiter:
				// Parentheses
				depth += 1
				append(&values, Delimiter.ParenL)
				stage = .Start
			
			case TokenLiteral:
				if token.value == nil do return nil, .Expression_Invalid
				
				append(&values, token.value)
				stage = .Value
			
			case TokenIdentifier:
				
				// Get identifier type
				#partial switch get_identifier_type(vm, text) {
				case .Function:
					// !!! TODO !!! fix this
					return nil, parser_error_emit(
						vm, state, .Unimplemented,
						"Function calls in expression aren't implemented yet"
					)
				
				case .Constant:
					// Get constant value
					const_id, id_err := get_const_id_by_name(vm, text)
					const, const_err := get_constant(vm, const_id)
					
					if const_err != nil || id_err != nil {
						return nil, parser_error_emit(
							vm, state, .Expression_Invalid,
							"Expected only valid constants in expression"
						)
					}
					
					append(&values, const.value)
					stage = .Value
					
				case .Type:
				
					// Get base type
					type_id, id_err := get_type_id_by_name(vm, text)
					base_type := get_base_type(vm, type_id, false)
					
					if base_type == -1 || id_err != nil {
						return nil, parser_error_emit(
							vm, state, .Expression_Invalid,
							"Expected only valid type casts in expression"
						)
					}
					
					if base_type != type {
						return nil, parser_error_emit(
							vm, state, .Type_Mismatch,
							"Types don't match in expression"
						)
					}
					
					val, val_err := parse_type_cast_expr(vm, state, scope, base_type)
					if val_err != nil do return nil, val_err
					
					append(&values, val)
					stage = .Value
					
				case:
					// Must be a variable
					
					var_id, var, found := parse_var_id(scope, text)
					if !found {
						return nil, parser_error_emit(
							vm, state, .Expression_Invalid,
							"Expected only valid identifiers in expression"
						)
					}
					
					val, val_err := parse_var_expr(vm, state, scope, type, var_id, var.type)
					if val_err != nil do return nil, val_err
					
					append(&values, val)
					stage = .Value
				}
			
			case TokenKeyword:
				// Builtins
				
				#partial switch b.type {
				case .Deref:
					deref_val, deref_err := parse_deref_expr(vm, state, scope, type)
					if deref_err != nil do return nil, deref_err
					
					append(&values, deref_val)
					stage = .Value
				
				case .AsPtr:
					asptr_val, asptr_err := parse_as_ptr_expr(vm, state, scope, type)
					if asptr_err != nil do return nil, asptr_err
					
					append(&values, asptr_val)
					stage = .Value
				
				case: unreachable()
				}
			
			case: unreachable()
			}
		
		case .Value, .Postfix:
			// Can be:
			// - operator
			//     * normal
			//     * postfix
			// - delimiter (right paren, else close)
			
			expectation_value := Expectation {
				positive = {
					TokenOperator {},
					TokenDelimiter {}
				},
				
				negative = {
					TokenDelimiter {
						field = { .ParenL }
					}
				}
			}
			
			if !parse_expectations(token, expectation_value) {
				return nil, parser_error_emit(
					vm, state, .Token_Unexpected,
					"Expected an operator or \")\" in expression"
				)
			}
			
			#partial switch &b in token.body {
			case TokenOperator:
				if get_precedence(b.type) == 0 {
					return nil, parser_error_emit(
						vm, state, .Token_Unexpected,
						"Expected a valid operator in expression"
					)
				}
				
				append(&values, b.type)
				
				if b.type in postfix_operators {
					if stage == .Postfix {
						return nil, parser_error_emit(
							vm, state, .Expression_Invalid,
							"Invalid expression"
						)
					}
					
					stage = .Postfix
					
				} else {
					
					stage = .Operator
				}
				
			case TokenDelimiter:
				#partial switch b.type {
				case .ParenR:
					if depth == 0 {
						break expr_loop
					}
					
					depth -= 1
					append(&values, Delimiter.ParenR)
					stage = .Value
				
				case:
					if depth != 0 {
						return nil, parser_error_emit(
							vm, state, .Expression_Depth,
							"Expression parentheses don't match"
						)
					}
				
					break expr_loop
				}
			}
		}
	}
	
	// Reverse single
	state.token -= 1
	
	expr, err = slice.clone(values[:], alloc)
	assert(err == nil, "Failed to clone expression slice")

	return
}

parse_var_expr :: proc(
	vm : VM, state : ^ParseState,
	scope : FRAME, type : TypeID, var : VarID, var_type : TypeID
) -> (var_val : AST_VarExpr, err : Error) {
	
	res_type := var_type
	off   : uintptr
	stage : enum {
		Value,
		Selector
	}
	
	offsets : [dynamic]AST_OffsetValue
	defer delete(offsets)
	
	select_loop: for ;; {
		base_type := get_base_type(vm, res_type, false)
		res_type = base_type
		
		type_body, type_err := get_type(vm, base_type)
		#partial switch &b in type_body.body {
		case StructBody:
		
			switch stage {
			case .Value:
			
				// Selector or break
				token, text, token_err := get_next_token(vm, state)
				if token_err != nil do return {}, token_err
				
				#partial switch &t in token.body {
				case TokenDelimiter:
					if t.type != .Period {
						
						// Reverse single
						state.token -= 1
						break select_loop
					}
					
					stage = .Selector
					
				case:
					// Not a selector
	
					// Reverse single
					state.token -= 1
					break select_loop
				}
			
			case .Selector:
				// Expect a member identifier
				
				token, text, token_err := get_next_token(vm, state)
				if token_err != nil do return {}, token_err
				
				if text == "" || text == "_" {
					return {}, parser_error_emit(
						vm, state, .Unknown_Member,
						"Invalid struct member identifier"
					)
				}
				
				// Find correct member
				idx := -1; for m, i in b.members {
					(m.name == text) or_continue
					
					idx = i
					break
				}
				
				if idx == -1 {
					return {}, parser_error_emit(
						vm, state, .Unknown_Member,
						"Invalid struct member identifier"
					)
				}
				
				// Apply changes
				member  := b.members[idx]
				res_type = member.base_type 
				off     += member.offset
				
				stage = .Value
			
				// Detect access
				if !ast_can_access_data(vm, scope, base_type) {
					return {}, parser_error_emit(
						vm, state, .Invalid_Access,
						"Trying to access opaque data in non-raw scope"
					)
				}
			}
			
		case ArrayBody:
			assert(stage == .Value, "Incorrect stage in array variable member parsing")
			
			// STAGE 0: Expect "[" or break
			token, text, token_err := get_next_token(vm, state)
			if token_err != nil do return {}, token_err
				
			#partial switch &t in token.body {
			case TokenDelimiter:
				if t.type != .SquareL {
					
					// Reverse single
					state.token -= 1
					break select_loop
				}
				
			case:
				// Not a selector

				// Reverse single
				state.token -= 1
				break select_loop
			}
			
			// STAGE 1: Expect index
			expr, expr_err := parse_expression(vm, state, scope, -1)
			if expr_err != nil do return {}, expr_err
			single_offset := mem.align_forward_int(b.size, b.align)
			
			#partial switch &t in expr.body {
			case AST_ConstantValue:
				
				idx := as_int(t.value)
				
				// Compile time bounds check
				if idx < 0 || idx >= b.size {
					return {}, parser_error_emit(
						vm, state, .Bounds_Check,
						"Given index doesn't fit within bounds of the array"
					)
				}
				
				multi_offset  := idx * single_offset
				off += uintptr(multi_offset)
			
			case:
				
				// Append offset
				if off != 0 {
					append(&offsets, off)
					off = 0
				}
				
				// Add dynamic offset
				append(&offsets, AST_OffsetExpr {
					ceil = b.size,
					
					size = single_offset,
					expr = expr
				})
			}
			
			res_type = b.base_type
			
			// STAGE 2: Expect "]"
			if !parse_util_single_token(
				vm, state, TokenDelimiter { type = .SquareR },
				"Expected \"]\" after array index selector"
			) { return {}, .Expression_Invalid }
			
			// Detect access
			if !ast_can_access_data(vm, scope, base_type) {
				return {}, parser_error_emit(
					vm, state, .Invalid_Access,
					"Trying to access opaque data in non-raw scope"
				)
			}
			
		case: break select_loop
		}
	}
	
	if res_type != type {
		return {}, parser_error_emit(
			vm, state, .Type_Mismatch,
			"Variable and expression types don't match"
		)
	}
	
	offset : AST_Offset
	if len(offsets) == 0 {
		offset.single = off
	} else {
		
		alloc, alloc_err := vm_get_ast_allocator(vm)
		if alloc_err != nil do return {}, alloc_err
		
		offset.values = slice.clone(offsets[:], alloc)
	}
	
	// Build result
	var_val.var  = var
	var_val.off  = offset
	var_val.type = type
	
	return
}

parse_type_cast_expr :: proc(
	vm : VM, state : ^ParseState,
	scope : FRAME, type : TypeID,
	loc := #caller_location
) -> (cast_val : AST_ExpressionValue, err : Error) {
	
	// STAGE 0: Expect "("
	if !parse_util_single_token(
		vm, state, TokenDelimiter { type = .ParenL },
		"Expected \"(\" in type cast expression"
	) { return {}, .Token_Unexpected }
	
	// STAGE 1: Expect typename
	cast_type, found_type := parse_util_type(vm, state, "in type cast expression")
	if !found_type do return {}, .Token_Unexpected
	
	// STAGE 2: Expect ","
	if !parse_util_single_token(
		vm, state, TokenDelimiter { type = .Comma },
		"Expected \",\" after type in type cast expression"
	) { return {}, .Token_Unexpected }
	
	// STAGE 3: Expect expression
	expr : AST_Expression; expr_err : Error
	_, expr_err  = parse_expression(vm, state, scope, cast_type, &expr)
	if expr_err != nil do return {}, expr_err
	
	// STAGE 4: Expect ")"
	if !parse_util_single_token(
		vm, state, TokenDelimiter { type = .ParenR },
		"Expected \")\" to close type cast expression"
	) { return {}, .Token_Unexpected }
	
	#partial switch &b in expr.body {
	case AST_ConstantValue:
	
		// Constants don't need casting
		return b.value, nil
		
	case AST_RuntimeExpression:
		
		if !types_can_cast(vm, expr.type, type) {
			return {}, parser_error_emit(
				vm, state, .Type_Mismatch,
				"Given types can't be cast to each other"
			)
		}
	
		// Construct Cast Expression
		return AST_CastExpr {
			values = b.values,
			type = type
		}, nil
	
	case:
		// This *should* ?? be unreachable
		unreachable()
	}
	
	return
}

parse_array_expr :: proc(
	vm : VM, state : ^ParseState,
	scope : FRAME, type : TypeID,
	loc := #caller_location
) -> (arr_val : AST_ExpressionBody, err : Error) {
	token, text, token_err := get_next_token(vm, state)
	if token_err != nil do return nil, token_err
	
	// STAGE 0: Expect type or "{"
	expectation_start := Expectation {
		positive = {
			TokenDelimiter {
				field = { .CurlyL }
			},
			
			TokenIdentifier {}
		}
	}
	
	if !parse_expectations(token, expectation_start) {
		return nil, parser_error_emit(
			vm, state, .Token_Unexpected,
			"Expected typename, variable or \"{\" to start array literal"
		)
	}
	
	#partial switch &b in token.body {
	case TokenDelimiter:
	
		// Parse Literal
		arr_val, err = parse_array_literal_expr(vm, state, scope, type)
		if err != nil do return
	
		// Expect "}"
		if !parse_util_single_token(
			vm, state, TokenDelimiter{ type = .CurlyR },
			"Expected \"}\" in array literal"
		) { return nil, .Token_Unexpected }
			
		// Detect access
		if !ast_can_access_data(vm, scope, type) {
			return nil, parser_error_emit(
				vm, state, .Invalid_Access,
				"Trying to access opaque data in non-raw scope"
			)
		}
		
	case TokenIdentifier:
		
		#partial switch get_identifier_type(vm, text) {
		case .Type:
			
			type_id, type_err := get_type_id_by_name(vm, text)
			if get_base_type(vm, type_id, false) != type {
				return nil, parser_error_emit(
					vm, state, .Type_Mismatch,
					"Types don't match in array literal"
				)
			}
			
			// Expect "{"
			if !parse_util_single_token(
				vm, state, TokenDelimiter{ type = .CurlyL },
				"Expected \"{\" in array literal"
			) { return nil, .Token_Unexpected }
			
			// Parse Literal
			arr_val, err = parse_array_literal_expr(vm, state, scope, type)
			if err != nil do return
	
			// Expect "}"
			if !parse_util_single_token(
				vm, state, TokenDelimiter{ type = .CurlyR },
				"Expected \"}\" in array literal"
			) { return nil, .Token_Unexpected }
			
			// Detect access
			if !ast_can_access_data(vm, scope, type) {
				return nil, parser_error_emit(
					vm, state, .Invalid_Access,
					"Trying to access opaque data in non-raw scope"
				)
			}
			
		case .Function:
			return nil, parser_error_emit(
				vm, state, .Unimplemented,
				"Function calls in array expressions aren't implemented"
			)
		
		case .Variable,
			.Unknown:
			
			// Get variable id
			var_id, var, found := parse_var_id(scope, text)
			if !found {
				return {}, parser_error_emit(
					vm, state, .Invalid_Name,
					"Expected a valid variable identifier in array expression"
				)
			}
			
			var_val, var_err := parse_var_expr(vm, state, scope, type, var_id, var.type)
			if var_err != nil do return {}, var_err
			
			type_body, type_err := get_type(vm, type)
			if type_err != nil {
				return {}, parser_error_emit(
					vm, state, .Unknown_Type,
					"Unable to determine type of array expression"
				)
			}
			
			arr_val = AST_MemoryAddress {
				var  = var_id,
				addr = var_val.off,
				size = type_body.size
			}
		
		case:
			return nil, parser_error_emit(
				vm, state, .Expression_Invalid,
				"Invalid array literal expression"
			)
		}
	}
	
	return
}

parse_array_literal_expr :: proc(
	vm : VM, state : ^ParseState,
	scope : FRAME, type : TypeID,
	loc := #caller_location
) -> (arr_val : AST_ArrayLiteral, err : Error) {
	
	// Get array literal type
	type_body, type_err := get_type(vm, get_base_type(vm, type, true))
	if type_err != nil do return {}, type_err
	
	array_body  := type_body.body.(ArrayBody) or_else unreachable()
	member_type := get_base_type(vm, array_body.base_type, false)
	
	values : [dynamic]AST_ArrayMember
	defer delete(values)
	
	parse_loop: for ;; {
		
		// Check for closing
		next, _, _ := get_next_token(vm, state, false)
		if delim, ok := next.body.(TokenDelimiter); ok {
			if delim.type == .CurlyR {
				
				// Consume token
				state.token += 1
				break parse_loop
			}
		}
		
		// Parse expression
		member_expr, expr_err := parse_expression(vm, state, scope, member_type)
		if expr_err != nil do return {}, expr_err
		
		append(&values, AST_ArrayMember {
			val = member_expr,
		})
		
		// Expect "," or "}"	
		token, text, token_err := get_next_token(vm, state)
		if token_err != nil do return {}, token_err
		
		expectation_member := Expectation {
			positive = {
				TokenDelimiter {
					field = { .Comma, .CurlyR }
				}
			}
		}
		
		if !parse_expectations(token, expectation_member) {
			return {}, parser_error_emit(
				vm, state, .Token_Unexpected,
				"Expected \",\" between members in array literal or \"}\" to end the literal"
			)
		}
		
		#partial switch (token.body.(TokenDelimiter) or_else unreachable()).type {
		case .Comma:  // Continue
		case .CurlyR: // End
			break parse_loop
		
		case: unreachable()
		}
	}
	
	if len(values) != array_body.size {
		return {}, parser_error_emit(
			vm, state, .Invalid_Array_Size,
			"Array size doesn't match with provided literal size"
		)
	}
	
	// Reverse single
	state.token -= 1
	
	// Generate literal
	alloc, alloc_err := vm_get_ast_allocator(vm)
	if alloc_err != nil do return {}, alloc_err
	
	arr_val.values = slice.clone(values[:], alloc)
	return
}

parse_struct_expr :: proc(
	vm : VM, state : ^ParseState,
	scope : FRAME, type : TypeID,
	loc := #caller_location
) -> (struct_val : AST_ExpressionBody, err : Error) {
	token, text, token_err := get_next_token(vm, state)
	if token_err != nil do return nil, token_err
	
	// STAGE 0: Expect type or "{"
	expectation_start := Expectation {
		positive = {
			TokenDelimiter {
				field = { .CurlyL }
			},
			
			TokenIdentifier {}
		}
	}
	
	if !parse_expectations(token, expectation_start) {
		return nil, parser_error_emit(
			vm, state, .Token_Unexpected,
			"Expected typename, variable or \"{\" to start struct literal"
		)
	}
	
	#partial switch &b in token.body {
	case TokenDelimiter:
	
		// Parse Literal
		struct_val, err = parse_struct_literal_expr(vm, state, scope, type)
		if err != nil do return
	
		// Expect "}"
		if !parse_util_single_token(
			vm, state, TokenDelimiter{ type = .CurlyR },
			"Expected \"}\" in struct literal"
		) { return nil, .Token_Unexpected }
		
		// Detect access
		if !ast_can_access_data(vm, scope, type) {
			return nil, parser_error_emit(
				vm, state, .Invalid_Access,
				"Trying to access opaque data in non-raw scope"
			)
		}
		
	case TokenIdentifier:
		
		#partial switch get_identifier_type(vm, text) {
		case .Type:
			type_id, type_err := get_type_id_by_name(vm, text)
			if get_base_type(vm, type_id, false) != type {
				return nil, parser_error_emit(
					vm, state, .Type_Mismatch,
					"Types don't match in struct literal"
				)
			}
			
			// Expect "{"
			if !parse_util_single_token(
				vm, state, TokenDelimiter{ type = .CurlyL },
				"Expected \"{\" in struct literal"
			) { return nil, .Token_Unexpected }
			
			// Parse Literal
			struct_val, err = parse_struct_literal_expr(vm, state, scope, type)
			if err != nil do return
	
			// Expect "}"
			if !parse_util_single_token(
				vm, state, TokenDelimiter{ type = .CurlyR },
				"Expected \"}\" in struct literal"
			) { return nil, .Token_Unexpected }
			
			// Detect access
			if !ast_can_access_data(vm, scope, type) {
				return nil, parser_error_emit(
					vm, state, .Invalid_Access,
					"Trying to access opaque data in non-raw scope"
				)
			}
			
		case .Function:
			return nil, parser_error_emit(
				vm, state, .Unimplemented,
				"Function calls in struct expressions aren't implemented"
			)
		
		case .Variable,
			.Unknown:
			
			// Get variable id
			var_id, var, found := parse_var_id(scope, text)
			if !found {
				return {}, parser_error_emit(
					vm, state, .Invalid_Name,
					"Expected a valid variable identifier in struct expression"
				)
			}
			
			var_val, var_err := parse_var_expr(vm, state, scope, type, var_id, var.type)
			if var_err != nil do return {}, var_err
			
			type_body, type_err := get_type(vm, type)
			if type_err != nil {
				return {}, parser_error_emit(
					vm, state, .Unknown_Type,
					"Unable to determine type of struct expression"
				)
			}
			
			struct_val = AST_MemoryAddress {
				var  = var_id,
				addr = var_val.off,
				size = type_body.size
			}
		
		case:
			return nil, parser_error_emit(
				vm, state, .Expression_Invalid,
				"Invalid struct literal expression"
			)
		}
	}
	
	return
}

parse_struct_literal_expr :: proc(
	vm : VM, state : ^ParseState,
	scope : FRAME, type : TypeID,
	loc := #caller_location
) -> (struct_val : AST_StructLiteral, err : Error) {
	
	// Get array literal type
	type_body, type_err := get_type(vm, get_base_type(vm, type, true))
	if type_err != nil do return {}, type_err
	
	struct_body := type_body.body.(StructBody) or_else unreachable()
	
	values : [dynamic]AST_StructMember
	defer delete(values)
	
	parse_loop: for ;; {
		
		// Check for closing
		next, _, _ := get_next_token(vm, state, false)
		if delim, ok := next.body.(TokenDelimiter); ok {
			if delim.type == .CurlyR {
				
				// Consume token
				state.token += 1
				break parse_loop
			}
		}
		
		// STAGE 0: Get identifier
		token, text, token_err := get_next_token(vm, state)
		if token_err != nil do return {}, token_err
		
		if !parse_expectations(token, expectation_identifier) {
			return {}, parser_error_emit(
				vm, state, .Token_Unexpected,
				"Expected member identifier in struct literal"
			)
		}
		
		if text == "" || text == "_" {
			return {}, parser_error_emit(
				vm, state, .Unknown_Member,
				"Invalid struct member identifier"
			)
		}
		
		idx := -1; for m, i in struct_body.members {
			(m.name == text) or_continue
			
			idx = i
			break
		}
		
		if idx == -1 {
			return {}, parser_error_emit(
				vm, state, .Unknown_Member,
				"Invalid struct member identifier"
			)
		}
		
		member := struct_body.members[idx]
		
		for m in values {
			(m.idx == idx) or_continue
			
			// Given name already exists in values
			return {}, parser_error_emit(
				vm, state, .Struct_Member_Over,
				"Overwriting existing member value in struct literal"
			)
		}
		
		// STAGE 1: Expect "="
		if !parse_util_single_token(
			vm, state, TokenOperator { type = .Equals },
			"Expected \"=\" after struct member identifier"
		) { return {}, .Token_Unexpected }
		
		// STAGE 2: Parse expression
		member_expr, expr_err :=
			parse_expression(vm, state, scope, get_base_type(vm, member.base_type, false))
		if expr_err != nil do return {}, expr_err
		
		append(&values, AST_StructMember {
			idx = idx,
			val = member_expr,
		})
		
		// Expect "," or "}"
		token, text, token_err = get_next_token(vm, state)
		if token_err != nil do return {}, token_err
		
		expectation_member := Expectation {
			positive = {
				TokenDelimiter {
					field = { .Comma, .CurlyR }
				}
			}
		}
		
		if !parse_expectations(token, expectation_member) {
			return {}, parser_error_emit(
				vm, state, .Token_Unexpected,
				"Expected \",\" between members in struct literal or \"}\" to end the literal"
			)
		}
		
		#partial switch (token.body.(TokenDelimiter) or_else unreachable()).type {
		case .Comma:  // Continue
		case .CurlyR: // End
			break parse_loop
		
		case: unreachable()
		}
	}
	
	// Reverse single
	state.token -= 1
	
	// Generate literal
	alloc, alloc_err := vm_get_ast_allocator(vm)
	if alloc_err != nil do return {}, alloc_err
	
	struct_val.values = slice.clone(values[:], alloc)
	return
}

parse_deref_expr :: proc(
	vm : VM, state : ^ParseState,
	scope : FRAME, type : TypeID
) -> (deref_val : AST_DerefExpr, err : Error) {
	
	// STAGE 0: Expect "("
	if !parse_util_single_token(
		vm, state, TokenDelimiter { type = .ParenL },
		"Expected \"(\" after deref builtin"
	) { return {}, .Token_Unexpected }
	
	// STAGE 1: Expect type name
	ptr_type, type_found := parse_util_type(vm, state, " in deref expression")
	if !type_found do return {}, .Token_Unexpected
	
	// STAGE 2: Expect ","
	if !parse_util_single_token(
		vm, state, TokenDelimiter { type = .Comma },
		"Expected \",\" after type name in deref expression"
	) { return {}, .Token_Unexpected }
	
	// STAGE 3: Expect ptr expression
	type_body, type_err := get_type(vm, get_base_type(vm, ptr_type, true))
	if type_err != nil {
		return {}, parser_error_emit(
			vm, state, .Invalid_Type,
			"Unable to determine type in pointer deref"
		)
	}
	
	#partial switch &t in type_body.body {
	case PointerBody:
		if 	get_base_type(vm, t.base_type, false) !=
			get_base_type(vm, type, false) {
			return {}, parser_error_emit(
				vm, state, .Type_Mismatch,
				"Dereference and expression types don't match"
			)
		}
		
	case:
		return {}, parser_error_emit(
			vm, state, .Not_A_Pointer,
			"Trying to dereference a non-pointer type"
		)
	}
	
	// Parse pointer expression
	expr : AST_Expression
	_, p_expr_err := parse_expression(vm, state, scope, ptr_type, &expr)
	if p_expr_err != nil do return {}, p_expr_err
	
	// STAGE 2: Expect ")"
	if !parse_util_single_token(
		vm, state, TokenDelimiter { type = .ParenR },
		"Expected \")\" after deref builtin"
	) { return {}, .Token_Unexpected }
	
	// Pointers allowed only in raw scopes
	if !ast_scope_is_raw(scope) {
		return {}, parser_error_emit(
			vm, state, .Disallowed,
			"Must be in raw scope to use deref builtin"
		)
	}
	
	deref_val.value = expr.body
	return
}

parse_as_ptr_expr :: proc(
	vm : VM, state : ^ParseState,
	scope : FRAME, type : TypeID
) -> (ptr_val : AST_AsPtrExpr, err : Error) {
	
	// STAGE 0: Expect "("
	if !parse_util_single_token(
		vm, state, TokenDelimiter { type = .ParenL },
		"Expected \"(\" after as_ptr builtin"
	) { return {}, .Token_Unexpected }
	
	// STAGE 1: Expect type name
	// NOTE: this isn't strictly necessary
	//		 since we get the necessary id
	//		 as a parameter...   but we'll
	//		 keep it for consistancy sake
	ptr_type, type_found := parse_util_type(vm, state, "in as_ptr expression")
	if !type_found do return {}, .Token_Unexpected
	
	if ptr_type != type {
		return {}, parser_error_emit(
			vm, state, .Type_Mismatch,
			"Expression and as_ptr builtin types don't match"
		)
	}
	
	// Check base type
	type_body, type_err := get_type(vm, get_base_type(vm, ptr_type, true))
	if type_err != nil do return {}, type_err
	
	base_type : TypeID
	#partial switch &b in type_body.body {
	case PointerBody:
		base_type = get_base_type(vm, b.base_type, false)
		
	case:
		return {}, parser_error_emit(
			vm, state, .Not_A_Pointer,
			"Type name in as_ptr builtin must be a valid pointer"
		)
	}
	
	if base_type == -1 {
		return {}, parser_error_emit(
			vm, state, .Unknown_Type,
			"Unable to get base type in as_ptr expression"
		)
	}
	
	// STAGE 2: Expect ","
	if !parse_util_single_token(
		vm, state, TokenDelimiter { type = .Comma },
		"Expected \",\" after type name in as_ptr builtin"
	) { return {}, .Token_Unexpected }
	
	// STAGE 3: Parse variable
	token, text, token_err := get_next_token(vm, state)
	if token_err != nil do return {}, token_err
	
	var_id, var, found := parse_var_id(scope, text)
	if !found {
		return {}, parser_error_emit(
			vm, state, .Invalid_Name,
			"Expected a valid variable identifier in as_ptr expression"
		)
	}
	
	var_val, var_err := parse_var_expr(vm, state, scope, base_type, var_id, var.type)
	if var_err != nil do return {}, var_err
	
	// STAGE 4: Expect ")"
	if !parse_util_single_token(
		vm, state, TokenDelimiter { type = .ParenR },
		"Expected \")\" to end as_ptr builtin"
	) { return {}, .Token_Unexpected }
	
	// Finished, generate AST
	ptr_val = {
		var  = var_val,
		type = type
	}
	
	// Check mutability
	if !var.mutable {
		return {}, parser_error_emit(
			vm, state, .Disallowed,
			"Unable to get the pointer of an immutable variable"
		)
	}
	
	// Check rarity
	if !ast_scope_is_raw(scope) {
		return {}, parser_error_emit(
			vm, state, .Disallowed,
			"Must be in raw scope to use as_ptr builtin"
		)
	}
	
	return
}

parse_alloc_expr :: proc(
	vm : VM, state : ^ParseState,
	scope : FRAME, type : TypeID
) -> (alloc_val : Variant, err : Error) {
	
	// STAGE 0: Expect "("
	if !parse_util_single_token(
		vm, state, TokenDelimiter { type = .ParenL },
		"Expected \"(\" after alloc builtin"
	){ return nil, .Token_Unexpected }
	
	// STAGE 1: Expect type
	base_type, found := parse_util_type(vm, state, "Expected type in alloc builtin")
	if !found do return nil, .Token_Unexpected
	
	// STAGE 2: Expect ","
	if !parse_util_single_token(
		vm, state, TokenDelimiter { type = .Comma },
		"Expected \",\" after type in alloc builtin"
	){ return nil, .Token_Unexpected }
	
	// STAGE 3: Expect constant expression
	size, expr_err := parse_constant_expression(vm, state)
	if expr_err != nil do return nil, expr_err
	
	// STAGE 4: Expect ")"
	if !parse_util_single_token(
		vm, state, TokenDelimiter { type = .ParenR },
		"Expected \")\" after alloc builtin"
	){ return nil, .Token_Unexpected }
	
	// Complete, check semantics and allocate
	type_body, type_err := get_type(vm, get_base_type(vm, type, true))
	if type_err != nil {
		return nil, parser_error_emit(
			vm, state, .Unknown_Type,
			"Unable to resolve pointer type"
		)
	}
	
	#partial switch &b in type_body.body {
	case PointerBody:
		base_type = get_base_type(vm, base_type, false)
		if b.base_type != base_type {
			return nil, parser_error_emit(
				vm, state, .Type_Mismatch,
				"Variable type is not a pointer to the type given in alloc builtin"
			)
		}
	
	case:
		return nil, parser_error_emit(
			vm, state, .Type_Mismatch,
			"Trying to assign allocation result to a non-pointer type"
		)
	}
	
	// Allocate
	num_elems := as_int(size); if num_elems <= 0 {
		return nil, parser_error_emit(
			vm, state, .Invalid_Size,
			"Trying to allocate n <= 0 elements of given type"
		)
	}
	
	type_body, type_err = get_type(vm, get_base_type(vm, base_type, true))
	if type_err != nil {
		return nil, parser_error_emit(
			vm, state, .Unknown_Type,
			"Unable to determine base type in alloc builtin"
		)
	}
	
	single_size := mem.align_forward_int(type_body.size, type_body.align)
	alloc_size  := type_body.size if num_elems == 1 else single_size * num_elems
	
	ptr, alloc_err := heap_allocate(vm, alloc_size, type_body.align, base_type)
	if alloc_err != nil {
		return nil, parser_error_emit(
			vm, state, alloc_err,
			"Unable to complete memory allocation"
		)
	}
	
	return ptr, nil
}

// -   Types   -
parse_type :: proc(vm : VM, state : ^ParseState) -> (err : Error) {
	token, text, token_err := get_next_token(vm, state)
	if token_err != nil do return token_err
	
	temp_alloc := context.temp_allocator
	
	// EXAMPLE SYNTAX:
	// #type MyPointer   = ^int;
	// #type MyReference = int;
	// #type MyUniqueRef = unique int;
	// #type MyArray     = [10]int;
	// #type MyStruct    = struct {
	//     a : int,
	//     b : float,
	// }
	
	// STAGE 0: Expect identifier
	if !parse_expectations(token, expectation_identifier) {
		return parser_error_emit(
			vm, state, .Token_Unexpected,
			"Expected an identifier after #type declaration"
		)
	}
	
	identifier := text
	switch get_identifier_type(vm, identifier) {
	case .Type:
		return parser_error_emit(
			vm, state, .Type_Over,
			"Trying to declare over existing type"
		)
	
	case .Function,
		 .Variable,
		 .Constant:

		return parser_error_emit(
			vm, state, .Invalid_Name,
			"Trying to declare over existing identifier"
		)
	
	case .Unknown:
	}
	
	// STAGE 1: Expect "="
	if !parse_util_equals(vm, state, "after a type identifier") do return .Token_Unexpected
	
	// STAGE 2: Expect type selector
	token, text, token_err = get_next_token(vm, state)
	if token_err != nil do return token_err
	
	if !parse_expectations(token, expectation_variable_typedef) {
		return parser_error_emit(
			vm, state, .Token_Unexpected,
			"Expected type body declaration"
		)
	}
	
	// Type factory
	type : Type
	
	#partial switch &b in token.body {
	case TokenOperator:
		// We know this is a pointer
		err = parse_pointer_type(vm, state, &type)
	
	case TokenKeyword:
		// Either a struct or a unique reference
		#partial switch b.type {
		case .Struct:
			err = parse_struct_type(vm, state, &type, temp_alloc)
		
		case .Unique:
			err = parse_unique_reference_type(vm, state, &type)
		
		case: unreachable()
		}
		
	case TokenDelimiter:
		// We know this is a SquareL (array type)
		err = parse_array_type(vm, state, &type)
	
	case TokenIdentifier:
	
		// A non-unique type reference
		id, id_err := get_type_id_by_name(vm, text)
		base_type, type_err := get_type(vm, id)
		
		if id_err != nil || type_err != nil {
			parser_error_emit(
				vm, state, .Unknown_Type,
				"Expected a valid type identifier for reference type definition"
			)
		}
		
		// --- Type Generation ---
		type.body = ReferenceBody {
			base_type = get_base_type(vm, id, false)
		}
		
		// Size and alignment
		type.size  = base_type.size
		type.align = base_type.align
	
	case: unreachable()
	};	if err != nil do return
	
	// STAGE 3: Expect Terminator ";"
	if !parse_util_terminator(vm, state) do return .Token_Unexpected
	
	// --- Register Type ---
	if type.body == nil do return .Invalid_Type
	
	type.name = identifier
	fmt.println("Type", identifier, "=", type.body)
	
	type_id, type_err := register_type(vm, type)
	return type_err
}

parse_pointer_type :: proc(vm : VM, state : ^ParseState, type : ^Type) -> (err : Error) {
	token, text, token_err := get_next_token(vm, state)
	if token_err != nil do return token_err
	
	// EXAMPLE SYNTAX:
	// ^int
	
	// STATE 0: Expect identifier
	id, id_err := get_type_id_by_name(vm, text)
	if id_err != nil {
		return parser_error_emit(
			vm, state, .Unknown_Type,
			"Expected a valid type identifier for pointer type definition"
		)
	}
	
	// --- Type Generation ---
	type.body = PointerBody {
		base_type = id
	}
	
	// Size and alignment
	type.size  = 8
	type.align = 8
	
	return
}

parse_unique_reference_type :: proc(vm : VM, state : ^ParseState, type : ^Type) -> (err : Error) {
	token, text, token_err := get_next_token(vm, state)
	if token_err != nil do return token_err
	
	// EXAMPLE SYNTAX:
	// unique int
	
	// STATE 0: Expect identifier
	id, id_err := get_type_id_by_name(vm, text)
	base_type, type_err := get_type(vm, id)
	
	if id_err != nil || type_err != nil {
		return parser_error_emit(
			vm, state, .Unknown_Type,
			"Expected a valid type identifier for unique reference type definition"
		)
	}
	
	// --- Type Generation ---
	type.body = ReferenceBody {
		base_type = get_base_type(vm, id, false),
		unique = true
	}
	
	// Size and alignment
	type.size  = base_type.size
	type.align = base_type.align
	
	return
}

parse_array_type :: proc(vm : VM, state : ^ParseState, type : ^Type) -> (err : Error) {
	
	// EXAMPLE SYNTAX:
	// [10]int
	// [MY_INTEGER_CONSTANT]int
	
	// STATE 0: Expect constant expression
	arr_size : int
	value, value_err := parse_constant_expression(vm, state)
	if value_err != nil do return value_err
	
	#partial switch t in value {
	case int,
		uint,
		uintptr:
		arr_size = as_int(value)
		
	case:
		return parser_error_emit(
			vm, state, .Expression_Invalid,
			"Expected an integer constant expression in array size declaration"
		)
	}
	
	// Check correct size
	if arr_size < 1 {
		return parser_error_emit(
			vm, state, .Invalid_Array_Size,
			"Expected an integer > 0 for array size declaration"
		)
	}
	
	// STATE 1: Expect "]"
	token, text, token_err := get_next_token(vm, state)
	if token_err != nil do return token_err
	
	if !parse_expectations(token, expectation_square_close) {
		return parser_error_emit(
			vm, state, .Token_Unexpected,
			"Expected closing square brackets \"]\" in array size declaration"
		)
	}
	
	// STATE 2: Expect type identifier
	token, text, token_err = get_next_token(vm, state)
	if token_err != nil do return token_err
	
	id, id_err := get_type_id_by_name(vm, text)
	base_type, type_err := get_type(vm, id)
	
	if id_err != nil || type_err != nil {
		return parser_error_emit(
			vm, state, .Unknown_Type,
			"Expected a valid type identifier for array type definition"
		)
	}
	
	// --- Type Generation ---
	type.body = ArrayBody {
		base_type = id,
		
		size  = arr_size,
		align = base_type.align
	}
	
	// Size and alignment
	type.size  = arr_size * mem.align_forward_int(
		base_type.size, base_type.align)
	type.align = base_type.align
	// NOTE: this feels incorrect???
	
	return
}

parse_struct_type :: proc(
	vm : VM, state : ^ParseState,
	type : ^Type, alloc := context.allocator
) -> (err : Error) {
	token, text, token_err := get_next_token(vm, state)
	if token_err != nil do return token_err
	
	// EXAMPLE SYNTAX:
	// struct {
	//     a : int,
	//     b : float,
	//     
	//     // Padding
	//     _ : int,
	//     
	// 	   // Nested structs
	//     thing : MyStructType,
	// };
	
	// STATE 0: Expect "{"
	if !parse_expectations(token, expectation_struct_curly) {
		return parser_error_emit(
			vm, state, .Token_Unexpected,
			"Expected \"{\" after struct keyword"
		)
	}
	
	// --- Struct Builder ---
	members : [dynamic]StructMember
	defer delete(members)
	
	stage : enum {
		Member,
		Comma,
	}
	
	member_loop: for ;; {
		// Loop until an exit
		// Through  a  CurlyR
		// Or an Error
		
		switch stage {
		case .Member:
			// STATE 1: Expect a member or end
			end, member_err := parse_struct_type_member(vm, state, &members)
			if member_err != nil do return; if end do break member_loop
			
			stage = .Comma
		
		case .Comma:
			token, text, token_err = get_next_token(vm, state)
			if token_err != nil do return token_err
			
			// STATE 2: Expect a comma  or end
			if !parse_expectations(token, expectation_curly_or_comma) {
				return parser_error_emit(
					vm, state, .Token_Unexpected,
					"Expected \",\" between struct members or \"}\""
				)
			}
			
			#partial switch token.body.(TokenDelimiter).type {
			case .Comma:
				stage = .Member
			
			case .CurlyR:
				break member_loop
			
			case: unreachable()
			}
			
		}
	}
	
	// --- Type Generation ---
	body : StructBody
	
	i, size, align : int
	for &m in members {
		type, type_err := get_type(vm, m.base_type)
		if type_err != nil do return type_err
		
		offset := mem.align_forward_int(i, type.align)
		m.offset = uintptr(offset)
		
		i = offset + type.size
		
		// NOTE: naive alignation calculation
		//		 might require a better method
		align = max(align, type.align)
		size  = offset + type.size
	}
	
	type.size  = mem.align_forward_int(size, align)
	type.align = align
	
	body.members = slice.clone(
		members[:], alloc)
	type.body = body
	
	return
}

parse_struct_type_member :: proc(
	vm : VM, state : ^ParseState,
	members : ^[dynamic]StructMember
) -> (end_loop : bool, err : Error) {
	token, text, token_err := get_next_token(vm, state)
	if token_err != nil do return false, token_err
	
	// EXAMPLE SYNTAX:
	// my_member : MyType
	
	// STATE 0: Expect an identifier or "}"
	if !parse_expectations(token, expectation_ident_or_curly) {
		return false, parser_error_emit(
			vm, state, .Token_Unexpected,
			"Expected struct member identifier or \"}\""
		)
	}
	
	#partial switch &b in token.body {
	case TokenDelimiter:
		// Must be a CurlyR
		return true, nil
	
	case TokenIdentifier:
		// Handle logic outside of switch
	
	case: unreachable()
	}
	
	identifier := text
	if identifier == "" do return false, .Struct_Member_None
	
	// Check other members
	TOKEN_UNNAMED_FIELD :: "_"
	if identifier != TOKEN_UNNAMED_FIELD do for m in members {
		if m.name == identifier do return false, .Struct_Member_Over
	}
	
	// STATE 1: Expect colon
	if !parse_util_single_token(
		vm, state, TokenOperator { type = .Colon },
		"Expected \":\" after struct member identifier"
	) { return false, .Token_Unexpected }
	
	// STATE 2: Expect type
	token, text, token_err = get_next_token(vm, state)
	if token_err != nil do return false, token_err
	
	if !parse_expectations(token, expectation_identifier) {
		return false, parser_error_emit(
			vm, state, .Token_Unexpected,
			"Expected struct member type identifier"
		)
	}
	
	id, id_err := get_type_id_by_name(vm, text)
	base_type, type_err := get_type(vm, id)
	
	if id_err != nil || type_err != nil {
		return false, parser_error_emit(
			vm, state, .Unknown_Type,
			"Expected valid struct member type"
		)
	}
	
	// --- Member Generation ---
	member : StructMember
	member.name = identifier
	member.base_type = id
	
	append(members, member)
	return
}

// --- Variables ---
parse_variable :: proc(vm : VM, state : ^ParseState, scope : FRAME) -> (err : Error) {
	token, text, token_err := get_next_token(vm, state)
	if token_err != nil do return token_err
	
	// EXAMPLE SYNTAX:
	// var my_var : int = 2;
	// var other_var : bool; // Defaults to zero / false
	
	// STAGE 0: Expect identifier
	if !parse_expectations(token, expectation_identifier) {
		return parser_error_emit(
			vm, state, .Token_Unexpected,
			"Expected a variable identifier"
		)
	}
	
	// Check for existing identifiers
	variable : Variable
	variable.name = text
	switch get_identifier_type(vm, variable.name) {
	case .Function:
		return parser_error_emit(
			vm, state, .Token_Unexpected,
			"Variable name overshadows existing function"
		)
	
	case .Constant:
		return parser_error_emit(
			vm, state, .Token_Unexpected,
			"Variable name overshadows existing constant"
		)
		
	case .Type:
		return parser_error_emit(
			vm, state, .Token_Unexpected,
			"Variable name overshadows existing type"
		)
	
	case .Variable: unreachable()
	case .Unknown:
	}
	
	// Check existing variables
	if 	_, _, var_exists := parse_var_id(scope, variable.name);
		var_exists {
		return parser_error_emit(
			vm, state, .Token_Unexpected,
			"Variable name overshadows existing variable"
		)
	}
	
	// STAGE 1: Expect ":"
	if !parse_util_single_token(
		vm, state, TokenOperator{ type = .Colon },
		"Expected \":\" after variable name declaration"
	) { return .Token_Unexpected }
	
	// STAGE 2: Expect type or keyword, "immutable"
	token, text, token_err = get_next_token(vm, state)
	if token_err != nil do return token_err
	
	if !parse_expectations(token, expectation_variable_type_assign) {
		return parser_error_emit(
			vm, state, .Token_Unexpected,
			"Expected a typename or keyword in variable declaration"
		)
	}
	
	variable.mutable = true
	#partial switch &b in token.body {
	case TokenIdentifier:
		type_id, id_err := get_type_id_by_name(vm, text)
		if id_err != nil {
			return parser_error_emit(
				vm, state, .Unknown_Type,
				"Expected a valid type in variable declaration"
			)
		}
		
		variable.type = type_id
	
	case TokenKeyword:
		#partial switch b.type {
		case .Immutable:
			variable.mutable = false
			
		case: unreachable()
		}
		
		// NOW expect type
		token, text, token_err = get_next_token(vm, state)
		if token_err != nil do return token_err
		
		type_id, id_err := get_type_id_by_name(vm, text)
		if id_err != nil {
			return parser_error_emit(
				vm, state, .Unknown_Type,
				"Expected a valid type in variable declaration"
			)
		}
		
		variable.type = type_id
	
	case: unreachable()
	}
	
	// STAGE 3: Expect ";" or "="
	token, text, token_err = get_next_token(vm, state)
	if token_err != nil do return token_err
	
	if !parse_expectations(token, expectation_expr_or_end) {
		return parser_error_emit(
			vm, state, .Token_Unexpected,
			"Expected \";\" or \"=\" after variable type"
		)
	}
	
	expr     : EXPR
	expr_err : Error
	#partial switch &b in token.body {
	case TokenDelimiter:
		// Terminator
		
		if !variable.mutable {
			return parser_error_emit(
				vm, state, .Invalid_Variable,
				"Immutable variables must be assigned a value"
			)
		}
		
	case TokenOperator:
		
		// TODO: handle different variable types!
		base_id := get_base_type(vm, variable.type, true)
		base_type, type_err := get_type(vm, base_id)
		if type_err != nil do return type_err
		
		im_spec: if !variable.mutable {
			
			peek, text, peek_err := get_next_token(vm, state, false)
			if peek_err != nil do break im_spec
			
			kw := peek.body.(TokenKeyword) or_break im_spec
			#partial switch kw.type {
			case .Alloc:
				// Consume the token
				state.token += 1
				
				alloc_val, alloc_err := parse_alloc_expr(vm, state, scope, variable.type)
				if alloc_err != nil do return alloc_err
				
				// Create expression
				expr, expr_err = ast_allocate_expr(vm, state)
				assert(err == nil, "Unable to allocate expression")
				
				expr.body = AST_ConstantValue {
					value = alloc_val
				}
			
				// FOR NOW, LETS ALLOW STATIC ALLOCATIONS EVERYWHERE
				if scope.parent != nil && false {
					return parser_error_emit(
						vm, state, .Unimplemented,
						"Allocations outside of file scope are yet to be implemented"
					)
				}
				
			case: break im_spec
			}
			
			// Expect terminator
			if !parse_util_terminator(vm, state) {
				return .Token_Unexpected
			}
			
			break
		}
		
		// Expression
		expr, expr_err = parse_expression(vm, state, scope, variable.type)
		if expr_err != nil do return expr_err
		
		// Expect terminator
		if !parse_util_terminator(vm, state) {
			return .Token_Unexpected
		}
	}
	
	// Complete statement, generate AST
	var_id, var_err := ast_create_variable(
		vm, state, scope,
		variable
	)
	
	assert(var_err == nil, "Unable to create frame variable")
	fmt.println("Variable", variable.name, "=", variable)
	
	if expr == nil do return
	// If we have an expression, create assignation
	
	node, node_err := ast_allocate_node(vm, state, scope)
	if node_err != nil do unreachable() // Should be unreachable
	
	node.body = AST_Assign {
		
		op = .Equals,
		
		var  = var_id,
		type = variable.type,
		expr = expr,
	}
	
	return ast_append_node(vm, state, scope, node)
}

parse_deref :: proc(vm : VM, state : ^ParseState, scope : FRAME) -> (err : Error) {
	
	// STAGE 0: Expect "("
	if !parse_util_single_token(
		vm, state, TokenDelimiter { type = .ParenL },
		"Expected \"(\" after deref builtin"
	) { return .Token_Unexpected }
	
	// STAGE 1: Expect type name
	ptr_type, type_found := parse_util_type(vm, state, " in deref expression")
	if !type_found do return .Token_Unexpected
	
	// STAGE 2: Expect ","
	if !parse_util_single_token(
		vm, state, TokenDelimiter { type = .Comma },
		"Expected \",\" after type name in deref expression"
	) { return .Token_Unexpected }
	
	// STAGE 3: Expect ptr expression
	type_body, type_err := get_type(vm, get_base_type(vm, ptr_type, true))
	if type_err != nil {
		return parser_error_emit(
			vm, state, .Invalid_Type,
			"Unable to determine type in pointer deref"
		)
	}
	
	base_type : TypeID
	#partial switch &t in type_body.body {
	case PointerBody:
		base_type = t.base_type
		
	case:
		return parser_error_emit(
			vm, state, .Not_A_Pointer,
			"Trying to dereference a non-pointer type"
		)
	}
	
	// Parse pointer expression
	p_expr, p_expr_err := parse_expression(vm, state, scope, ptr_type)
	if p_expr_err != nil do return p_expr_err
	
	// STAGE 2: Expect ")"
	if !parse_util_single_token(
		vm, state, TokenDelimiter { type = .ParenR },
		"Expected \")\" after deref builtin"
	) { return .Token_Unexpected }
	
	// STAGE 3: Expect operator
	token, text, token_err := get_next_token(vm, state)
	if token_err != nil do return token_err
	
	expectation_operator := Expectation {
		positive = {
			TokenOperator {
				field = {
					.Equals,
					
					.AddEq, .SubEq,
					.MulEq, .DivEq,
				}
			}
		}
	}
	
	if !parse_expectations(token, expectation_operator) {
		return parser_error_emit(
			vm, state, .Token_Unexpected,
			"Expected assignation after pointer deref"
		)
	}
	
	type_body, type_err = get_type(vm, get_base_type(vm, base_type, true))
	if type_err != nil {
		return parser_error_emit(
			vm, state, .Unknown_Type,
			"Unable to determine dereferenced type"
		)
	}
	
	// Match operator to variable type
	op := (token.body.(TokenOperator) or_else unreachable()).type
	
	#partial switch &t in type_body.body {
	case ArrayBody,
		StructBody:
		(op != .Equals) or_break
		return parser_error_emit(
			vm, state, .Token_Unexpected,
			"Given assignation type not supported for given type"
		)
	}
	
	// STAGE 4: Expect expression
	expr, expr_err := parse_expression(vm, state, scope, base_type)
	if expr_err != nil do return expr_err
	
	// STAGE 5: Expect ";"
	if !parse_util_terminator(vm, state) do return .Token_Unexpected
	
	// Pointers allowed only in raw scopes
	if !ast_scope_is_raw(scope) {
		return parser_error_emit(
			vm, state, .Disallowed,
			"Must be in raw scope to use deref builtin"
		)
	}
	
	// Finished, generate AST
	node, node_err := ast_allocate_node(vm, state, scope)
	if node_err != nil do return node_err
	
	node.body = AST_DerefAssign {
		op = op,
		
		type = base_type,
		from = ptr_type,
		
		ptr  = p_expr,
		expr = expr,
	}
	
	err = ast_append_node(vm, state, scope, node)
	return
}

// --- Utils ---
parse_util_equals :: proc(vm : VM, state : ^ParseState, $M : string) -> bool {
	token, text, token_err := get_next_token(vm, state)
	if token_err != nil do return false
	
	if !parse_expectations(token, expectation_equals) {
		parser_error_emit(
			vm, state, .Token_Unexpected,
			"Expected \"=\" " + M
		)
		
		return false
	}
	
	return true
}

parse_util_terminator :: proc(vm : VM, state : ^ParseState, loc := #caller_location) -> bool {
	token, text, token_err := get_next_token(vm, state)
	if token_err != nil do return false
	
	if !parse_expectations(token, expectation_terminator) {
		parser_error_emit(
			vm, state, .Token_Unexpected,
			"Expected \";\" at the end of the statement"
		)
		when ODIN_DEBUG do fmt.println(loc)
		
		return false
	}
	
	return true
}

parse_util_single_token :: proc(vm : VM, state : ^ParseState, t : TokenBody($T), $M : string) -> bool {
	token, text, token_err := get_next_token(vm, state)
	if token_err != nil do return false
	
	token_raw, ok := token.body.(TokenBody(T))
	if !ok || token_raw.type != t.type {
		parser_error_emit(
			vm, state, .Token_Unexpected,
			M
		)
		
		return false
	}
	
	return token_raw.type == t.type
}

parse_peek_expr_first_type :: proc(vm : VM, scope : FRAME, start : TokenID) -> TypeID {
	
	exp := Expectation {
		positive = {
			TokenOperator {
				field = {}
			},
			
			TokenLiteral {},
			
			TokenIdentifier {},
			
			TokenDelimiter {}
		}
	}
	
	depth   : int
	literal : Variant
	search: for i := start; i < TokenID(len(vm.tokens)); i += 1 {
		next, next_err := get_token(vm, i)
		if next_err != nil do return -1
		
		parse_expectations(next, exp) or_break
		// Matches
		#partial switch &b in next.body {
		case TokenLiteral:
			literal = next.value
			
		case TokenIdentifier:
			
			// Must be a constant
			text, text_err := get_token_string(vm, next)
			if text_err != nil do continue // Ignore text errors
			
			// Type
			#partial switch get_identifier_type(vm, text) {
			case .Type:
				type, _ := get_type_id_by_name(vm, text)
				return type
			
			case .Unknown, .Variable:
				var_id, var, found := parse_var_id(scope, text)
				found or_continue
				
				type, found_type := peek_var_type(vm, scope, var_id, var.type, &i)
				if found_type do return type
			
			case: continue	
			}
			
		case TokenDelimiter:
			
			#partial switch b.type {
			case .ParenL:
				depth += 1
			case .ParenR:
				if depth == 0 do break search
				depth -= 1
				
			case:
				break search
			}
		}
	}
	
	switch t in literal {
	case bool:
		type, err := get_type_id_by_name(vm, "bool")
		return type
	
	case int,
		uint,
		uintptr,
		byte:
		type, err := get_type_id_by_name(vm, "int")
		return type
	
	case f64:
		type, err := get_type_id_by_name(vm, "float")
		return type
	}
	return -1
	
	// --- Internal Procedures ---
	peek_var_type :: proc(
		vm : VM, scope : FRAME,
		id : VarID, type : TypeID,
		token : ^TokenID
	) -> (expr_type : TypeID, found : bool) {
		
		stage : enum {
			Value,
			Selector,
		}
		
		res_type := type
		search_loop: for ;; {
			next, next_err := get_token(vm, token^)
			(next_err == nil) or_return
			text, _ := get_token_string(vm, next)
			
			base_type := get_base_type(vm, res_type, false)
			type_body, type_err := get_type(vm, get_base_type(vm, res_type, true))
			(type_err == nil) or_return
			
			#partial switch &b in type_body.body {
			case StructBody:
				
				switch stage {
				case .Value:
					if (next.body.(TokenDelimiter) or_else {}).type != .Period do return base_type, true
					stage = .Selector
					
				case .Selector:
					parse_expectations(next, expectation_identifier) or_return
					
					idx := -1; for m, i in b.members {
						(m.name == text) or_continue
						
						idx = i
					}
					
					(idx >= 0) or_return
					
					res_type = b.members[idx].base_type
					stage = .Value
				}
				
			case ArrayBody:
				(stage == .Value) or_return
				
				// "["
				token^ += 1
				lb, lberr := get_token(vm, token^)
				(lberr == nil) or_return
				
				#partial switch &b in lb.body {
				case TokenDelimiter:
					if b.type != .SquareL do return base_type, true
					
					
				case:
					return base_type, true
				}
				
				depth : int
				inside_loop: for ;; {
					token^ += 1
					t, t_err := get_token(vm, token^)
					(t_err == nil) or_return
					
					d := (t.body.(TokenDelimiter) or_else {}).type
					if d == .SquareL {
						depth += 1
					} else if d == .SquareR {
						if depth == 0 do break inside_loop
						depth -= 1
					}
				}
				
				res_type = b.base_type
			
			case:
				// Base type found
				return type_body.id, true
			}
			
			token^ += 1
		}
		
		return
	}
}


/* --- parse_var_id ---
 * calculate runtime VarID during
 * parsing, if one can be found
 */
parse_var_id :: proc(scope : FRAME, name : string) -> (id : VarID, var : Variable, found : bool) {
	
	if scope.parent != nil do id, var, found = parse_var_id(scope.parent, name)
	if found do return
	// defer if found do fmt.println("Variable", name, "found :", id)
	
	for v in scope.variables {
		if v.name == name do return id, v, true
		id += 1
	}
	
	return
}

/* --- peek_const ---
 * determine whether an expression is constant
 */
parse_peek_const :: proc(vm : VM, start : TokenID) -> (is_const : bool) {
	
	exp := Expectation {
		positive = {
			TokenOperator {
				field = {}
			},
			
			TokenLiteral {},
			
			TokenIdentifier {},
			
			TokenDelimiter {
				field = { .Terminator, .ParenL, .ParenR, },
			}
		}
	}
	
	for i in start..<TokenID(len(vm.tokens)) {
		next := vm.tokens[i]
		
		parse_expectations(next, exp) or_break
		// Matches
		#partial switch &b in next.body {
		case TokenIdentifier:
			
			// Must be a constant
			sub := strings.substring(vm.text, next.start, next.end) or_else ""
			(get_identifier_type(vm, sub) == .Constant) or_return
			
		case TokenDelimiter:
			
			// Expression ended as constant
			if b.type == .Terminator do return true
		}
	}
	
	return
}

parse_util_type :: proc(vm : VM, state : ^ParseState, $M : string) -> (type : TypeID, found : bool) {
	token, text, token_err := get_next_token(vm, state)
	if token_err != nil do return -1, false
	
	type_id, type_err := get_type_id_by_name(vm, text)
	if type_err != nil {
		parser_error_emit(
			vm, state, .Token_Unexpected,
			"Expected valid type name "
		)
		
		return -1, false
	}
	
	return type_id, true
}

/* --- parse_util_skip_until_terminator ---
 * skips all tokens until encountering a terminator
 */
parse_util_skip_until_terminator :: proc(vm : VM, state : ^ParseState, from_err : Error) -> (err : Error) {
	
	if from_err == .EOF do return .EOF
	#partial switch &t in from_err {
	case mem.Allocator_Error:
		return from_err
	}
	
	term_loop: for ;; {
		token, text, token_err := get_next_token(vm, state)
		if token_err != nil do return token_err
		
		if parse_expectations(token, expectation_terminator) do return
	}
}