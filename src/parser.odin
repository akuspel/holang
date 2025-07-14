package holang

/* --- Parser ---
 * handle the parsing of given tokens
 */

import "core:fmt"
import "core:strings"

// --- Types ---
Expectation :: struct {
	positive,
	negative : []TokenType,
}

ParserState :: struct {
	
	depth : int,
	body  : ParserStateBody,
	token : TokenID, // Start token
	
	parent : ^ParserState,
}

ParserStateBody :: union {
	
	ConstState,
	TypeState,
	VariableState,
	StructState,
	
	ExpressionState,
	ConstExpressionState,
	
	ParenState,
	CurlyState,
	SquareState,
}

// --  States  --
ConstState :: struct {
	
	// Parsing
	phase : enum {
		Start,	// #const
		Name,	// MyConst
		Equals, // =
		Expr,	// true
	},
	
	// Data
	name_token : TokenID,
	value : Variant,
	
}

VariableState :: struct {
	
	// Parsing
	phase : enum {
		Start,	// var
		Name,	// my_var
		Col,	// :
		Type,	// int
		Equals, // =
		Expr,	// 10
	},
	
	// Data
	name_token : TokenID,
	base_type_token : TokenID,
	value : Variant,
}

TypeState :: struct {
	
	// Parsing
	expr : enum {
		None,
		Reference,
		Unique,
		Pointer,
		Array,
		Struct,
	},
	
	fin : bool,
	
	phase : enum {
		Start,	// #type
		Name,	// MyType
		Equals, // =
		Expr,	// unique int
	},
	
	// Data
	name_token : TokenID,
	base_type_token : TokenID,
	
	type_body : TypeBody,
	members : [dynamic] StructMember,
}

ExpressionState :: struct {
	
}

ConstExpressionState :: struct {
	// Expression, but its value
	// Is solved during parsing
	
	// Parsing
	depth : int,
	phase : enum {
		Start,		// Entered expression
		Value,		// Received value (const identifier or literal)
		Operator,	// E.G. *
	},
	
	// Data
	value : Variant,
	values : [dynamic] ConstExprVal,
}

@(private)
ConstExprVal :: union { Variant, Operator, Delimiter }


StructState :: struct {
	
	// #type TypeName = struct {
	//	   member_1,
	//	   member_2,
	//	   ... ,
	//	   member_n
	// }
	
	// Parsing
	phase : enum {
		Start,
		Body,
		Member,
	},
	
	member : enum {
		Name,
		Col,
		Type,
	},
	
	// Data
	members : [dynamic] [2]TokenID, // {name, type}
}

ParenState :: struct {
	
}

CurlyState :: struct {
	
}

SquareState :: struct {
	
	value_token : TokenID,
	const_id	: ConstID,
	
	phase : enum {
		Start,
		Expr,
	}
}

// --- Procedures ---

parse_tokens :: proc(
	vm : VM,
	text : string,
	tokens : []Token,
	start : TokenID,
) -> (num : int, err : Error) {
	assert(vm != nil, "VM must be valid!")
	if text == "" do return 0, .Empty_String
	if tokens == nil do return 0, .No_Destination
	
	num_tokens := len(tokens)
	last : Token
	
	states : [dynamic]ParserState
	defer delete(states)
	i : int // Token index
	
	// Debug print
	temp := context.temp_allocator
	a, b : string
	expect_str : string
	defer if err != nil {
		fmt.println("Encountered Error while Parsing:", err)
		fmt.printfln("On line %i, column %i", last.meta.line_num, last.meta.rune_num)
		assert(err != .Invalid_String, "NMOOO")
		
		if err == .Token_Unexpected {
			
			fmt.println("Error!", a)
			fmt.println(expect_str)
			fmt.println("> Got:", b)
			
		}
		
		if len(states) > 0 {
			fmt.println("\nScope:")
			
			start_token, end_token : Token
			start_token = tokens[states[0].token]
			end_token	= tokens[i]
			sub := strings.substring(text, start_token.start, end_token.end) or_else ""
			fmt.println(sub)
			
			fmt.println(states)
		} else {
			fmt.println("\nScope:")
			
			next := tokens[i]
			sub := strings.substring(text, 0, next.end) or_else ""
			fmt.println(sub)
			
			fmt.println(states)
		}
		
		// Clean states (ignore errors)
		for len(states) > 0 do pop_state(vm, text, &states)
	}
	
	// Parse loop
	for i < num_tokens || len(states) > 0 {
		next := &tokens[i]; defer if err != nil do last = next^
		peek := true // Iter boolean
		
		// Current state is the top of states stack
		state :=
			&states[len(states) - 1] if len(states) > 0 else
			&ParserState {}
		state.depth = len(states)
		
		// Get strings
		sub, s_ok := strings.substring(text, next.start, next.end)
		if !s_ok do return 0, .Invalid_String
		
		sub_last := strings.substring(text, last.start, last.end) or_else ""
		a = sub_last; b = sub // Store strings for debug printing
		
		#partial switch &s in state.body {
		case ConstExpressionState:
			succ : bool
			exp : Expectation
			
			expectation_cont_expr_a := Expectation {
				positive = {
					TokenIdentifier {},
					TokenLiteral {},
					TokenDelimiter {
						field = { .ParenL }
					}
				}
			}
			
			expectation_cont_expr_b := Expectation {
				positive = {
					TokenOperator {},
					TokenDelimiter {}
					// NOTE: any delimiter that
					//		 comes after level 0
					//		 and is NOT a ParenL
					//		 ends the expression
				},
				
				negative = {
					TokenDelimiter {
						field = { .ParenL }
					}
				}
			}
			
			switch s.phase {
			case .Operator:
				fallthrough
				
			case .Start:
				// Expect a constant identifier
				// Or a literal value
				
				exp = expectation_cont_expr_a
				succ = parse_expectations(
					next^, exp
				)
				
				succ or_break
				
				s.phase = .Value
				#partial switch &b in next.body {
				case TokenIdentifier:
					
					// Check for constant
					const_id, id_err := get_const_id_by_name(vm, sub)
					if id_err != nil do return 0, id_err
					
					const, const_err := get_constant(vm, const_id)
					if const_err != nil do return 0, const_err
					
					value := const.value
					cexpr_push_value(&s.values, value)
					
				case TokenLiteral:
					
					cexpr_push_value(&s.values, next.value)
					
				case TokenDelimiter:
					
					// MUST be a ParenL
					s.depth += 1
					s.phase = .Start // Override phase
					cexpr_push_value(&s.values, b.type)
				}
				
			case .Value:
				// Expect an operator
				
				exp = expectation_cont_expr_b
				succ = parse_expectations(
					next^, exp
				)
				
				succ or_break
				
				#partial switch &b in next.body {
				case TokenOperator:
					
					// Operator, push
					s.phase = .Operator
					cexpr_push_value(&s.values, b.type)
					
					// Solve available
					solve_err := cexpr_solve_top(&s.values)
					if solve_err != nil do return 0, solve_err
					
				case TokenDelimiter:
					
					delim: #partial switch b.type {
					case .ParenR:
						if s.depth == 0 {
							
							// Solve state
							peek = false // Parent state checks exit
							pop_err := pop_state(vm, text, &states)
							if pop_err != nil do return 0, pop_err
							break
						}
						
						s.depth -= 1
						s.phase = .Value
						
						// Right parentheses reached
						// Solve until the first parens
						solve_err := cexpr_solve_value_until_paren(
							&s.values
						);	if solve_err != nil do return 0, solve_err
					
					case:
						
						// IF depth isn't 0, the expression
						// Has loose ends. No loose ends, Waltuh
						if s.depth != 0 do return 0, .Expression_Depth
						
						// Solve state
						peek = false // Parent state checks exit
						pop_err := pop_state(vm, text, &states)
						if pop_err != nil do return 0, pop_err
					}
				}
			}
			
			if !succ {
				expect_str = print_expectations(exp, temp)
				return 0, .Token_Unexpected
			}
			
			// --- Iternal Procedures ---
		
		// Constant Definition
		// NOTE: can only be defined in file scope
		//		 AKA when depth == 0
		//		 OR  when state.body == nil
		case ConstState:
			succ : bool
			exp : Expectation
			
			switch s.phase {
			case .Start:
				exp = expectation_identifier
				succ = parse_expectations(
					next^, exp
				)
				
				succ or_break
				
				s.name_token = token_id(start, i)
				s.phase = .Name
			
			case .Name:
				exp = expectation_equals
				succ = parse_expectations(
					next^, exp
				)
				
				succ or_break
				s.phase = .Equals
			
			case .Equals:
				exp = expectation_lit_or_ident_or_paren
				succ = parse_expectations(
					next^, exp
				)
				
				succ or_break
				
				// Peek expression
				succ = peek_const(vm, text, tokens, i)
				if !succ do fmt.println("Expected Constant Expression!")
				succ or_break
				
				peek = false // Start expression from when it began
				append_state(&states, ConstExpressionState {}, i)
				s.phase = .Expr
			
			case .Expr:
				exp = expectation_terminator
				succ = parse_expectations(
					next^, exp
				)
				
				succ or_break
				
				// Solve constant
				pop_err := pop_state(vm, text, &states)
				if pop_err != nil do return 0, pop_err
			}
			
			if !succ {
				expect_str = print_expectations(exp, temp)
				return 0, .Token_Unexpected
			}
		
		// Type Definition
		// NOTE: can only be defined in file scope
		case TypeState:
			succ : bool
			exp : Expectation
			
			switch s.phase {
			case .Start:
				exp = expectation_identifier
				succ = parse_expectations(
					next^, exp
				)
				
				succ or_break
				s.phase = .Name
				
				s.name_token = token_id(start, i)
			
			case .Name:
				exp = expectation_equals
				succ = parse_expectations(
					next^, exp
				)
				
				succ or_break
				s.phase = .Equals
			
			case .Equals:
				
				// Standard case
				exp = expectation_variable_typedef
				succ = parse_expectations(
					next^, exp
				)
				
				succ or_break
				// Figure out which type of definition this is
				#partial switch &b in next.body {
				case TokenOperator:
					// We know this is a pointer
					s.expr = .Pointer
				
				case TokenKeyword:
					// Struct or Unique
					#partial switch b.type {
					case .Struct:
						append_state(&states, StructState {}, i)
						s.expr = .Struct
					
					case .Unique:
						s.expr = .Unique
					}
				
				case TokenDelimiter:
					// We know this is a SquareL
					append_state(&states, SquareState {}, i)
					s.expr = .Array
				
				case TokenIdentifier:
					// Type reference
					s.expr = .Reference
					s.base_type_token = token_id(start, i)
					s.type_body = ReferenceBody {}
				}
				
				// Finalize expression in next
				s.phase = .Expr
			
			case .Expr:
				
				// Making a unique type reference
				final := s.fin // Can move to terminator
				if !s.fin do #partial switch s.expr {
				case .Pointer:
					exp = expectation_identifier
					succ = parse_expectations(
						next^, exp
					)
					
					succ or_break
					s.fin = true
					s.type_body = PointerBody {}
					
					s.base_type_token = token_id(start, i)
				
				case .Unique:
					exp = expectation_identifier
					succ = parse_expectations(
						next^, exp
					)
					
					succ or_break
					s.fin = true
					s.type_body = ReferenceBody { unique = true }
					
					s.base_type_token = token_id(start, i)
					
				case .Array:
					// Get the type of the array
					exp = expectation_identifier
					succ = parse_expectations(
						next^, exp
					)
					
					succ or_break
					s.fin = true
					
					s.base_type_token = token_id(start, i)
					
				case:
					s.fin = true
					final = true
				}
				
				// --- End Of Expression ---
				final or_break // Deferred to next token
				exp = expectation_terminator
				succ = parse_expectations(
					next^, exp
				)
				
				succ or_break
				
				// Finalize type definition
				pop_err := pop_state(vm, text, &states)
				if pop_err != nil do return 0, pop_err
			}
			
			if !succ {
				expect_str = print_expectations(exp, temp)
				return 0, .Token_Unexpected
			}
		
		case StructState:
			succ : bool
			exp : Expectation
			
			expectation_struct_body := Expectation {
				positive = {
					TokenIdentifier {},
					TokenDelimiter {
						field = { .CurlyR }
					}
				}
			}
			
			switch s.phase {
			case .Start:
				exp = expectation_struct_curly
				succ = parse_expectations(
					next^, exp
				)
				
				succ or_break
				s.phase = .Body
				
			case .Body:
				exp = expectation_struct_body
				succ = parse_expectations(
					next^, exp
				)
				
				succ or_break
				
				#partial switch &b in next.body {
				case TokenIdentifier:
					// Struct field name
					s.phase  = .Member
					s.member = .Name
					
				case TokenDelimiter:
					// End of struct
					pop_err := pop_state(vm, text, &states)
					if pop_err != nil do return 0, pop_err
				}
			
			case .Member:
				// Defining a member
				
				switch s.member {
				case .Name:
					exp = expectation_col
					succ = parse_expectations(
						next^, exp
					)
					
					succ or_break
					s.member = .Col
					
				case .Col:
					exp = expectation_identifier
					succ = parse_expectations(
						next^, exp
					)
					
					succ or_break
					s.member = .Type
					
					// Append the member
					append(&s.members, [2]TokenID {token_id(start, i-2), token_id(start, i)})
					
				case .Type:
					exp = expectation_curly_or_comma
					succ = parse_expectations(
						next^, exp
					)
					
					succ or_break
					#partial switch &b in next.body {
					case TokenDelimiter:
						#partial switch b.type {
						case .Comma:
							
							// Next element
							s.member = .Name
							s.phase  = .Body
						
						case .CurlyR:
							
							// End of struct
							pop_err := pop_state(vm, text, &states)
							if pop_err != nil do return 0, pop_err
						}
					}
				}
			}
			
			if !succ {
				expect_str = print_expectations(exp, temp)
				return 0, .Token_Unexpected
			}
			
			
		// Different sorts of brackets
		case SquareState:
			succ : bool
			exp : Expectation
			
			switch s.phase {
			case .Start:
				exp = expectation_num_or_ident
				succ = parse_expectations(
					next^, exp
				)
				
				succ or_break
				
				#partial switch &b in next.body {
				case TokenLiteral:
					// It is a number
					s.value_token = token_id(start, i)
					s.const_id = -1
				
				case TokenIdentifier:
					// It must be a constant
					s.value_token = -1
					
					// --- Getting A Constant Example ---
					const_name := strings.substring(text, next.start, next.end) or_else ""
					const_id, const_err := get_const_id_by_name(vm, const_name)
					if const_err != nil do return 0, const_err
					
					s.const_id = const_id
				}
				
				s.phase = .Expr
				
			case .Expr:
				exp = expectation_square_close
				succ = parse_expectations(
					next^, exp
				)
				
				succ or_break
				pop_err := pop_state(vm, text, &states)
				if pop_err != nil do return 0, pop_err
				
			}
			
			if !succ {
				expect_str = print_expectations(exp, temp)
				return 0, .Token_Unexpected
			}
		
		
		// Default case (File scope)
		case:
			// File scope
			// Possibilities are endless
			file_scope_expect := Expectation {
				positive = {
					TokenKeyword {
						field = {
							.Type,
							.Function,
							.Variable,
							.Constant,
						},
					},
				},
			}
			
			// Need a declarative keyword
			if parse_expectations(
				next^, file_scope_expect
			) {
				
				// Expectation matched
				// Update State
				
				kw := next.body.(TokenKeyword)
				#partial switch kw.type {
				case .Function:
				
				case .Variable:
					append_state(&states, VariableState {}, i)
				
				case .Constant:
					append_state(&states, ConstState {}, i)
				
				case .Type:
					append_state(&states, TypeState {}, i)
				
				case:
					return 0, .Token_Unknown
				}
				
			} else {
				
				expect_str = print_expectations(file_scope_expect, temp)
				return 0, .Token_Unexpected
			}
		}
		
		if peek {
			last = next^
			i += 1
		}
	}
	
	return num_tokens, nil
	
	// --- Internal Procedures ---
	token_id :: proc(start : TokenID, idx : int) -> TokenID {
		return start + TokenID(idx)
	}
	
	/* --- peek_const ---
	 * determine whether an expression is constant
	 */
	peek_const :: proc(vm : VM, text : string, tokens : []Token, start : int) -> (is_const : bool) {
		
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
		
		for i in start..<len(tokens) {
			next := tokens[i]
			
			parse_expectations(next, exp) or_break
			// Matches
			#partial switch &b in next.body {
			case TokenIdentifier:
				
				// Must be a constant
				sub := strings.substring(text, next.start, next.end) or_else ""
				_, const_err := get_const_id_by_name(vm, sub)
				if const_err != nil do return false
				
			case TokenDelimiter:
				
				// Expression ended as constant
				if b.type == .Terminator do return true
			}
		}
		
		return
	}
}

parse_expectations :: proc(
	next : Token,
	expects : Expectation
) -> bool {
	
	// Handle positive cases
	positive : bool
	if expects.positive == nil {
		positive = true
	} else {
		
		for &t in expects.positive {
			positive ||= expect(next, t, false)
		}
	}
	
	// Handle negative cases
	if expects.negative == nil {
		return positive
	} else {
		
		for &t in expects.negative {
			negative := expect(next, t, true)
			if !negative do return false
		}
	}
	
	return positive
}

print_expectations :: proc(
	expects : Expectation,
	alloc := context.temp_allocator,
	loc := #caller_location
) -> string {
	
	parts : [dynamic]string
	defer delete(parts)
	
	fmt.println(loc)
	
	append(&parts, fmt.aprint("> Expected ", allocator = alloc))
	if expects.positive != nil {
		for &f in expects.positive do append(&parts, print(f, alloc))
	}
	
	if expects.negative != nil && len(expects.negative) > 0 {
		append(&parts, fmt.aprint("but not ", allocator = alloc))
		for &f in expects.negative do append(&parts, print(f, alloc))
	}
	
	combo, _ := strings.concatenate(parts[:], alloc)
	return combo
	
	print :: proc(f : TokenType, alloc := context.allocator) -> string {
		switch &b in f {
		case TokenKeyword:		return print_field(b, alloc)
		case TokenOperator: 	return print_field(b, alloc)
		case TokenIdentifier:	return print_field(b, alloc)
		case TokenLiteral:		return print_field(b, alloc)
		case TokenDelimiter:	return print_field(b, alloc)
		}
		
		return ""
	}
}

print_field :: proc(
	body : TokenBody($T),
	alloc := context.allocator
) -> string {
	parts : [dynamic]string
	defer delete(parts)
	
	for v in body.field do append(&parts, fmt.aprint(v, ", ", sep = "", allocator = alloc))
	if body.field == {} do for t in T do append(&parts, fmt.aprint(t, ", ", sep = "", allocator = alloc))
	
	combo, _ := strings.concatenate(parts[:], alloc)
	return combo
}

/* --- expect ---
 * expect the next token to
 * follow specified pattern
 *
 * on not expect anything BUT
 * NOTE: when using not,
 *		 you must join expressions with &&,
 *		 not with || like in normal case
 */
expect :: proc(next : Token, type : Maybe(TokenType), not : bool) -> bool {
	if type == nil do return true // No expectations
	
	a, b : typeid
	any_of : bool
	switch &b in type.? {
	case TokenKeyword:		
		a = typeid_of(type_of(b))
		any_of = b.field == {}
	case TokenIdentifier:
		a = typeid_of(type_of(b))
		any_of = b.field == {}
	case TokenOperator:
		a = typeid_of(type_of(b))
		any_of = b.field == {}
	case TokenLiteral:
		a = typeid_of(type_of(b))
		any_of = b.field == {}
	case TokenDelimiter:
		a = typeid_of(type_of(b))
		any_of = b.field == {}
	}
	
	switch &t in next.body {
	case TokenKeyword:		
		b = typeid_of(type_of(t))
	case TokenIdentifier:
		b = typeid_of(type_of(t))
	case TokenOperator:
		b = typeid_of(type_of(t))
	case TokenLiteral:
		b = typeid_of(type_of(t))
	case TokenDelimiter:
		b = typeid_of(type_of(t))
	}
	
	// Continue check if types match
	match := (a == b)
	if !match do return not
	if any_of do return !not
	
	t := type.?
	switch &b in next.body {
	case TokenKeyword:		 match = b.type in t.(TokenKeyword).field
	case TokenIdentifier:	 match = b.type in t.(TokenIdentifier).field
	case TokenOperator: 	 match = b.type in t.(TokenOperator).field
	case TokenLiteral:		 match = b.type in t.(TokenLiteral).field
	case TokenDelimiter:	 match = b.type in t.(TokenDelimiter).field
	}
	
	return match != not
}