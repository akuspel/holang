#+private="file"
package holang

/* --- Recursive Descent Parser ---
 * a replacement for the horrifying mess
 * that was and is
 *     parser.odin *and*
 *     parser_state.odin
 *
 * using a *simple* recursive
 * descent algorithm, no AST!
 */

import "core:fmt"
import "core:mem"
import "core:slice"

// --- Constants ---
DUMBO_MESSAGES :: false

// --- Types ---
ParseState :: struct {
	
	token : TokenID,	// Current token
	scope : struct {
		depth : int,		// Scope depth
		type  : ScopeType,	// Type
	}

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
		if parse_scope(vm, &state) ==
			.Invalid_Token {
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
	if err != nil do return
	
	text, err = get_token_string(vm, token)
	if err != nil do return
	
	// Update next token
	if grow do state.token += 1
	return
}

/* --- parser_error_emit ---
 * emits an error during parsing
 * which gets printed with multi
 * error support
 */
parser_error_emit :: proc(
	vm : VM, state : ^ParseState,
	error : Error, message : string,
	) -> (err : Error) {
	
	
	token, token_err := get_token(vm, state.token)
	
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
		"\nError during parsing at line %i, char %i\nError type: ",
		token.meta.line_num, token.meta.rune_num
	)
	
	fmt.println(error)
	fmt.println("Message:", message)
	
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
parse_scope :: proc(vm : VM, state : ^ParseState) -> (err : Error) {
	token, text, token_err := get_next_token(vm, state)
	if token_err != nil do return token_err
	
	// Depth 0 - FILE DEPTH:
	// in this depth, you can define
	// global variables, constants,
	// types, functions, AND NOTHING ELSE!
	
	switch state.scope.type {
	case .File: // DEPTH = 0
	
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
		
		// Declarative keyword expected
		if parse_expectations(token, file_scope_expect) {
			
			kw := token.body.(TokenKeyword)
			#partial switch kw.type {
			case .Function:
				parser_dumbo_emit("Parsing a Function Declaration!")
				
			case .Variable:
				parser_dumbo_emit("Parsing a Variable Declaration!")
			
			case .Constant:
				parser_dumbo_emit("Parsing a Constant Declaration!")
				return parse_constant(vm, state)
			
			case .Type:
				parser_dumbo_emit("Parsing a Type Declaration!")
				return parse_type(vm, state)
			
			// If you reach here, count me impressed
			case: unreachable()
			}
			
		} else {
			
			return parser_error_emit(
				vm, state, .Token_Unexpected,
				"Expected Constant, Type, Variable or Function declaration in File Scope"
			)
		}
		
	case .Function: // DEPTH = 1
	
	case .Block: // DEPTH > 1
		
	}
	
	return
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
	// FIGURE OUT: should I return on value err
	//			   or keep parsing IN constant?
	
	// STAGE 3: Expect terminator ";"
	if !parse_util_terminator(vm, state) do return .Token_Unexpected
	
	// Create a constant
	constant := Constant {
		name = identifier,
		value = value
	}
	fmt.println("Constant", identifier, "=", value)
	
	return register_constant(vm, constant)
}

parse_constant_expression :: proc(vm : VM, state : ^ParseState) -> (value : Variant, err : Error) {
	token, text, token_err := get_next_token(vm, state)
	if token_err != nil do return nil, token_err
	
	// NOTE: we'll do it simple for now
	//		 and only expect a variant
	if !parse_expectations(token, {positive = {
		TokenLiteral { field = { .Number, .Boolean } }
	}}) {
		return nil, parser_error_emit(
			vm, state, .Token_Unexpected,
			"Expected a literal value"
		)
	}
	
	value = token.value
	if value == nil {
		return nil, parser_error_emit(
			vm, state, .Invalid_Value,
			"Expected a valid constant value"
		)
	}
	
	return
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
			base_type = id
		}
		
		// Size and alignment
		type.size  = base_type.size
		type.align = base_type.align
	
	case: unreachable()
	};	if err != nil do return
	
	// STAGE 3: Expect Terminator ";"
	if !parse_util_terminator(vm, state) do return .Token_Unexpected
	
	// --- Register Type ---
	type.name = identifier
	fmt.println("Type", identifier, "=", type.body)
	
	return register_type(vm, type)
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
		base_type = id,
		unique = true
	}
	
	// Size and alignment
	type.size  = base_type.size
	type.align = base_type.align
	
	return
}

parse_array_type :: proc(vm : VM, state : ^ParseState, type : ^Type) -> (err : Error) {
	token, text, token_err := get_next_token(vm, state)
	if token_err != nil do return token_err
	
	// EXAMPLE SYNTAX:
	// [10]int
	// [MY_INTEGER_CONSTANT]int
	
	// STATE 0: Expect identifier or literal
	if !parse_expectations(token, expectation_num_or_ident) {
		return parser_error_emit(
			vm, state, .Token_Unexpected,
			"Expected integer literal or -constant identifier in array size declaration"
		)
	}
	
	arr_size : int
	#partial switch &b in token.body {
	case TokenLiteral:
		arr_size = token.value.(int) or_else -1
	
	case TokenIdentifier:
		
		
		id, id_err := get_const_id_by_name(vm, text)
		const, const_err := get_constant(vm, id)

		if id_err != nil || const_err != nil {
			return parser_error_emit(
				vm, state, .Unknown_Const,
				"Expected integer literal or -constant identifier in array size declaration"
			)
		}
		
		arr_size = const.value.(int) or_else -1
	}
	
	// Check correct size
	if arr_size < 1 {
		return parser_error_emit(
			vm, state, .Invalid_Array_Size,
			"Expected an integer > 0 for array size declaration"
		)
	}
	
	// STATE 1: Expect "]"
	token, text, token_err = get_next_token(vm, state)
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

parse_util_terminator :: proc(vm : VM, state : ^ParseState) -> bool {
	token, text, token_err := get_next_token(vm, state)
	if token_err != nil do return false
	
	if !parse_expectations(token, expectation_terminator) {
		parser_error_emit(
			vm, state, .Token_Unexpected,
			"Expected \";\" at the end of the statement"
		)
		
		return false
	}
	
	return true
}

parse_util_single_token :: proc(vm : VM, state : ^ParseState, t : TokenBody($T), $M : string) -> bool {
	token, text, token_err := get_next_token(vm, state)
	if token_err != nil do return false
	
	token_raw, ok := token.body.(TokenBody(T))
	if !ok {
		parser_error_emit(
			vm, state, .Token_Unexpected,
			M
		)
		
		return false
	}
	
	return token_raw.type == t.type
}