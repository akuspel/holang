package holang

/* --- Parser State ---
 * utils, types, functions
 * for handling the parser state
 */

import "core:fmt"
import "core:mem"
import "core:slice"
import "core:strings"

// --- Procedures ---
@(private)
/* --- append_state ---
 * append new level of state
 */
append_state :: proc(states : ^[dynamic]ParserState, body : ParserStateBody, idx : int) {
	assert(states != nil, "Parser State Array must be valid!")
	assert(body != nil, "Parser State Body must be valid!")
	
	num_states := len(states)
	state := ParserState {
		depth = num_states + 1,
		body  = body,
		token = TokenID(idx),
	}
	
	// Linked list parent
	if num_states > 0 do state.parent = &states[num_states - 1]
	
	append(states, state)
}

@(private)
/* --- pop_state ---
 * pop state and clean up
 */
pop_state :: proc(
	vm : VM, text : string,
	states : ^[dynamic]ParserState
) -> (err : Error) {
	assert(states != nil, "Parser State Array must be valid!")
	num_states := len(states)
	assert(num_states > 0, "Can not pop from emprt State Array!")
	
	// Doesn't need to be a pointer
	// Since the underlying data is
	// Reset, there is no other ref
	state := states[num_states - 1]
	pop(states)
	
	// Don't move on to next element
	// Solve until no more pops
	
	// !!! TODO !!!
	// figure out if this is
	// necessary or not
	// iter^ = true
	
	// Generate commands from state
	// And clean up the mess
	return solve_state(vm, text, &state)
}

@(private="file")
/* --- solve_state ---
 * solve parser state
 * when popped
 */
solve_state :: proc(vm : VM, text : string, state : ^ParserState) -> (err : Error) {
	// NOTE: since in the caller location
	//		 these are already checked,
	//		 checking for null pointers
	//		 should NOT be necessary
	when ODIN_DEBUG {
		assert(vm != nil, "VM must be valid")
		assert(state != nil, "Parser State must be valid")
	}
	
	// Behaviour per state
	#partial switch &b in state.body {
	case StructState:
		
		// Build Struct type from 
		defer delete(b.members)
		
		(state.parent != nil) or_break
		
		#partial switch &parent_body in state.parent.body {
		case TypeState:
			
			// Solve members and append
			parent_body.type_body = StructBody {}
			
			for m in b.members {
				member : StructMember
				name_token, name_err := get_token(vm, m.x); if name_err != nil do return name_err
				type_token, tnam_err := get_token(vm, m.y); if tnam_err != nil do return tnam_err
				
				name	 := strings.substring(text, name_token.start, name_token.end) or_else ""
				typename := strings.substring(text, type_token.start, type_token.end) or_else ""
				
				// Check name
				if name == "" do return .Struct_Member_None // Invalid name
				if name != "_" do for &m in parent_body.members do if m.name == name do return .Struct_Member_Over
				
				type_id, type_err := get_type_id_by_name(vm, typename)
				if type_err != nil do return type_err
				
				
				// Assign member values
				member.name = name
				member.base_type = type_id
				append(&parent_body.members, member)
			}
		}
	
	case SquareState:
		
		// Get variant from value or const
		value : Variant
		
		if b.value_token != -1 {
			value_token, token_err := get_token(vm, b.value_token)
			if token_err != nil do return token_err
			
			value = value_token.value
			
		} else if b.const_id != -1 {
			const, const_err := get_constant(vm, b.const_id)
			if const_err != nil do return const_err
			
			value = const.value
			
		}
		
		if value == nil do return .Invalid_Value
		(state.parent != nil) or_break
		
		#partial switch &parent_body in state.parent.body {
		case TypeState:
			
			// Create array base
			parent_body.type_body = ArrayBody {
				size = value.(int) or_else 0
			}
		}
	
	case TypeState:
		
		// Finalize type
		defer delete(b.members)
		
		name_token, name_err := get_token(vm, b.name_token)
		if name_err != nil do return name_err
		name := strings.substring(text, name_token.start, name_token.end) or_else ""
		
		new_type : Type
		new_type.name = name
		
		#partial switch &t in b.type_body {
		case StructBody:
			
			// Calculate offsets
			// And type size
			i, size, align : int
			for &m in b.members {
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
			
			new_type.size  = size
			new_type.align = align
			
			t.members = b.members[:]
		
		case ArrayBody:
			
			// --- Figure Out Base Type ---
			base_type_token, token_err := get_token(vm, b.base_type_token)
			if token_err != nil do return token_err
			
			base_type_name := strings.substring(text, base_type_token.start, base_type_token.end) or_else ""
			type_id, id_err := get_type_id_by_name(vm, base_type_name)
			if id_err != nil do return id_err
			
			base_type, type_err := get_type(vm, type_id)
			if type_err != nil do return type_err
			
			// Assign values
			t.base_type = type_id
			t.align = base_type.align
			
			// Error when defining array size
			if t.size <= 0 do return .Invalid_Array_Size
			
			new_type.size  = t.size * mem.align_forward_int(
				base_type.size, t.align)
			new_type.align = t.align
		
		case PointerBody:
			
			// --- Figure Out Base Type ---
			base_type_token, token_err := get_token(vm, b.base_type_token)
			if token_err != nil do return token_err
			
			base_type_name := strings.substring(text, base_type_token.start, base_type_token.end) or_else ""
			type_id, id_err := get_type_id_by_name(vm, base_type_name)
			if id_err != nil do return id_err
			
			// Assign values
			t.base_type = type_id
			
			new_type.size  = 8
			new_type.align = 8
		
		case ReferenceBody:
			
			// --- Figure Out Base Type ---
			base_type_token, token_err := get_token(vm, b.base_type_token)
			if token_err != nil do return token_err
			
			base_type_name := strings.substring(text, base_type_token.start, base_type_token.end) or_else ""
			type_id, id_err := get_type_id_by_name(vm, base_type_name)
			if id_err != nil do return id_err
			
			base_type, type_err := get_type(vm, type_id)
			if type_err != nil do return type_err
			
			// Assign values
			t.base_type = type_id
			
			new_type.size  = base_type.size
			new_type.align = base_type.align
		}
		
		new_type.body = b.type_body
		fmt.println(new_type)
		type_id, type_err := register_type(vm, new_type)
		return type_err
	
	// Runtime expression
	case ExpressionState:
		
		defer delete(b.values)
		
		// An expression must have a parent state
		if state.parent == nil do return .Expression_Invalid
		
		cmd_alloc := mem.dynamic_arena_allocator(&vm.cmd_arena)
		
		#partial switch &parent_body in state.parent.body {
		case VariableState:
			parent_body.expression.values = slice.clone(
				b.values[:], cmd_alloc
			)
		}
	
	// Constant expression
	case ConstExpressionState:
		
		defer delete(b.values)
		
		// Solve expression if there is any left
		solve_err := cexpr_solve_value_until_paren(
			&b.values
		);	if solve_err != nil do return solve_err
		
		// Must have exactly one value left in values
		if len(b.values) != 1 do return .Expression_Invalid
		value := b.values[0].body.(Variant) or_else nil
		if value == nil do return .Expression_Invalid
		
		(state.parent != nil) or_break
		
		#partial switch &parent_body in state.parent.body {
		case ConstState:
			parent_body.value = value
		
		case VariableState:
			parent_body.value = value
		}
		
	case ConstState:
	
		// Create constant
		new_constant : Constant
		new_constant.value = b.value
		if new_constant.value == nil do return .Expression_Invalid
		
		name_token, token_err := get_token(vm, b.name_token)
		if token_err != nil do return token_err
		
		new_constant.name = strings.substring(
			text, name_token.start, name_token.end) or_else ""
		if new_constant.name == "" do return .Invalid_Name
		
		// Add constant
		fmt.println(new_constant)
		return register_constant(vm, new_constant)
	
		
	
	case:
		fmt.println("UNHANDLED STATE POP:")
		fmt.println(state)
	}
	
	return
}