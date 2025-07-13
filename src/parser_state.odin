package holang

/* --- Parser State ---
 * utils, types, functions
 * for handling the parser state
 */

import "core:fmt"
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
				
				type_id, type_err := get_type_id_by_name(vm, typename)
				if type_err != nil do return type_err
				
				// Assign member values
				member.name = name
				member.base_type = type_id
				append(&parent_body.members, member)
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
				
				offset := i
				diff := i %% type.align
				if diff != 0 do offset += type.size - diff
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
		
		case:
		}
		
		new_type.body = b.type_body
		fmt.println(new_type)
		return register_type(vm, new_type)
	
	case:
	}
	
	return
}