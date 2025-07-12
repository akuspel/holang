package holang

/* --- Variables ---
 * handle variables and data
 */


// --- Types ---
Variable :: struct {
	
	ptr, bef : uintptr,
	// For alignment reasons,
	// Ptr might have empty space
	// Before it -> bef
	
	type : TypeID,
	mutable : bool, // Can't mutate nonmutable things
	
}

ExternalVariable :: struct {
	using _ : Variable,
	
	// yada
}

/* --- Constant ---
 * "compile" time constant
 * - like #define in C
 * - or :: in Odin
 */
Constant :: struct {
	
	value : Variant,
	name : string,
}

/* --- Variant ---
 * base underlying data types
 */
Variant :: union {
	
	int,
	uint,
	uintptr,
	
	f64,
	bool,
	byte,
}

/* --- Child ---
 * nested child value of arr / struct
 */
Child :: struct {
	idx  : int,
	next : ^Child,
}

// --- Procedures ---
@(private)
get_variable_value :: proc(vm : VM, v : Variable, off : ^Child) -> (value : Variant, err : Error) {
	if vm == nil  do return nil, .No_VM
	if v.ptr == 0 do return nil, .Null_Pointer
	
	type, type_err := get_type(vm, v.type)
	if type_err != nil do return nil, type_err
	
	// Parse body types
	switch &b in type.body {
	case IntBody:
		
		// Signed or unsigned
		if b.signed do return (^int)(get_global_ptr(vm, v.ptr))^, nil
		return (^uint)(v.ptr)^, nil
	
	case FloatBody: return (^f64) (get_global_ptr(vm, v.ptr))^, nil
	case BoolBody:	return (^bool)(get_global_ptr(vm, v.ptr))^, nil
	case ByteBody:	return (^byte)(get_global_ptr(vm, v.ptr))^, nil
	
	case PointerBody:
		return (^uintptr)(get_global_ptr(vm, v.ptr))^, nil
	
	case ReferenceBody:
		
		child := v
		child.type = b.base_type
		
		return get_variable_value(vm, child, {})
	
	// The ones where things might go wrong
	case ArrayBody:
		if off == nil do return nil, .Unknown_Member
		
		// Bounds check
		if off.idx < 0 || off.idx >= b.size do return nil, .Bounds_Check
		
		offset := uintptr(off.idx * b.align)
		child  := Variable {
			ptr  = v.ptr + offset,
			type = b.base_type,
		}
		
		return get_variable_value(vm, child, off.next)
	
	case StructBody:
		if off == nil do return nil, .Unknown_Member
		
		// Bounds check
		if off.idx < 0 || off.idx >= len(b.members) do return nil, .Bounds_Check
		
		member := b.members[off.idx]
		child  := Variable {
			ptr  = v.ptr + member.offset,
			type = member.base_type,
		}
		
		return get_variable_value(vm, child, off.next)
	}
	
	return
}

get_external_variable_value :: proc(
	vm : VM,
	v  : ExternalVariable,
	off : int,
)

// --  Stack Behaviour	--
variable_push :: proc(
	vm : VM,
	type : TypeID,
	mut  : bool,
) -> (err : Error) {
	if vm == nil do return .No_VM
	
	sp, stack_err := get_stack_ptr(vm.stack)
	if stack_err != nil do return stack_err
	variable := Variable {
		type = type,
		mutable = mut,
	}
	
	type, type_err := get_type(vm, type)
	val_ptr, push_err := stack_push(
		vm.stack, nil,
		type.size, type.align
	);	if push_err != nil do return push_err
	
	variable.ptr = val_ptr
	variable.bef = sp
	
	append(&vm.variables, variable)
	
	return
}



/* IGNORE FOR NOW!

		
		// --- Internal Procs ---
		struct_members_recursive :: proc(
			off : int, i : ^int,
			offset : ^uintptr,
			
		) -> int {
			
			for m in members {
				if is_struct() 
				
				(i + mi == off) or_continue
				
				// Found one
				offset^ += m.offset
				return i^
			}
			
			if 
		}
*/