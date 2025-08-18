package holang

/* --- Types ---
 * underlying structures
 * for creating and using
 * types in holang
 *
 * very nice!
 */

import "core:mem"
import "core:slice"
import "core:strings"

// --- Types ---

/* --- TypeID ---
 * each holang type has an underlying ID
 * which is an index in an array of types
 */
TypeID :: distinct int


/* --- Type ---
 * underlying data type for
 * defining types in holang
 */
Type :: struct {
	
	name : string,
	id	 : TypeID,
	
	size  : int,
	align : int,
	
	// NOTE: obfuscate disables the ability
	//		 to transparently view the underlying
	//		 data of a variable with given type
	obfuscate : bool,
	
	body : TypeBody,
}


/* --- TypeBody ---
 * defining characteristics
 * of a holang type
 */
TypeBody :: union {
	
	ByteBody,
	IntBody,
	FloatBody,
	BoolBody,
	
	StructBody,
	ArrayBody,
	PointerBody,
	ReferenceBody,
}


// --  Bodies  --
StructBody :: struct {
	
	members : []StructMember,
	
}

ArrayBody :: struct {
	
	size  : int,
	align : int,
	
	base_type : TypeID,
}

ByteBody :: struct {
	
}

IntBody :: struct {
	signed : bool,
}

FloatBody :: struct {
	
}

BoolBody :: struct {
	
}

PointerBody :: struct {
	
	base_type : TypeID,
}

ReferenceBody :: struct {
	
	base_type : TypeID,
	unique	  : bool,
}

// -   Structs	 -
StructMember :: struct {
	
	name   : string,
	offset : uintptr,
	
	base_type : TypeID,
}


// --- Variables ---

// --  Base Types  --
type_byte := Type {
	name = "byte",
	
	size  = 1,
	align = 1,
	
	body = ByteBody {}
}

type_bool := Type {
	name = "bool",
	
	size  = 8,
	align = 8,
	
	body = BoolBody {}
}

type_int := Type {
	name = "int",
	
	size  = 8,
	align = 8,
	
	body = IntBody {
		signed = true,
	}
}

type_uint := Type {
	name = "uint",
	
	size  = 8,
	align = 8,
	
	body = IntBody {}
}

type_float := Type {
	name = "float",
	
	size  = 8,
	align = 8,
	
	body = FloatBody {}
}

// --- Procedures ---

register_type :: proc(vm : VM, base : Type) -> (err : Error) {
	if vm == nil do return .No_VM
	type := base
	
	// Check for type redifinitions
	if get_identifier_type(vm, type.name) != .Unknown do return .Type_Over
	
	// Copy memory to VM
	alloc := mem.dynamic_arena_allocator(&vm.type_arena)
	type.name = strings.clone(type.name, alloc)
	
	#partial switch &b in type.body {
	case StructBody:
		
		b.members = slice.clone(b.members, alloc)
		for &m in b.members do m.name = strings.clone(m.name, alloc)
	}
	
	// Create the type
	type.id = TypeID(len(vm.types))
	append(&vm.types, type)
	
	return
}

@(private)
base_types_init :: proc(vm : VM) -> (err : Error) {
	
	// NOTE: only called when creating the VM,
	//		 we can be positively sure, that
	//		 no errors will be happening here
	
	err = register_type(vm, type_byte)
	if err != nil do return
	
	err = register_type(vm, type_int)
	if err != nil do return
	err = register_type(vm, type_uint)
	if err != nil do return
	
	err = register_type(vm, type_float)
	if err != nil do return
	err = register_type(vm, type_bool)
	if err != nil do return
	
	return
}

@(require_results)
get_type :: proc(vm : VM, id : TypeID) -> (type : Type, err : Error) {
	if vm == nil do return {}, .No_VM
	if !(int(id) >= 0 && int(id) < len(vm.types)) do return {}, .Unknown_Type
	return vm.types[id], nil
}

@(require_results)
get_type_id_by_name :: proc(vm : VM, name : string) -> (id : TypeID, err : Error) {
	if vm == nil do return	-1, .No_VM
	if name == "" do return -1, .Invalid_Name
	for &t in vm.types {
		(t.name == name) or_continue
		
		// Found type
		return t.id, nil
	}
	
	// No matching type
	return -1, .Unknown_Type
}

@(require_results)
types_match :: proc(vm : VM, a, b : TypeID) -> bool {
	(min(a, b) >= 0) or_return
	if a == b do return true
	
	// Find base types
	// NOTE: there is a possibility that
	//		 both could be -1 (invalid),
	//		 intelligent to account for
	return get_base_type(vm, a, false) == get_base_type(vm, b, false)
}

@(require_results)
types_can_cast :: proc(vm : VM, a, b : TypeID) -> bool {
	(min(a, b) >= 0) or_return
	if a == b do return true
	
	base_a, base_b :=
		get_base_type(vm, a, true),
		get_base_type(vm, b, true)
	
	if base_a == base_b do return true
	
	// Analyze Types
	type_a, type_b : Type
	t_err : Error
	
	type_a, t_err = get_type(vm, base_a)
	(t_err == nil) or_return
	
	type_b, t_err = get_type(vm, base_b)
	(t_err == nil) or_return
	
	switch &b in type_a.body {
	case IntBody, ByteBody:
		#partial switch &bb in type_b.body {
		case IntBody,
			 FloatBody,
			 ByteBody,
			 BoolBody,
			 PointerBody:
			 	return true
		case: 	return false
		}
		
	case BoolBody, FloatBody:
		#partial switch &bb in type_b.body {
		case IntBody,
			 FloatBody,
			 ByteBody,
			 BoolBody:
			 	return true
		case: 	return false
		}
		
	case ArrayBody:
		
		// Analyze arrays
		bb := type_b.body.(ArrayBody) or_return
		(b.size == bb.size) or_return
		(	get_base_type(vm,  b.base_type, true) == 
		 	get_base_type(vm, bb.base_type, true)) or_return
		
		return true
	
	case PointerBody:
		
		if bb, ok := type_b.body.(PointerBody); ok {
			return 	get_base_type(vm,  b.base_type, true) == 
			 		get_base_type(vm, bb.base_type, true)
		}
		
		#partial switch &bb in type_b.body {
		case IntBody, ByteBody:
				return true
		case:	return false
		}
	
	case ReferenceBody: // Should be impossible
		assert(false, "References shouldn't exist here...")
	
	// Don't even try to deal with structs
	case StructBody: return false
	}
	
	return false
}

@(require_results)
/* --- get_base_type ---
 * gets the underlying base type
 * of a given TypeID
 *
 * ignore_unique works through
 * unique type references
 */
get_base_type :: proc(vm : VM, id : TypeID, ignore_unique : bool) -> TypeID {
	if id < 0 do return -1
	
	t, t_err := get_type(vm, id)
	if t_err != nil do return -1
	
	#partial switch &b in t.body {
	case ReferenceBody:
		if b.unique && !ignore_unique do return id
		return get_base_type(vm, b.base_type, ignore_unique)
	
	case: return id
	}
	
	return -1
}

