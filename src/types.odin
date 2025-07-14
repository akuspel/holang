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
