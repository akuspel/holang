package holang

/* --- AST ---
 * handle the types, generation
 * and execution of AST
 */

import "core:fmt"

// --- Types ---
@(private="file")
NODE :: ^AST_Node

AST_Node :: struct {
	
	// Each node is part
	// To a scope frame
	scope : FRAME,
	
	body : AST_Body,
	meta : struct {
		token : TokenID,
	}
}

@(private="file")
EXPR :: ^AST_Expression
AST_Expression :: struct {
	
	type : TypeID,
	
	body : AST_ExpressionBody,
	
	meta : struct {
		token : TokenID,
	}
}

AST_ExpressionBody :: union {
	AST_ConstantValue,
	AST_RuntimeExpression,
	AST_MemoryAddress,
	AST_StructLiteral,
	AST_ArrayLiteral,
}

AST_ConstantValue :: struct {
	value : Variant,
}

AST_StructLiteral :: struct {
	values : []AST_StructMember,
}

AST_StructMember :: struct {
	idx : int,
	val : EXPR,
}

AST_ArrayLiteral :: struct {
	values : []AST_ArrayMember,
}

AST_ArrayMember :: struct {
	val : EXPR,
}

AST_MemoryAddress :: struct {
	// E.G. when you're getting
	// Structs / Arrays from an
	// Existing variable
	
	addr : AST_Offset,
	size : int,
}

// -   Expressions   -
AST_ExpressionValue :: union {
	
	Variant,
	Operator,
	Delimiter,
	
	AST_CastExpr,
	AST_VarExpr,
}

AST_CastExpr :: struct {
	using _ : AST_RuntimeExpression,
	type : TypeID,
}

AST_VarExpr :: struct {
	var  : VarID,
	off  : AST_Offset,
	type : TypeID,
}

AST_Offset :: struct {
	single : Maybe(uintptr), // In case offset is constant
	values : []AST_OffsetValue,
}

AST_OffsetValue :: union {
	uintptr,
	AST_OffsetExpr
}

AST_OffsetExpr :: struct {
	
	ceil : int, // Bounds check
	
	size : int, // Single offset size
	expr : EXPR,
}
AST_RuntimeExpression :: struct {
	values : []AST_ExpressionValue,
}

AST_Body :: union {
	
	AST_Branch,
	AST_Conditional,
	
	// Scopes
	AST_Empty,
	AST_For,
	
	// Commands
	AST_Assign,
	
	AST_Return,
	AST_Break,
	AST_Continue,
}

// --  Scopes  --
// NOTE: all scopes have certain
// 		 shared functionality, which
// 		 is represented by AST_Scope
//		 E.G. local 

@(private="file")
FRAME :: ^AST_Frame
AST_Frame :: struct {
	parent    : ^AST_Frame,
	
	variables : [dynamic]Variable,
	nodes     : [dynamic]NODE,
}

AST_Scope :: struct($B : typeid) {
	
	child : FRAME,
	using _ : B,
}

AST_Branch :: struct {
	cond : EXPR,
	
	// Usually a Frame
	if_body   : FRAME,
	else_body : FRAME,
}

// Executes only one command
// When given condition is true
AST_Conditional :: struct {
	cond : EXPR,
	
	child : NODE,
}

// Serves as while loop
// Like in Odin
AST_For :: AST_Scope(
	struct {
		cond : EXPR,
	}
)

// Empty scope
AST_Empty :: AST_Scope(
	struct {}
)

// --  Commands  --
AST_Assign :: struct {
	
	var  : VarID,
	off  : AST_Offset,
	
	type : TypeID,
	expr : EXPR,
}

AST_Return :: struct {
	result : EXPR,
}

AST_Break :: struct {
}

AST_Continue :: struct {
}

// --- Procedures ---
@(private)
ast_allocate_node :: proc(
	vm : VM, state : ^ParseState,
	scope : FRAME,
) -> (node : NODE, err : Error) {
	assert(scope != nil, "Must provide a Scope Frame!")
	
	defer if err != nil {
		assert(false, "Unable to create AST Node!") }
	alloc, alloc_err := vm_get_ast_allocator(vm)
	if alloc_err != nil do return nil, alloc_err
	
	node, alloc_err = new(AST_Node, alloc)
	if alloc_err != nil do return nil, alloc_err
	
	node.meta.token = TokenID(state.token)
	node.scope = scope
	
	return
}

@(private)
ast_allocate_frame :: proc(
	vm : VM, state : ^ParseState,
	scope : FRAME,
) -> (frame : FRAME, err : Error) {
	assert(scope != nil, "Must provide a Scope Frame!")
	
	alloc, alloc_err := vm_get_ast_allocator(vm)
	if alloc_err != nil do return nil, alloc_err
	
	frame, alloc_err = new(AST_Frame, alloc)
	if alloc_err != nil do return nil, alloc_err
	
	frame.parent = scope
	append(&vm.frames, frame)
	
	return
}

@(private)
ast_allocate_expr :: proc(
	vm : VM, state : ^ParseState,
) -> (expr : EXPR, err : Error) {
	
	alloc, alloc_err := vm_get_ast_allocator(vm)
	if alloc_err != nil do return nil, alloc_err
	
	expr, alloc_err = new(AST_Expression, alloc)
	if alloc_err != nil do return nil, alloc_err
	
	return
}

@(private)
ast_create_variable :: proc(
	vm : VM, state : ^ParseState,
	scope : FRAME, variable : Variable
) -> (id : VarID, err : Error) {
	assert(scope != nil, "Must provide a Scope Frame!")
	_, err = append(&scope.variables, variable)
	if err != nil do return -1, err
	
	return VarID(
		ast_count_variables(scope) - 1
	), nil
}

@(private)
ast_append_node :: proc(
	vm : VM, state : ^ParseState,
	scope : FRAME, node : NODE
) -> (err : Error) {
	assert(scope != nil, "Must provide a Scope Frame!")
	assert(node  != nil, "Must provide a AST Node!")
	
	_, err = append(&scope.nodes, node)
	assert(err == nil, "Unable to append to Scope!")
	
	ast_print_node(vm, node)
	return
}

// --- Utils ---
@(private)
ast_count_variables :: proc(
	scope : FRAME
) -> int {
	n : int; if scope.parent != nil {
		n = ast_count_variables(scope.parent)
	};  return len(scope.variables) + n
}

@(private)
ast_get_variable :: proc(
	scope : FRAME,
	id : VarID,
) -> (var : Variable, found : bool) {
	
	if scope.parent != nil do var, found = ast_get_variable(scope.parent, id)
	if found do return
	
	min := ast_count_variables(scope.parent) if
		scope.parent != nil else 0
	
	for v, i in scope.variables {
		(VarID(min + i) == id) or_continue
		
		return v, true
	};  return
}

@(private="file")
ast_print_node :: proc(vm : VM, node : NODE) {
	fmt.printf("AST ")
	
	switch &b in node.body {
	case AST_Assign:
		v, _ := ast_get_variable(node.scope, b.var)
		t, _ := get_type(vm, b.type)
		fmt.println("Assign", v.name, ":", t.name, "= EXPR")
		
	case AST_Branch:	fmt.println("Branch")
	case AST_Break: 	fmt.println("Break")
	case AST_Return:	fmt.println("Return")
	case AST_Continue:	fmt.println("Continue")
	
	case AST_Empty:		fmt.println("Scope")
	case AST_For:		fmt.println("For Loop")
	
	case AST_Conditional:
		fmt.println("Conditional Expression")
	}
}

@(private)
ast_clear_frame :: proc(frame : FRAME) {
	clear(&frame.nodes)
	clear(&frame.variables)
}

@(private)
ast_clean_frame :: proc(frame : FRAME) {
	delete(frame.nodes)
	delete(frame.variables)
}