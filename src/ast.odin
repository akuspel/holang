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
	
	body : union {
		AST_ConstantValue,
		AST_RuntimeExpression,
		AST_StructArrayLiteral,
		AST_MemoryAddress,
	},
	
	meta : struct {
		token : TokenID,
	}
}

AST_ConstantValue :: struct {
	value : Variant,
}

AST_StructArrayLiteral :: struct {
	values : [] struct {
		idx : int,
		val : ^AST_Expression,
	}
}

AST_MemoryAddress :: struct {
	// E.G. when you're getting
	// Structs / Arrays from an
	// Existing variable
	
	addr : uintptr,
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
	off  : uintptr,
	type : TypeID,
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
	off  : uintptr,
	
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
	
	return VarID(len(scope.variables) - 1), nil
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
	
	fmt.println("AST", node)
	return
}