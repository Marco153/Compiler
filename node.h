#pragma once
#include "token.h"
#include <queue>

#define NODE_FLAGS_IS_PARENTHESES 0x1
#define NODE_FLAGS_IS_SCOPE 0x2
#define NODE_FLAGS_IS_PROCESSED 0x4
#define NODE_FLAGS_IS_BRACKETS 0x8
#define NODE_FLAGS_INDEX_IS_TYPE 0x10
#define NODE_FLAGS_COLON_INFER 0x20
#define NODE_FLAGS_FUNC_OUTSIDER 0x40
#define NODE_FLAGS_FUNC_CONSTRUCTOR 0x80
#define NODE_FLAGS_IS_PROCESSED2 0x100
#define NODE_FLAGS_FUNC_INTERNAL 0x200
#define NODE_FLAGS_FUNC_LINK_NAME 0x400
#define NODE_FLAGS_FUNC_TEST      0x800
#define NODE_FLAGS_AR_LIT_ANON   0x1000
#define NODE_FLAGS_CALL_RET_ANON   0x2000
#define NODE_FLAGS_FUNC_MACRO      0x4000
#define NODE_FLAGS_ALIGN_STACK_WHEN_CALL 0x8000
#define NODE_FLAGS_IS_PROCESSED3 0x10000

#define ASSIGN_VEC(v1, v2) v1.assign(v2.begin(), v2. end())
#define INSERT_VEC(v1, v2) v1.insert(v1.end(), v2.begin(), v2.end())

#define DEBUG_NAME

node *new_node();
node *new_node(node *src);
enum tkn_type2;
struct import_strct;
struct func_byte_code;
struct node;

enum parser_cond
{
	GREATER,
	GREATER_EQUAL,
	LESSER,
	LESSER_EQUAL,
	EQUAL,
};

struct node;
struct node_iter
{
	std::vector<token2> *tkns;
	int cur_idx;
	int cur_scope_count;

	node_iter(std::vector<token2> *v)
	{
		memset(this, 0, sizeof(node_iter));
		tkns = v;
		cur_idx = 0;
	}

	void CheckTwoBinaryOparatorsTogether(node *);

	void node_iter::ExpectTkn(tkn_type2);
	node *node_iter::parse_func_like();
	node *node_iter::parse_strct_like();
	node *node_iter::parse_all();
	node *node_iter::parse_stmnts();
	node *node_iter::parse_sub_expr(int prec);
	node *node_iter::parse_expr();
	node *node_iter::parse_(int prec,  parser_cond);
	void CreateCondAndScope(node **n);

	//Znode *parse_expression();
	node* parse_expression(int);

	node *node_iter::parse_sub_expression();
	node *parse(tkn_type2 target);

	token2 *peek_tkn();
	token2 *get_tkn();

	bool is_operator(token2 *tkn, int *precedence);
	bool is_unary(tkn_type2 *tp);
	node *grow_unary_tree(bool left, node **top);
};
enum keyword
{
	KW_DBG_BREAK,
	KW_RETURN,
	KW_TRUE,
	KW_FALSE,
	KW_FN,
	KW_MATCH,
	KW_RET_TYPE,
	KW_BREAK,
	KW_CONTINUE,
	KW_NIL,
	KW_USING,
	KW_REL,
	KW_CONSTRUCTOR,
};
enum node_type
{
	N_BINOP,
	N_SCOPE,
	N_STMNT,
	N_UNOP,
	N_IDENTIFIER,
	N_APOSTROPHE,
	N_FOR,
	N_WHILE,
	N_IMPORT,
	N_IMPORT_LIB,
	N_TYPE,
	N_CAST,
	N_INT,
	N_FLOAT,
	N_CONST,
	N_KEYWORD,
	N_INDEX,
	N_IF,
	N_ELSE,
	N_ELSE_IF,
	N_IF_BLOCK,
	N_DECL,
	N_CALL,
	N_FUNC_DECL,
	N_OP_OVERLOAD,
	N_FUNC_DEF,
	N_STRUCT_DECL,
	N_SIGNATURE,
	N_VAR_ARGS,
	N_LAMBDA,

	N_ENUM_DECL,
	N_UNION_DECL,

	N_DESUGARED,

	N_STR_LIT,
};
struct stmnt_nd
{
	node *n;
	scope *scp;
};
struct node
{
	node *l;
	node *r;

	token2 *t;

	node_type type;

#ifdef DEBUG_NAME
	node *not_found_nd;
#endif
	union
	{
		tkn_type2 op_type;
		overload_op ovrld_op;
	};

	union
	{
		scope *scp;
		node *call_templates;
		type_struct2 *tstrct;
		bool is_unsigned;

		struct
		{
			func_decl *fdecl;
			std::string *str;
		};

		type2 decl_type;
		type2 *ptr_tp;
		struct
		{
			decl2 *ar_lit_decl;
			type2 *ar_lit_tp;
			int ar_byte_sz;
		};
		keyword kw;
		struct
		{
			int i_var_idx;
		}for_strct;
	};
		
	int flags;
	~node()
	{
	}
	void Free()
	{
		if(l != nullptr)
			l->Free();
		if(r != nullptr)
			r->Free();
		free(this);
	}
	node *NewTree()
	{
		auto ret = new_node(this);
		if (this->t != nullptr)
		{
			ret->t = this->t->NewTkn();
		}


		if(this->l != nullptr)
			ret->l = this->l->NewTree();

		if(this->r!= nullptr)
			ret->r = this->r->NewTree();

		return ret;
		
	}
	void FreeTree()
	{
		if(this->l)
			this->l->FreeTree();
		if(this->r)
			this->r->FreeTree();

		free(this);
		
	}
};
enum msg_type
{
	MSG_BINOP,
	MSG_STMNT,
	MSG_UNOP,
	MSG_IDENTIFIER,
	MSG_INT,
	MSG_FLOAT,
	MSG_KEYWORD,
	MSG_INDEX,
	MSG_IF,
	MSG_ELSE,
	MSG_IF_BLOCK,
	MSG_DECL,
	MSG_CALL,
	MSG_CAST,
	MSG_RETURN,
	MSG_FUNC_DECL,
	MSG_MATCH,
	MSG_DONE,
};
 struct variable
 {
	std::string name;
	type2 type;
 };



#define SCOPE_INSIDE_FUNCTION 1
#define SCOPE_INSIDE_STRUCT   2
#define SCOPE_IS_GLOBAL   4

struct scope
{
	scope *parent;
	std::vector<decl2 *> vars;
	std::vector<scope *> children;
	std::vector<template_to_be_assigned> templs_to_be_assigned;

	std::vector<decl2 *> imports;

	int flags;
	int type;

	func_decl *fdecl;
	type_struct2 *tstrct;

	decl2 *FindVariable(std::string name);
};
struct message
{
	msg_type type;
	node *n;
	type2 lhs_type;
	type2 rhs_type;
	node *stmnt;
	scope *scp;
};
struct unit_file
{
	std::string name;
//	std::string contents;
	char* contents;
	unsigned long long contents_sz;
	std::vector<token2> tkns;
	std::vector<func_byte_code *> bc_funcs;

	std::vector<char *> lines;

	scope *global;
	node *s;
};

bool decl2::AssignTemplate(std::string tname, type2 *tp, comma_ret *given_arg)
{
	switch(type.type)
	{
	case enum_type2::TYPE_AUTO:
	{
		type = *tp;
		return true;
	}break;
	case enum_type2::TYPE_TEMPLATE:
	{
		if(type.templ_name == tname)
		{
			type.tp = tp;
			return true;
		}
	}break;
	case enum_type2::TYPE_FUNC_PTR:
	{
		auto func_ptr = type.fdecl;
		int cur_arg = 0;
		FOR_VEC(t, func_ptr->args)
		{
			//original function parameter signature args
			if ((*t)->AssignTemplate(tname, tp, given_arg))
			{
				auto func_ptr_in_call_arg = given_arg->tp.fdecl->args[cur_arg];
				ASSERT(func_ptr_in_call_arg->type.type == enum_type2::TYPE_AUTO)
				// func passed to func call
				func_ptr_in_call_arg->AssignTemplate(tname, tp, given_arg);
			}

			cur_arg++;
		}

		auto func_ptr_in_call= given_arg->tp.fdecl;

		if (func_ptr_in_call->ret_type.type == enum_type2::TYPE_AUTO)
		{
			DescendNode(func_ptr_in_call->func_node->r, func_ptr_in_call->scp);
			func_ptr->ret_type = func_ptr_in_call->ret_type;
		}
	}break;
	}
	return false;
}
enum import_type
{
	IMP_IMPLICIT_NAME,
	IMP_BY_ALIAS,
};
struct import_strct
{
	import_type type;
	std::string alias;
	unit_file *fl;
	decl2 *FindDecl(std::string name)
	{

		ASSERT(fl->global);
		scope* imp_scp = fl->global;
		// searching import scopes vars
		FOR_VEC(var, imp_scp->vars)
		{
			if ((*var)->name == name)
				return *var;
		}
		return nullptr;
	}
};
