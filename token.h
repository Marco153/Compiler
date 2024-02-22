#pragma once
#include<vector>
#include<string>
#include "machine_rel.h"
#include "rel_utils.h"


#define ASSERT(cond) if(!(cond)){int *a = nullptr; *a = 1;}
#define FOR_VEC(a, vec) for(auto a = (vec).begin(); a < (vec).end(); a++)

struct decl2;
struct node;
struct type_struct2;
struct type2;
struct func_decl;
struct comma_ret;
struct scope;
struct token2;
struct import_strct;
struct unit_file;

type_struct2 *SearchSerializedStruct(std::string name);
bool GetTypeFromTkns(token2 *tkns, type2 &tp);
bool GetDeclFromTkns(std::vector<token2> *tkns, int *i, decl2 *);
bool GetDeclFromTkns(std::vector<token2> *tkns, int *i, decl2 *);
type2 DescendNode(node *n, scope *scp);
void InsertIntoCharVector(std::vector<char> *vec, void *src, int size);
std::string FuncNameWithTempls(std::string fname, std::vector<type2> *types);
std::string MangleFuncNameWithArgs(func_decl *fdecl, std::string original_name, int start_arg);
decl2 *NewDecl(std::string name, type2 tp);
void TransformSingleFuncToOvrlStrct(decl2 *decl_exist);



enum overload_op
{
	INDEX_OP,
	ASSIGN_OP,
	DEREF_OP,
	COND_EQ_OP,
	FOR_OP,
};
enum tkn_type2
{
	T_WORD,
	T_PLUS,
	T_PIPE,
	T_EQUAL,
	T_AND,
	T_COND_EQ,
	T_COND_NE,
	T_PLUS_EQUAL,
	T_MINUS_EQUAL,
	T_OR,
	T_EXCLAMATION,
	T_MINUS,
	T_DIV,
	T_AT,
	T_OPEN_PARENTHESES,
	T_CLOSE_PARENTHESES,
	T_NEW_LINE,
	T_SEMI_COLON,
	T_OPEN_BRACKETS,
	T_CLOSE_BRACKETS,
	T_NUM,
	T_INT,
	T_FLOAT,
	T_DOLLAR,
	T_OPEN_CURLY,
	T_COLON,
	T_CLOSE_CURLY,
	T_TILDE,
	T_COMMA,
	T_POINT,
	T_GREATER_THAN,
	T_GREATER_EQ,
	T_LESSER_THAN,
	T_LESSER_EQ,
	T_MUL,
	T_AMPERSAND,
	T_PERCENT,

	T_STR_LIT,
	T_APOSTROPHE,

	T_TWO_POINTS,

	T_THREE_POINTS,

	T_EOF,
};
enum enum_type2
{
	TYPE_AUTO,
	TYPE_ARRAY_TYPE,
	TYPE_STATIC_ARRAY,
	TYPE_ARRAY,
	TYPE_ARRAY_DYN,

	TYPE_ENUM_IDX_32,

	TYPE_OVERLOADED_FUNCS,

	TYPE_VOID,
	TYPE_VOID_TYPE,

	TYPE_RANGE,

	//
	TYPE_STRUCT_TYPE,
	TYPE_CHAR_TYPE,
	TYPE_STR_LIT,
	TYPE_STATIC_ARRAY_TYPE,
	TYPE_U64_TYPE,
	TYPE_U32_TYPE,
	TYPE_U16_TYPE,
	TYPE_U8_TYPE,
	TYPE_S64_TYPE,
	TYPE_S32_TYPE,
	TYPE_S16_TYPE,
	TYPE_S8_TYPE,
	TYPE_BOOL_TYPE,
	TYPE_UNION_TYPE,
	TYPE_FUNC_TYPE,
	TYPE_ENUM_TYPE,
	TYPE_F32_TYPE,
	TYPE_F64_TYPE,
	//

	TYPE_INT,

	TYPE_IMPORT,

	TYPE_REL_TYPE,
	TYPE_REL,

	TYPE_U64,
	TYPE_U32,
	TYPE_U16,
	TYPE_U8,
	TYPE_S64,
	TYPE_S32,
	TYPE_S16,
	TYPE_S8,
	TYPE_F32,
	TYPE_F64,
	TYPE_BOOL,
	TYPE_CHAR,

	TYPE_UNION_DECL,
	TYPE_UNION,

	TYPE_ENUM_DECL,
	TYPE_ENUM,

	TYPE_MACRO_EXPR,

	TYPE_STRUCT_DECL,
	TYPE_STRUCT,
	TYPE_STRUCT_ANON,
	TYPE_BUILT_IN_MACRO,
	TYPE_FUNC_DECL,
	TYPE_FUNC_PTR,
	TYPE_FUNC_DEF,
	TYPE_FUNC_EXTERN,
	TYPE_FUNC,
	TYPE_COMMA,
	TYPE_TEMPLATE,
};
struct type_data
{
	union
	{
		// offset holds a ptr to the another strct type_data
		long long offset;
		// entries is how many members a strct has
		int entries;
	};
	enum_type2 tp;
	short name;
	short name_len;

};
struct template_expr
{
	std::string name;
	int type;
	type2 *final_type;
	node *expr;
	node *func;
	scope *scp;
};
struct func_overload_strct
{
	std::string name;

	overload_op ovrld_op;
	std::vector<func_decl *> fdecls;
};
#define TYPE_CONST_WAS_DECLARED 1
#define TYPE_IS_REL   2
#define TYPE_SELF_REF 4
#define TYPE_STRCT_WAS_FROM_VAL 8
#define TYPE_VAR_ARGS_STRCT 0x10
#define TYPE_NOT_INSTANTIATED_YET 0x20
#define TYPE_FLAGS_AUTO_CAST 0x40
#define TYPE_FUNC_RECURSIVE 8
struct type2
{
	enum_type2 type;
	char ptr;
#ifndef COMPILER
	rel_ptr<type_struct2> strct;
#else
	bool is_const;
	int flags;
	union
	{
		int i;
		float f;

		unsigned char u8;
		unsigned short u16;
		unsigned int u32;
		unsigned long long u64;

		int str_len;

		char s8;
		short s16;
		int s32;
		long long s64;
		type_struct2 *strct;
		std::string template_name;
		void (*macro_builtin)(node *, node *, node *, void *);

		func_decl *fdecl;
		func_overload_strct *overload_funcs;
		import_strct *imp;

		node *nd;

		struct
		{
			scope *scp;
			decl2 *e_decl;
		};

		template_expr templ;

		struct
		{
			enum_type2 rel_lhs;
			type2 *rel_rhs;
		};
		struct
		{
			union
			{
				std::string templ_name;
				int ar_size;
			};
			int *decl_offset;
			
			type2 *tp;
		};
		struct
		{
			decl2 *from_enum;
			int e_idx;
		};
	};
	// ***
	// DONT PUT ANY VARIABLES PAST THIS UNION
	// ***

#endif
	decl2 *GetEnumDecl(std::string name);
	func_decl *ChooseFuncOverload(std::vector<type2> *tps);
	bool IsStrct()
	{
		return type == enum_type2::TYPE_STRUCT;
	}
	bool IsFloat()
	{
		return type == TYPE_F32 || type == TYPE_F64;
	}
	type2()
	{
		memset(this, 0, sizeof(*this));
	}
	type2 operator=(const type2 &other)
	{
		memcpy(this, &other, sizeof(*this));
		return *this;
	}
	type2(const type2 &other)
	{
		memcpy(this, &other, sizeof(*this));
	}
	~type2(){}
};
struct template_to_be_assigned
{
	std::string name;
	type2 **ptr;
};
#define FUNC_DECL_IS_LAMBDA   0x1
#define FUNC_DECL_IS_OUTSIDER 0x2
#define FUNC_DECL_IS_OP_OVERLOAD 0x4
#define FUNC_DECL_IS_DONE 0x8
#define FUNC_DECL_TEMPLATES_DECLARED_TO_SCOPE 0x10
#define FUNC_DECL_EXTERN 0x20
#define FUNC_DECL_TEMPLATED 0x40
#define FUNC_DECL_VAR_ARGS 0x80
#define FUNC_DECL_CONSTRUCTOR 0x100
#define FUNC_DECL_INTERNAL 0x200
#define FUNC_DECL_LINK_NAME 0x400
#define FUNC_DECL_TEST 0x800
#define FUNC_DECL_ARGS_GOTTEN 0x1000
#define FUNC_DECL_MACRO 0x2000
#define FUNC_DECL_NAME_INSERTED 0x4000
#define FUNC_DECL_ALIGN_STACK_WHEN_CALL 0x8000

struct func_decl
{
	std::string name;
	std::string link_name;

	std::vector<decl2 *> args;
	std::vector<decl2 *> vars;
	std::vector<node *> stmnts;
	std::vector<template_expr> templates;
	std::vector<template_to_be_assigned> temps_to_be_assigned;
	int flags;
	//tkn_type2 op_overload;
	overload_op op_overload;
	type2 ret_type;
	node *func_node;
	scope *scp;

	node *reached_nd;

	unit_file *from_file;

	decl2 *this_decl;


	int code_start_idx;

	int stack_size;

	int biggest_saved_regs = 2;
	int biggest_call_args;

	int biggest_saved_lhs;

	int saved_regs_offset;

	int saved_rsp_offset;

	int saved_lhs_offset;

	int per_stmnt_strct_val_sz;
	int per_stmnt_strct_val_offset;

	int total_of_var_args;

	// strct_vals holds the func's args that are struct vals
	int strct_vals_offset;
	int strct_vals_sz;

	int strct_val_ret_offset;

	int call_strcts_val_sz;
	int call_strcts_val_offset;

	int array_literal_offset;
	int array_literal_sz;


	int var_args_start_offset;
	/*
	func_decl *NewFuncComplete()
	{
		auto ret = this->new_func();
		ret->func_node = this->func_node->NewTree();
		ret->scp   = NewScope(this->scp);
		return ret;

	}
	*/
	func_decl *new_func()
	{
		auto ret = (func_decl *)malloc(sizeof(func_decl));
		memset(ret, 0, sizeof(func_decl));
		//memcpy(ret, this, sizeof(func_decl));
		//ret->args.assign(args.begin(), args.end());
		ret->func_node = this->func_node;
		ret->templates.assign(templates.begin(), templates.end());
		ret->temps_to_be_assigned.assign(temps_to_be_assigned.begin(), temps_to_be_assigned.end());
		ret->scp = nullptr;
		ret->from_file = from_file;
		return ret;
	}
};
#define DECL_NOT_DONE  0x1
// this is used for vars inside structs that are after a using
#define DECL_FROM_USING  0x2
#define DECL_IS_ARG  0x4
#define DECL_INSERT_VAR_ARGS_AR  0x8
#define DECL_IS_GLOBAL  0x10
struct decl2
{
#ifndef COMPILER
	rel_ptr<char> name;
#else
	std::string name;
#endif
	type2 type;
	int offset;
	int flags;

	node *using_node;
	node *bottom_n;

	node *decl_nd;

	unit_file *from_file;

	bool AssignTemplate(std::string tname, type2 *tp, comma_ret *);
	decl2()
	{
		memset(this, 0, sizeof(*this));
	}
	~decl2(){}
};


decl2 *FindDeclFunc(std::vector<decl2> *vec, bool(*func)(decl2 *))
{
	FOR_VEC(it, *vec)
	{
		if(func(&*it))
			return &*it;
	}
	return nullptr;
}

std::string OvrldOpToStr(overload_op op);

#define TP_STRCT_TEMPLATED 1
#define TP_STRCT_STRUCT_NOT_NODE 2
struct type_struct2
{
#ifndef COMPILER
	rel_ptr<char> name;
	rel_array<decl2> vars;
#else
	std::string name;
	std::vector<decl2 *> vars;

	decl2 *this_decl;

	unit_file *from_file;

	std::vector<template_expr> templates;
	std::vector<template_to_be_assigned> temps_to_be_assigned;
	std::vector<func_decl *> op_overloads;
	std::vector<decl2 *> op_overloads_funcs;
	std::vector<func_overload_strct> op_overloads_intern;
	std::vector<func_overload_strct> constructors;

	std::vector<type_struct2 *> instantiated_strcts;
	type_struct2 *original_strct;
	node *strct_node;
	scope *scp;

	int size;
	int flags;
	int biggest_type;
	int type_sect_offset;
	type_struct2()
	{
		memset(this, 0, sizeof(type_struct2));
	}
	//std::vector<std::string> templates;
	//std::vector<type2> templates_insantiated;
	func_decl *CreateNewOpOverload(func_decl *original, overload_op tp);
	void AddNewConstrctor(func_decl *fdecl)
	{
		constructors[0].fdecls.push_back(fdecl);
	}
	void AddOpOverload(func_decl *fdecl, overload_op op)
	{
		op_overloads.push_back(fdecl);


		decl2 *found_op = nullptr;
		FOR_VEC(f, op_overloads_funcs)
		{
			if (op == (*f)->type.fdecl->op_overload)
			{
				found_op = (*f);
				break;
			}
		}
	
		if(found_op)
		{
			if(found_op->type.type == TYPE_FUNC)
				TransformSingleFuncToOvrlStrct(found_op);
				
			if (IS_FLAG_OFF(fdecl->flags, FUNC_DECL_NAME_INSERTED))
				fdecl->name = MangleFuncNameWithArgs(fdecl, this->name, 1) + OvrldOpToStr(fdecl->op_overload);

			found_op->type.overload_funcs->fdecls.push_back(fdecl);
		}
		else
		{
			type2 f_tp;
			f_tp.type  = TYPE_FUNC;
			f_tp.fdecl = fdecl;

			std::string fname = this->name + OvrldOpToStr(fdecl->op_overload);

			//if (fdecl->op_overload == COND_EQ_OP)
			if (IS_FLAG_OFF(fdecl->flags, FUNC_DECL_NAME_INSERTED))
				fdecl->name = fname.substr();

			op_overloads_funcs.push_back(NewDecl(fname.substr(), f_tp));
		}
		fdecl->flags |= FUNC_DECL_NAME_INSERTED;
	}
#endif
	func_decl *FindOpOverload(overload_op tp, std::vector<type2> * = nullptr);
	func_decl *FindExistingOverload(std::vector<func_overload_strct> *funcs, void * op, std::vector<type2> *tps, bool search_operator_ovrld);
	decl2 *FindDecl(std::string name)
	{
#ifdef COMPILER
		for(int i = 0; i < this->vars.size(); i++)
		{
			auto decl = this->vars[i];
			if(decl->name == name)
			{
				return decl;
			}
		}
		return nullptr;
#endif
	}
#define NEXT_TYPE_DATA(st, name_sz) (type_data *)(((char*)(st + 1)) + (name_sz))
	void ToTypeSect(std::vector<std::vector<char>> *str_tbl, std::vector<char> *type_sect, int *str_tbl_sz)
	{
		// the name of the struct and variables go at the of the struct
		std::vector<char> buffer;
		std::vector<char> strct_str_tbl;
		//buffer.resize(128);
		//strct_str_tbl.resize(128);

		buffer.insert(buffer.end(), sizeof(type_data), 0);
		
		type_data *strct_ptr = (type_data *)buffer.data();

		int offset_to_str_tbl = sizeof(type_data) *(vars.size() + 1);
		offset_to_str_tbl    -= offsetof(type_data, name);

		strct_ptr->name = offset_to_str_tbl;
		strct_ptr->name_len = this->name.length();
		strct_ptr->tp = enum_type2::TYPE_STRUCT_DECL;
		strct_ptr->entries = vars.size();

		InsertIntoCharVector(&strct_str_tbl, (void *)this->name.data(), this->name.size());

		type_data *var_ptr = nullptr;

		int i = 0;
		FOR_VEC(v, vars)
		{
			if (IS_FLAG_ON((*v)->flags, DECL_FROM_USING))
				continue;

			int last_size = buffer.size();
			buffer.insert(buffer.end(), sizeof(type_data), 0);

			var_ptr = (type_data*)&buffer[last_size];
			
			offset_to_str_tbl  = (vars.size() - i) * sizeof(type_data);
			offset_to_str_tbl += strct_str_tbl.size();
			offset_to_str_tbl -= offsetof(type_data, name);

			var_ptr->tp = (*v)->type.type;
			var_ptr->name = offset_to_str_tbl;
			var_ptr->name_len = (*v)->name.length();

			InsertIntoCharVector(&strct_str_tbl, (void *)(*v)->name.data(), (*v)->name.size());

			if((*v)->type.type == enum_type2::TYPE_STRUCT)
				var_ptr->offset = (*v)->type.strct->type_sect_offset;

			i++;
		}

		InsertIntoCharVector(type_sect, (void *)buffer.data(), buffer.size());
		InsertIntoCharVector(type_sect, (void*)strct_str_tbl.data(), strct_str_tbl.size());
	}
};

#define TKN_FLAGS_IS_NEW_LINE 1

struct token2
{
	tkn_type2 type;
	int line;
	int line_offset;
	char *line_str;
	union
	{
		std::string str;
		union
		{
			int i;
			long long i64;
			unsigned long long u64;
		};
		float f;
	};
	int flags;

	token2(){}
	token2 operator=(const token2& other) 
	{
		token2 t;
		memcpy(this, &other, sizeof(other));

		return t;
	}
	token2(const token2& other) 
	{
		memcpy(this, &other, sizeof(*this));
	}
	~token2(){}

	std::string ToString();
	
	token2 *NewTkn()
	{
		auto ret = (token2 *)AllocMiscData(sizeof(token2));
		memset(ret, 0, sizeof(token2));
		
		if (type == T_WORD)
		{
			ret->line = line;
			ret->line_str = line_str;
			ret->line_offset = line;
			ret->str = str.substr();
		}
		else
		{
			memcpy(ret, this, sizeof(token2));
		}
		
		return ret;
	}
};
int IsTknWordStr(token2 *tkn, std::string str)
{
	return tkn->type == tkn_type2::T_WORD && tkn->str == str;
}
int FindToken(std::vector<token2> *to_search, int start, bool(*func)(tkn_type2))
{
	int i = 0;
	int max = to_search->size();
	tkn_type2 cur = tkn_type2::T_PLUS;
	while((start + i) < max)
	{
		cur = (*to_search)[start + i].type;
		if(func(cur))
			break;
		i++;
	}
	return start + i;
}
int FindWordAtSameScope(std::vector<token2> *to_search, int start, int end, std::string target)
{
	int level = 0;
	tkn_type2 ch = (tkn_type2)0;
	int i = 0;
	while (level >= 0 && (start + i) < end)
	{
		auto cur_tkn = &(*to_search)[start + i];
		ch = cur_tkn->type;

		if (ch == tkn_type2::T_CLOSE_CURLY)
		{
			level--;
		}
		if (ch == tkn_type2::T_OPEN_CURLY)
		{
			level++;
		}
		if(IsTknWordStr(cur_tkn, target) && level == 0)
		{
			return start + i;
		}
		i++;
	}
	return start + i;
}
int FindTokenClose(std::vector<token2> *to_search, int start, tkn_type2 target)
{
	int level = 0;
	tkn_type2 ch = (tkn_type2)0;
	int i = 0;
	while (level >= 0)
	{
		ch = (*to_search)[start + i].type;

		if (target == tkn_type2::T_OPEN_CURLY && ch == tkn_type2::T_CLOSE_CURLY)
		{
			level--;
		}
		if (target == tkn_type2::T_OPEN_CURLY && ch == tkn_type2::T_OPEN_CURLY)
		{
			level++;
		}
		if (target == tkn_type2::T_LESSER_THAN && ch == tkn_type2::T_GREATER_THAN)
		{
			level--;
		}
		if (target == tkn_type2::T_LESSER_THAN && ch == tkn_type2::T_LESSER_THAN)
		{
			level++;
		}
		if (target == tkn_type2::T_OPEN_PARENTHESES && ch == tkn_type2::T_CLOSE_PARENTHESES)
		{
			level--;
		}
		if (target == tkn_type2::T_OPEN_PARENTHESES && ch == tkn_type2::T_OPEN_PARENTHESES)
		{
			level++;
		}
		i++;
	}
	i--;
	return start + i;
}
std::string token2::ToString()
{
	switch(type)
	{
		case tkn_type2::T_PIPE:
		{
			return std::string("|");
		}break;
		case tkn_type2::T_EXCLAMATION:
		{
			return std::string("!");
		}break;
		case tkn_type2::T_COLON:
		{
			return std::string(":");
		}break;
		case tkn_type2::T_PLUS:
		{
			return std::string("+");
		}break;
		case tkn_type2::T_NEW_LINE:
		{
			return std::string("\n");
		}break;
		case tkn_type2::T_AT:
		{
			return std::string("@");
		}break;
		case tkn_type2::T_FLOAT:
		{
			return std::to_string(this->f);
		}break;
		case tkn_type2::T_INT:
		{
			return std::to_string(this->i);
		}break;
		case tkn_type2::T_WORD:
		{
			return this->str;
		}break;
		case tkn_type2::T_POINT:
		{
			return std::string(".");
		}break;
		case tkn_type2::T_COMMA:
		{
			return std::string(",");
		}break;
		case tkn_type2::T_CLOSE_CURLY:
		{
			return std::string("\n}\n");
		}break;
		case tkn_type2::T_OPEN_CURLY:
		{
			return std::string("\n{\n");
		}break;
		case tkn_type2::T_LESSER_THAN:
		{
			return std::string("<");
		}break;
		case tkn_type2::T_GREATER_THAN:
		{
			return std::string(">");
		}break;
		case tkn_type2::T_OPEN_BRACKETS:
		{
			return std::string("[");
		}break;
		case tkn_type2::T_CLOSE_BRACKETS:
		{
			return std::string("]");
		}break;
		case tkn_type2::T_OPEN_PARENTHESES:
		{
			return std::string("(");
		}break;
		case tkn_type2::T_CLOSE_PARENTHESES:
		{
			return std::string(")");
		}break;
		case tkn_type2::T_MINUS:
		{
			return std::string("-");
		}break;
		case tkn_type2::T_EQUAL:
		{
			return std::string("=");
		}break;
		case tkn_type2::T_AMPERSAND:
		{
			return std::string("&");
		}break;
		case tkn_type2::T_SEMI_COLON:
		{
			return std::string(";\n");
		}break;
		case tkn_type2::T_MUL:
		{
			return std::string("*");
		}break;
	}
	return std::string("");
}
std::string StringifyTkns(std::vector<token2> *tkns, int start, int amount)
{
	std::string final_ret;
	final_ret.reserve(64);
	for(int i = 0; i < amount; i++)
	{
		final_ret.append((*tkns)[start + i].ToString());
		if (i < (amount - 1))
		{
			final_ret.append(" ");
		}
	}
	return final_ret;
}
bool is_type_unsigned(enum_type2 tp)
{
	switch(tp)
	{
	case enum_type2::TYPE_U64:
	case enum_type2::TYPE_U32:
	case enum_type2::TYPE_U16:
	case enum_type2::TYPE_U8:
		return true;

	case enum_type2::TYPE_S64:
	case enum_type2::TYPE_S32:
	case enum_type2::TYPE_S16:
	case enum_type2::TYPE_S8:
		return false;
	}
}
struct comma_ret
{
	int type;
	union
	{
		type2 tp;
		decl2 decl;
	};
	node *n;
	comma_ret()
	{
		memset(this, 0, sizeof(comma_ret));
	}
	comma_ret operator=(const comma_ret& other) 
	{
		comma_ret c;
		memcpy(this, &other, sizeof(other));

		return c;
	}
	comma_ret(const comma_ret &other)
	{
		memcpy(this, &other, sizeof(comma_ret));
	}
	~comma_ret(){
	}
};
func_decl *type2::ChooseFuncOverload(std::vector<type2> *tps)
{
	std::string name = FuncNameWithTempls(overload_funcs->name, tps);;

	FOR_VEC(f, overload_funcs->fdecls)
	{
		if((*f)->name == name)
			return *f;
	}
	return nullptr;
}
