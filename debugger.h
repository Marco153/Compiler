#pragma once
#include "rel_utils.h"

struct dbg_type
{
	int type;
	char ptr;
	rel_ptr<int, dbg_type> data;
};
struct dbg_decl
{
	rel_ptr<int, char> name;
	rel_ptr<int, dbg_type> type;

	char reg;
	int offset;
};
struct dbg_scp
{
	rel_array<rel_ptr<int, dbg_scp>> children;
	rel_array<dbg_decl> decls;
};
struct dbg_line
{
	unsigned int code_idx;
	unsigned int ln_num;
};
struct dbg_func
{
	rel_ptr<int, char> name;

	unsigned int begin_code_idx;
	unsigned int end_code_idx;
	rel_array<dbg_line> lines;
	rel_ptr<int, dbg_scp> scp;
};
struct dbg_file
{
	rel_ptr<int, char> name;
	rel_array<dbg_func> funcs;
};
struct dbg_globals
{
	rel_array<dbg_file> files;
};

