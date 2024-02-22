#include "compile.h"
#include <windows.h>
#include <iostream>
#include <string>
#include <chrono> 
#include <thread>
#include <vector>

#include "udis86.h"
#include "libudis86/types.h"
#include "libudis86/udis86.c"
#include "libudis86/decode.c"
#include "libudis86/syn.c"
//#include "libudis86/syn-att.c"
#include "libudis86/syn-intel.c"
//#include "libudis86/itab.c"

#include "debugger.cpp"
#include "FileIO.cpp"


#define FOR_LESS(var_name, start_val, cond_val) for(int var_name = start_val; var_name < (cond_val); var_name++)
#define IS_FLAG_ON(a, flag) (((a) & (flag)) != 0)
#define IS_FLAG_OFF(a, flag) (((a) & (flag)) == 0)

#define COMPILER

char* AllocMiscData(int sz);

/*
#include "../memory.h"
#include "../sub_system.h"
#include "../Array.cpp"
#include "../memory.cpp"
*/

struct type_struct2;

struct global_variables
{
	char *main_buffer;
	bool string_use_page_allocator = false;

	std::vector<std::string> struct_scope;
	//Array<type_struct2> *structs;
	//Array<type_struct2> *template_strcts;

	//page_allocator *cur_page_allocator = nullptr;
	//sub_systems_enum string_sub_system_operations = sub_systems_enum::STRING;

}globals;

struct node;
struct scope;

void DescendStmntMode(node *stmnt, scope *scp, int mode, void *data);
// Descend Mode DescendNode
#define DMODE_DNODE 1
#define DMODE_DNAME 2
#define DMODE_DFUNC 3

//#include "../sub_system.cpp"
#define STRING_IMPLEMENTATION
//#include "../tokenizer.cpp"
#include "token.cpp"
#include "node.cpp"
#include "bytecode.h"
#include "obj_generator.h"

#include "error_report.cpp"

struct macro_call
{
	//string macro_name;
	//string inner;
	int start_ch;
	int end_ch;
};
/*
struct file
{
	string file_name;
	string contents;
	std::string modified_contents;

	//Array<macro_call> calls;

	string macro_dec;

	file(char *name)
	{
		memset(this, 0, sizeof(*this));
		file_name = name;
	}
};
*/


/*
int FindCharacterClose(string to_search, int start, char target)
{
	int level = 0;
	char ch = 0;
	int i = 0;
	while (level >= 0)
	{
		ch = to_search.data[start + i];
		if (target == '(' && ch == ')')
		{
			level--;
		}
		if (target == '(' && ch == '(')
		{
			level++;
		}
		i++;
	}
	i--;
	return start + i;
}
*/

/*
int SearchIncludeFiles(Array<file> *files, file *f)
{
	int ret = 0;
	if (f->contents.data == nullptr)
	{
		int read = 0;
		f->contents = ReadEntireFile(f->file_name.ToCString(), &read);
		f->modified_contents = std::string(f->contents.data, f->contents.len);
		f->calls.Init(64);
	}
	int i = 0;
	int max = f->modified_contents.length();
	while(i < (max - 1))
	{
		const char* start_buffer = f->modified_contents.data();
		const char *chs = &start_buffer[i];

		int quotes_idx = 1;
		if (chs[0] == '\"')
		{
			i++;
			while (chs[quotes_idx] != '\"')
			{
				quotes_idx++;
				i++;
			}
			i++;
			continue;
		}

		if ( chs[0] == '@' && chs[1] == '@')
		{
			if (f->macro_dec.data != nullptr)
			{
				i += 2;
				continue;
			}
			int first_parentheses = f->contents.FindCharacterIdx('(', false);
			string other = &f->contents.data[first_parentheses + 1];

			int sec_parentheses = FindCharacterClose(other, 0, '(');

			string inner;
			inner.data = &f->contents.data[first_parentheses + 1];
			inner.len  = sec_parentheses - 1;

			f->macro_dec = inner;

			i = first_parentheses + 1 + sec_parentheses + 1;
			return ret;
		}
		if (chs[0] == '@' && chs[1] != '@')
		{
			ret |= 1;
			macro_call call;
			memset(&call, 0, sizeof(call));
			call.start_ch = i;

			string contents;
			contents.data = (char *)f->modified_contents.data();
			contents.len  = f->modified_contents.length();
			NewStringUntilFindCharacters(contents, i + 1, &call.macro_name, " (\r\n\t");

			call.macro_name = call.macro_name.NewComplete();
			int first_parentheses = 0;
			char cur_ch = 0;

			while(cur_ch != '(')
			{
				cur_ch = chs[first_parentheses++];
			}

			int start = i + first_parentheses;
			int sec_parentheses = FindCharacterClose(contents, start, '(');

			call.inner.data = &contents.data[start];
			call.inner.len  = sec_parentheses - start;
			call.inner = call.inner.NewComplete();

			call.end_ch = i + first_parentheses + call.inner.len + 1;

			f->calls.Add(&call);
			i += call.end_ch - call.start_ch;
		}

		i++;
	}
	return ret;
}
*/
	// setting and compiling the macros.cpp file
char *boiler_code = "\
#define FOR_LESS(var_name, start_val, cond_val) for(int var_name = start_val; var_name < (cond_val); var_name++)\n\
#define IS_FLAG_ON(a, flag) (((a) & (flag)) != 0)\n\
#define IS_FLAG_OFF(a, flag) (((a) & (flag)) == 0)\n\
#include <iostream>\n\
#include \"../serializable_pound_defines.h\"\n\
#include <windows.h>\n\
#define COMPILER\n\
#define MACRO_DLL\n\
#include \"../memory.h\"\n\
#include \"../sub_system.h\"\n\
#include \"../Array.cpp\"\n\
#include \"../memory.cpp\"\n\
#include <vector>\n\
#include <string>\n\
struct type_struct2;\n\
struct global_variables\n\
{\n\
	char *main_buffer;\n\
	bool string_use_page_allocator = false;\n\
	std::vector<std::string> struct_scope;\n\
	Array<type_struct2> *structs;\n\
	Array<type_struct2> *template_strcts;\n\
	page_allocator* cur_page_allocator = nullptr; \n\
	sub_systems_enum string_sub_system_operations = sub_systems_enum::STRING;\n\
}globals;\n\
#include \"../sub_system.cpp\"\n\
#define STRING_IMPLEMENTATION\n\
#include \"../string.cpp\"\n\
#include \"token.cpp\"\n\
#include \"node.cpp\"\n\
\
BOOL WINAPI DllMain(\
HINSTANCE hinstDLL,  \
DWORD fdwReason,   \
LPVOID lpReserved ) \
{\
switch (fdwReason)\
{\
case DLL_PROCESS_ATTACH:\
	globals.main_buffer = InitSubSystems(64 * 1024 * 1024);\n\
	break;\
case DLL_THREAD_ATTACH:\
	break;\
\
case DLL_THREAD_DETACH:\
	break;\
\
case DLL_PROCESS_DETACH:\
	break;\
}\
return TRUE;\
}\
extern \"C\" __declspec(dllexport)void *GetGlobalsAddr(){return &globals;}\
";

std::string GetTypeInfoArrayStr(std::string str)
{
	std::string final_ret;
	final_ret.reserve(64);

	for(auto c : str)
	{

		char aux[16];
		sprintf_s(aux, 16, "\'\\x%x\', ", (unsigned char)c);
		final_ret.append(aux);
	}
	return final_ret;
}

void MapMacro(node *n, node *marco_inner, node *stmnt, void *out)
{
	int a = 0;
}
void ttt()
{
}
void call(int a, int b, int c, int d, int e, int f, int g)
{
	int gss = 9;
	int dss = 10;
	d = d + e;
	ttt();
	return;
}

char* std_str_to_heap(std::string* str);

void NewTypeToSection(char* type_name, enum_type2 idx)
{
	std::string final_name = std::string("$$") + type_name;
	prs_globals.type_sect_syms.push_back(
		machine_sym(SYM_TYPE_DATA, (unsigned int)prs_globals.type_sect.size(), std_str_to_heap(&final_name))
	);

	auto& buffer = prs_globals.type_sect;
	int sz = buffer.size();
	buffer.insert(buffer.end(), sizeof(type_data), 0);
	type_data* strct_ptr = (type_data*)((char *)buffer.data() + sz);
	strct_ptr->name = 0;
	strct_ptr->tp = idx;
}
int CreateDbgScope(std::vector<byte_code> *byte_codes, int start)
{

	int i = 0;
	for (auto bc = byte_codes->begin() + start; bc < byte_codes->end(); bc++)
	{
		i++;
	}
	return i;
}
struct dbg_func_info
{
	std::vector<dbg_line> lines;
};
struct dbg_scp_info
{
	char *data;

	char *str_tbl_offset;
	int str_tbl_sz;


	dbg_decl *decls_offset; 
	int decls_added;

	int total_sz;

};
struct dbg_reloc
{
	int type;
	char *to_fill;
	union
	{
		enum_type2 tp;
		char *strct_name;
	};
};
struct dbg_state
{
	dbg_scp_info *scp_info;
	dbg_func_info *cur_func;

	std::vector<dbg_reloc> rels;
	std::vector<dbg_scp> scps;
	std::vector<dbg_decl> decls;
	std::vector<dbg_type> types;
	std::vector<dbg_func> funcs;

	
};


void FillDbgScpInfo(dbg_scp_info *info, scope *ref_scp, dbg_state *state)
{
	int decls_sz = ref_scp->vars.size() * sizeof(dbg_decl);
	int name_sz = 0;

	FOR_VEC(d, ref_scp->vars)
	{
		name_sz += (*d)->name.length();
	}

	info->data           = AllocMiscData(decls_sz + name_sz);
	info->decls_offset   = (dbg_decl *)info->data;
	info->str_tbl_offset = info->data + decls_sz;

	//info->total_sz = total_sz;
	
	int cur_decl = 0;

	// assigning the decls to buffer
	dbg_decl *d_decl = (dbg_decl *)info->decls_offset;
	FOR_VEC(dd, ref_scp->vars)
	{
		auto d = *dd;
		//gettind the dst name addr
		char *name_offset = (char *)(info->str_tbl_offset + info->str_tbl_sz); 

		d_decl->name = (char *)(info->str_tbl_offset + info->str_tbl_sz);

		int name_sz = d->name.length();

		memcpy(name_offset, d->name.data(), name_sz);

		// one is because the string is null-terminated
		info->str_tbl_sz += name_sz + 1;

		dbg_reloc rel;
		
		if(d->type.type == TYPE_STRUCT)
		{
			rel.type = 1;
			rel.strct_name = (char *)d->type.strct->name.data();
		}
		else
		{
			rel.type = 0;
			rel.tp   = d->type.type;
		}

		if(IS_FLAG_ON(d->flags, DECL_IS_ARG))
			d_decl->reg = 5;
		else
			d_decl->reg = 4;

		d_decl->offset = d->offset;

		
		rel.to_fill = (char *)&d_decl->type;
		state->rels.push_back(rel);
		
		d_decl++;
		cur_decl++;
	}
}
dbg_file *GetDbgFile(char *name)
{
	return nullptr;
}

int CreateDbg(std::vector<byte_code> *byte_codes, int start, dbg_state *state)
{
	std::vector<dbg_line> lines;

	int i = 0;

	dbg_scp_info *last_scp;

	while(i < byte_codes->size())
	{
		auto bc = &(*byte_codes)[i];

		switch(bc->type)
		{
		case BEGIN_SCP:
		{
			last_scp = state->scp_info;
			auto new_scp = (dbg_scp_info *)AllocMiscData(sizeof(dbg_scp_info));;

			FillDbgScpInfo(new_scp, bc->scp, state);

			state->scp_info = new_scp;

			i += CreateDbg(byte_codes, i + 1, state);
		}break;
		case END_SCP:
		{
			state->scp_info = last_scp;
		}break;
		case BEGIN_FUNC:
		{
			last_scp = state->scp_info;
			auto new_scp  = (dbg_scp_info *)AllocMiscData(sizeof(dbg_scp_info));;
			auto new_func = (dbg_func_info *)AllocMiscData(sizeof(dbg_func_info));


			FillDbgScpInfo(new_scp, bc->fdecl->scp, state);
			
			state->scp_info = new_scp;
			state->cur_func = new_func;
			i += CreateDbg(byte_codes, i + 1, state);
		}break;
		case END_FUNC:
		{
			state->cur_func = nullptr;
			state->scp_info = last_scp;
		}break;
		case NEW_LINE:
		{
			dbg_line ln;
			ln.code_idx = bc->machine_code_idx;
			ln.ln_num  = bc->line;

			state->cur_func->lines.push_back(ln);
		}break;
		}

		i++;
	}

	return i;
}

int main(int argc, char **argv)
{
	char exe_path[256];
	// setting current directory to be the exe name
	GetModuleFileName(NULL, exe_path, 256);
	// not taking the exe name
	auto last_bar = std::string(exe_path).find_last_of("\\");
	auto h = SetCurrentDirectory(std::string(exe_path, last_bar).c_str());

	//CreateWindowEx()
	//glfwInit();
	//HANDLE hStdOut = GetStdHandle(STD_OUTPUT_HANDLE);
	//SetConsoleTextAttribute(hStdOut, FOREGROUND_RED | BACKGROUND_BLUE);
	//printf("hello");

	prs_globals.max_nd = 20000 * 2;

	//prs_globals.node_arena = InitSubSystems(64 * 1024 * 1024);
	prs_globals.max_misc = 16 * 1024 * 1024;
	prs_globals.misc_arena = (char*)VirtualAlloc(0, prs_globals.max_misc, MEM_COMMIT, PAGE_READWRITE);
	prs_globals.node_arena = (node*)VirtualAlloc(0, prs_globals.max_nd * sizeof(node), MEM_COMMIT, PAGE_READWRITE);
	/*
	globals.structs = (Array<type_struct2> *)malloc(sizeof(Array<int>));
	globals.template_strcts = (Array<type_struct2> *)malloc(sizeof(Array<int>));
	memset(globals.structs, 0, sizeof(Array<int>));
	memset(globals.template_strcts, 0, sizeof(Array<int>));
	*/

	ASSERT(argc > 1);

	auto a = ParseString("a = a * a + a * a + a * a");

	auto dummy_decl = new decl2();
	memset(dummy_decl, 0, sizeof(decl2));

	// adding sizeof builtin
	prs_globals.root = NewScope(nullptr);
	prs_globals.funcs_scp = NewScope(nullptr);

	type2 tp;
	tp.type = enum_type2::TYPE_FUNC;
	func_decl* sz_of_fdecl = (func_decl*)AllocMiscData(sizeof(func_decl));
	memset(sz_of_fdecl, 0, sizeof(func_decl));
	sz_of_fdecl->ret_type.type = enum_type2::TYPE_INT;
	tp.fdecl = sz_of_fdecl;
	tp.fdecl->flags |= FUNC_DECL_INTERNAL;
	tp.fdecl->args.push_back(dummy_decl);

	prs_globals.root->vars.push_back(NewDecl("sizeof", tp));

	prs_globals.root->vars.push_back(NewDecl("enum_count", tp));

	tp.type = enum_type2::TYPE_VOID;
	prs_globals.void_decl = NewDecl("void", tp);

	tp.type = enum_type2::TYPE_U64;
	prs_globals.i64_decl = NewDecl("i64", tp);

	tp.type = enum_type2::TYPE_U64;
	prs_globals.u64_decl = NewDecl("u64", tp);

	tp.type = enum_type2::TYPE_S64;
	prs_globals.s64_decl = NewDecl("s64", tp);

	tp.type = enum_type2::TYPE_U32;
	prs_globals.u32_decl = NewDecl("u32", tp);

	tp.type = enum_type2::TYPE_S32;
	prs_globals.s32_decl = NewDecl("s32", tp);

	tp.type = enum_type2::TYPE_U16;
	prs_globals.s16_decl = NewDecl("u16", tp);

	tp.type = enum_type2::TYPE_S16;
	prs_globals.u16_decl = NewDecl("s16", tp);

	tp.type = enum_type2::TYPE_U8;
	prs_globals.u8_decl = NewDecl("u8", tp);

	tp.type = enum_type2::TYPE_S8;
	prs_globals.s8_decl = NewDecl("s8", tp);

	tp.type = enum_type2::TYPE_BOOL;
	prs_globals.bool_decl = NewDecl("bool", tp);

	tp.type = enum_type2::TYPE_F32;
	prs_globals.f32_decl = NewDecl("f32", tp);

	tp.type = enum_type2::TYPE_F64;
	prs_globals.f64_decl = NewDecl("f64", tp);

	tp.type = enum_type2::TYPE_CHAR;
	prs_globals.char_decl = NewDecl("char", tp);

	// inserting builtin types
	{
		NewTypeToSection("s64", TYPE_S64);
		NewTypeToSection("s32", TYPE_S32);
		NewTypeToSection("s16", TYPE_S16);
		NewTypeToSection("s8", TYPE_S8);
		NewTypeToSection("u64", TYPE_U64);
		NewTypeToSection("u32", TYPE_U32);
		NewTypeToSection("u16", TYPE_U16);
		NewTypeToSection("u8", TYPE_U8);
		NewTypeToSection("bool", TYPE_BOOL);
		NewTypeToSection("void", TYPE_VOID);
		NewTypeToSection("str_lit", TYPE_STR_LIT);
	}
	
	// getting the compiler exe path 
	TCHAR buffer[MAX_PATH] = { 0 };
	GetModuleFileName(NULL, buffer, MAX_PATH);

	// addind the file name to the path
	std::string exe_dir = buffer;
	std::string file_name_dir = std::string(argv[1]);
	int last_bar_comp = exe_dir.find_last_of('\\');
	int last_bar_main = file_name_dir.find_last_of('/');
	std::string dir = exe_dir.substr(0, last_bar_comp) + "\\" + file_name_dir.substr(0, last_bar_main);
	prs_globals.work_dir = dir;

	std::string file_name = file_name_dir.substr(last_bar_main+1);

	AddNewFile("Core/std.lng");
	AddNewFile("tests.lng");
	AddNewFile(file_name);
	

	int cur_f = 0;

	while(true)
	{
		prs_globals.something_was_declared = false;
		
		for(cur_f = 0; cur_f < prs_globals.files.size(); cur_f++)
		{
			auto f = prs_globals.files[cur_f];
			prs_globals.cur_file = f;
			DescendNameFinding(f->s, f->global);
			int a = 0;
		}
		if(!prs_globals.something_was_declared)
			break;
	}
	prs_globals.flags = PSR_FLAGS_REPORT_UNDECLARED_IDENTS;

	for(cur_f = 0; cur_f < prs_globals.files.size(); cur_f++)
	{
		auto f = prs_globals.files[cur_f];
		prs_globals.cur_file = f;
		DescendNameFinding(f->s, f->global);
	}
	//DescendIndefinedIdents(s, &global);
	
	if(IS_FLAG_ON(prs_globals.flags, PSR_FLAGS_ERRO_REPORTED))
		ExitProcess(1);

	srand(time(0));
	prs_globals.flags |= PSR_FLAGS_ASSIGN_SAVED_REGS;
	prs_globals.flags |= PSR_FLAGS_AFTER_TYPE_CHECK;
	//unsigned long long match_type;
	
	for(cur_f = 0; cur_f < prs_globals.files.size(); cur_f++)
	{
		auto f = prs_globals.files[cur_f];
		prs_globals.cur_file = f;
		prs_globals.lhs_saved = 0;
		prs_globals.call_regs_used = 0;
		DescendNode(f->s, f->global);
		/*
		TypeCheckTree(f->s, f->global, []()
		{
			while (true)
			{
				auto msg = works.GetMessageA();
				if (!msg) continue;
				if (msg->type == msg_type::MSG_DONE) break;
			}
		});
		*/
	}

	prs_globals.call_regs_used = 0;
	std::vector<func_byte_code*>all_funcs = GetFuncs(prs_globals.funcs_scp);
	auto code = FromByteCodeToX64(&all_funcs);

	ResolveJmpInsts(&code);
	
	// resolving calls
	FOR_VEC(c, code.call_rels)
	{
		int *to_fill = (int *)&code.code.data()[c->call_idx];
		long long end_call_inst = (long long) ((char *)to_fill) + 4;

		long long dst = (long long)&code.code.data()[c->fdecl->code_start_idx];
		*to_fill  = dst - end_call_inst;
	}
	std::vector<char> dbg_asm;
	/*
	std::vector<func_byte_code*> all_funcs;
	for(cur_f = 0; cur_f < prs_globals.files.size(); cur_f++)
	{
		auto f = prs_globals.files[cur_f];
		prs_globals.cur_file = f;
		prs_globals.call_regs_used = 0;
		f->bc_funcs = GetFuncs(f->global);
		INSERT_VEC(all_funcs, f->bc_funcs);
	}

	auto code = FromByteCodeToX64(&all_funcs);
	ResolveJmpInsts(&code);
	
	// resolving calls
	FOR_VEC(c, code.call_rels)
	{
		int *to_fill = (int *)&code.code.data()[c->call_idx];
		long long end_call_inst = (long long) ((char *)to_fill) + 4;

		long long dst = (long long)&code.code.data()[c->fdecl->code_start_idx];
		*to_fill  = dst - end_call_inst;
	}
	*/

	//std::vector<char> dbg_asm;

	/*
	FOR_VEC(f_ptr, all_funcs)
	{

		auto f = *f_ptr;
		auto func_asm = AsmDebugStr(code.code.data(), &f->bcodes);
		dbg_asm.insert(dbg_asm.end(), func_asm.begin(), func_asm.end());
	}
	*/
	
	WriteFile("asm_dbg.txt", dbg_asm.data(), dbg_asm.size());


	code.symbols.insert(code.symbols.end(), prs_globals.type_sect_syms.begin(), prs_globals.type_sect_syms.end());

	auto data = code.code.data();

	std::string fname = argv[1];

	auto found_last_bar = fname.find_last_of("/");
	auto found_first_point = fname.find_last_of(".");

	if (found_last_bar == -1)
		found_last_bar = 0;
	// skipping the bar
	else
		found_last_bar++;

	std::string obj_name = std::string(fname, found_last_bar, found_first_point - (found_last_bar)) + ".obj";
	std::string obj_dir = (std::string("game/") + obj_name);
	GenerateObj((char *)obj_dir.c_str(), &code);
	CallLinker("main", (char *)obj_dir.c_str());

	/*
	interpreter interp;
	interp.SetBCode(&all_funcs);
	interp.Init();
	*/
	


	// excuting test functions
	//
	//

	char msg_hdr[256];
	auto exec_funcs = (char*)VirtualAlloc(0, code.code.size(), MEM_COMMIT, PAGE_EXECUTE_READWRITE);
	memcpy(exec_funcs, code.code.data(), code.code.size());

	
	// resolving data rels
	FOR_VEC(c, code.rels)
	{
		if (c->type != DATA)
			continue;
		int *to_fill = (int *)&exec_funcs[c->idx];
		long long end_call_inst = (long long) ((char *)to_fill) + 4;

		bool found = false;
		unsigned int sym_idx = 0;
		FOR_VEC(sym, code.symbols)
		{
			if(c->name == sym->name)
			{
				sym_idx = sym->idx;
				found = true;
			}
		}
		ASSERT(found);

		long long dst = (long long)&prs_globals.data_sect[sym_idx];
		*to_fill  = dst - end_call_inst;
	}
	

	FOR_VEC(f_ptr, all_funcs)
	{
		auto f = *f_ptr;
		if (f->is_test)
		{
			int(*func)(char*) = (int(*)(char*)) & exec_funcs[f->start_idx];
			char ret_success = 0;
			int reached = func(&ret_success);

			prs_globals.cur_file = f->fdecl->from_file;

			if (ret_success == 0)
			{
				token2* t = f->fdecl->func_node->t;
				REPORT_ERROR(t->line, t->line_offset, VAR_ARGS("test failed! reached %d\n", reached))
			}
		}
	}


 	return 0;
}
