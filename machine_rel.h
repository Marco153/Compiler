#pragma once
enum machine_sym_type
{
	SYM_FUNC,
	SYM_LIB_FUNC,
	SYM_DLL_FUNC,
	SYM_DLL_FUNC_NO_IMP_ADDING,
	SYM_DATA,
	SYM_TYPE_DATA,
};
enum machine_rel_type
{
	INSIDE_FUNC,
	DLL_FUNC,
	DLL_FUNC_NO_IMP_ADDING,
	DATA,
	TYPE_DATA,
};
struct machine_reloc
{
	machine_rel_type type;
	unsigned int idx;
	char *name;
	machine_reloc(machine_rel_type tp, unsigned int i, char* n)
	{
		type = tp;
		idx = i;
		name = n;
	}
};
struct machine_sym
{
	machine_sym_type type;
	unsigned int idx;
	char *name;
	
	machine_sym()
	{
	}
	machine_sym(machine_sym_type tp, unsigned int i, char* n)
	{
		type = tp;
		idx = i;
		name = n;
	}
};
