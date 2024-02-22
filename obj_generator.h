#pragma once
#include "bytecode.h"
#include "FileIO.cpp"
// Section contains executable code
#define SCN_CNT_CODE               0x20
#define SCN_CNT_INITIALIZED_DATA   0x40
#define SCN_CNT_UNINITIALIZED_DATA 0x80
#define SCN_ALIGN_8BYTES           0x00400000
#define SCN_ALIGN_16BYTES          0x00500000
#define SCN_MEM_READ               0x40000000
#define SCN_MEM_WRITE              0x80000000 
#define SCN_MEM_EXECUTE 0x20000000

#define SYM_CLASS_FUNCTION 101 
#define SYM_CLASS_EXTERNAL 2
#define SYM_CLASS_STATIC   3

#define SYM_DTYPE_FUNCTION 2

#define TEXT_SECT_RAW_DATA         1024
#define DATA_SECT_RAW_DATA         1024
#define RELOCS_TEXT_SECT_RAW_DATA  1500

#define SYMBOL_TABLE_OFFSET 2048 

#define BUFFER_SIZE (1024 * 4)
struct sect_table_struct;

struct coff
{
	void* text_sect_data;
	int text_sect_size;
};

struct obj_file
{
	char* start;
	sect_table_struct* sections;

	char* string_table;
	int* string_table_size;

	// ptr to raw data adresses in file
	int* ptrs_to_raw_data;

	machine_code *machine;
};



#pragma pack(push, 1)
struct reloc_table
{
	int virtual_address;
	int symbol_table_index;
	short type;
};
struct sect_table_struct
{
	long long name;
	int physical_address;
	int virtual_adress;
	int size_of_raw_data;
	int ptr_to_raw_data;
	int ptr_to_relocations;
	int pointer_to_line_nums;
	short num_relocations;
	short num_of_line_nums;
	int characteristics;
};
struct symbol_table
{
	long long name;
	int value;
	short section_number;
	short type;
	char storage_class;
	char number_of_aux_symbols;
};
struct coff_header
{
	unsigned short machine;
	unsigned short number_of_sections;
	unsigned int   time_date_stamp;
	unsigned int   ptr_to_symbol_table;
	unsigned int   number_of_symbols;
	unsigned short size_of_optional_header;
	unsigned short characteristics;
};
#pragma pack(pop)

short make_symbol_type(short t1, short t2)
{
	return t1 | (t2 << 4);
}

void CreateTextSection(obj_file *obj, machine_code *code, int offset, int relocs_offset)
{
	sect_table_struct* text_section = obj->sections;
	text_section->name = 0x6161;
	text_section->characteristics = SCN_CNT_CODE | SCN_MEM_EXECUTE | SCN_ALIGN_16BYTES | SCN_MEM_WRITE | SCN_MEM_READ;

	memcpy((void*)&obj->start[offset], code->code.data(), code->code.size());

	text_section->size_of_raw_data = code->code.size();
	text_section->ptr_to_raw_data = offset;
	text_section->ptr_to_relocations = relocs_offset;
}

void CreateTypeSection(obj_file *obj, machine_code *machine, int data_offset)
{
	sect_table_struct* data_section = obj->sections + 2;
	memcpy(&data_section->name, ".type", strlen("type"));
	
	int type_sect_sz = prs_globals.type_sect.size();
	int total_size = 4 + type_sect_sz + prs_globals.type_sect_str_table.size();

	int raw_data_offset = data_offset;

	int *offset_to_str_tbl = (int *)(obj->start + raw_data_offset);
	*offset_to_str_tbl = type_sect_sz;


	data_section->characteristics = SCN_MEM_READ | SCN_CNT_INITIALIZED_DATA | SCN_ALIGN_8BYTES | SCN_MEM_WRITE;
	data_section->size_of_raw_data = total_size;
	data_section->ptr_to_raw_data  = raw_data_offset;

	char* buffer = obj->start + raw_data_offset;

	memcpy(buffer, prs_globals.type_sect.data(), prs_globals.type_sect.size());
}
void CreateDataSection(obj_file *obj, machine_code *machine, int data_offset)
{
	sect_table_struct* data_section = obj->sections + 1;
	memcpy(&data_section->name, ".data", strlen("data"));


	int raw_data_offset = data_offset;
	int data_sect_size  = prs_globals.data_sect.size();

	data_section->characteristics = SCN_MEM_WRITE | SCN_MEM_READ | SCN_CNT_INITIALIZED_DATA | SCN_ALIGN_8BYTES;
	data_section->size_of_raw_data = data_sect_size;
	data_section->ptr_to_raw_data  = raw_data_offset;
	memcpy(obj->start + raw_data_offset, prs_globals.data_sect.data(), data_sect_size);
}


void CheckRelocNameWithSymbolFuncAndAddIt(obj_file *obj, int sym_idx, char* sym_name)
{
	
	sect_table_struct* sect = obj->sections;
	char* ptr_to_reloc = obj->start + sect->ptr_to_relocations + (sect->num_relocations * sizeof(reloc_table));
	reloc_table* reloc = (reloc_table*)ptr_to_reloc;

	FOR_VEC(mrel, obj->machine->rels)
	{
		if (strcmp(sym_name, mrel->name) == 0)
		{
			reloc->symbol_table_index = sym_idx;
			reloc->virtual_address = mrel->idx;
			reloc->type = IMAGE_REL_AMD64_REL32;

			sect->num_relocations++;
			reloc++;
		}
	}
}


void AssignFuncToSymbolTable(obj_file *obj, symbol_table* symb, int symbol_index, machine_sym* pre_s)
{
	char* f_name = pre_s->name;


	short f_name_len = strlen(f_name);
	char* dll_func_str;
	int dll_func_str_len = 0;

	if (pre_s->type == machine_sym_type::SYM_DLL_FUNC)
	{
		dll_func_str = "__imp_";
		dll_func_str_len = 6;
		// we're taking in account "__imp"
		f_name_len += 6;
	}

	CheckRelocNameWithSymbolFuncAndAddIt(obj, symbol_index, f_name);

	if (f_name_len < 8)
	{
		//memcpy(&symb->name, dll_func_str, 5);
		memcpy(((char*)&symb->name) + dll_func_str_len, pre_s->name, f_name_len);
	}
	else
	{
		// putting name on the string table
		char* str_tbl_end = obj->string_table + *obj->string_table_size;

		if (pre_s->type == machine_sym_type::SYM_DLL_FUNC)
		{
			memcpy(str_tbl_end, dll_func_str, 6);
			f_name_len -= 6;
		}
		memcpy(str_tbl_end + dll_func_str_len, pre_s->name, f_name_len);
		long long name_offset = ((long long)(*obj->string_table_size)) << 32;
		symb->name |= name_offset;
		*obj->string_table_size += f_name_len + 1 + dll_func_str_len;
	}
	
	switch(pre_s->type)
	{
	case machine_sym_type::SYM_LIB_FUNC:
	{
		symb->type = make_symbol_type(0, IMAGE_SYM_DTYPE_FUNCTION);
		symb->storage_class = IMAGE_SYM_CLASS_EXTERNAL;
	}break;
	case machine_sym_type::SYM_DLL_FUNC_NO_IMP_ADDING:
	case machine_sym_type::SYM_DLL_FUNC:
	{
		symb->section_number = 0;
		symb->storage_class = IMAGE_SYM_CLASS_EXTERNAL;
	}break;
	case machine_sym_type::SYM_FUNC:
	{
		symb->section_number = 1;
		symb->value = pre_s->idx;
		symb->type = make_symbol_type(0, IMAGE_SYM_DTYPE_FUNCTION);
		symb->storage_class = IMAGE_SYM_CLASS_EXTERNAL;
	}break;
	case machine_sym_type::SYM_DATA:
	{
		symb->section_number = 2;
		symb->value = pre_s->idx;
		symb->storage_class = SYM_CLASS_STATIC;
	}break;
	case machine_sym_type::SYM_TYPE_DATA:
	{
		symb->section_number = 3;
		symb->value = pre_s->idx;
		symb->storage_class = SYM_CLASS_STATIC;
	}break;
	}
}
void CreateSymbolTable(obj_file *obj, machine_code *code, int symbol_tbl_offset)
{
	symbol_table* symb = (symbol_table*)&obj->start[symbol_tbl_offset];
	//pre_symbol_struct* pre_s = (pre_symbol_struct*)c.symbols.start;
	int i = 0;
	FOR_VEC (s, code->symbols)
	{
		switch (s->type)
		{
		case machine_sym_type::SYM_LIB_FUNC:
		case machine_sym_type::SYM_DLL_FUNC_NO_IMP_ADDING:
		case machine_sym_type::SYM_DLL_FUNC:
		case machine_sym_type::SYM_FUNC:
		{
			AssignFuncToSymbolTable(obj, symb, i, &*s);
		}break;
		case machine_sym_type::SYM_TYPE_DATA:
		case machine_sym_type::SYM_DATA:
		{
			AssignFuncToSymbolTable(obj,symb, i, &*s);
		}break;
		default:
			ASSERT(false)
		}
		i++;
		symb++;
	}

	//symb->name = 0x66;

}
void CallLinker(char* entry, char* obj_name)
{
	STARTUPINFO si;
	PROCESS_INFORMATION pi;

	ZeroMemory(&si, sizeof(si));
	si.cb = sizeof(si);
	ZeroMemory(&pi, sizeof(pi));

	char cmd_line [512];
	snprintf(cmd_line, 512, "link %s /SUBSYSTEM:WINDOWS %s  \
		/ENTRY:%s", obj_name, prs_globals.linker_options.c_str(), entry);

	//	system(cmd_line);
	if (!CreateProcess(NULL,   // No module name (use command line)
		cmd_line,        // Command line
		NULL,           // Process handle not inheritable
		NULL,           // Thread handle not inheritable
		FALSE,          // Set handle inheritance to FALSE
		0,              // No creation flags
		NULL,           // Use parent's environment block
		NULL,           // Use parent's starting directory 
		&si,            // Pointer to STARTUPINFO structure
		&pi)           // Pointer to PROCESS_INFORMATION structure
		)
	{
		printf("CreateProcess failed (%d).\n", GetLastError());
		return;
	}

	
	// Wait until child process exits.
	WaitForSingleObject(pi.hProcess, INFINITE);

	// Close process and thread handles. 
	CloseHandle(pi.hProcess);
	CloseHandle(pi.hThread);
}

void GenerateObj(char* name, machine_code *machine)
{
	int total_type_sect_size = prs_globals.type_sect.size();

	int size = sizeof(coff_header) + sizeof(sect_table_struct) * 3;
	int text_sect_raw_data = size;

	size += machine->code.size();

	int text_sect_relocs_offset = size;

	size += machine->rels.size() * sizeof(reloc_table);

	int data_sect_offset = size;

	size += prs_globals.data_sect.size();

	int type_sect_offset = size;

	size += total_type_sect_size;
	
	int symbol_table_offset = size;

	size += machine->symbols.size() * sizeof(symbol_table);

	size += machine->rels.size() * 32 + 0x1000;
	
	char* buffer = (char*)malloc(size);

	memset(buffer, 0, size);

	coff_header* c_hdr = (coff_header*)&buffer[0];
	c_hdr->machine = 0x8664;
	c_hdr->number_of_sections = 3;
	c_hdr->time_date_stamp = 0x615343;
	c_hdr->ptr_to_symbol_table = symbol_table_offset;
	c_hdr->number_of_symbols = machine->symbols.size();



	obj_file f_obj;
	f_obj.machine = machine;
	f_obj.start = buffer;
	f_obj.sections = (sect_table_struct*)&buffer[sizeof(coff_header)];
	f_obj.ptrs_to_raw_data = (int*)&buffer[text_sect_raw_data];

	int symb_tbl_size = machine->symbols.size() * sizeof(symbol_table);
	f_obj.string_table = (char*)&buffer[symbol_table_offset + symb_tbl_size];
	f_obj.string_table_size = (int*)f_obj.string_table;
	*f_obj.string_table_size = 4;

	CreateTextSection(&f_obj, machine, text_sect_raw_data, text_sect_relocs_offset);
	CreateDataSection(&f_obj, machine, data_sect_offset);
	CreateTypeSection(&f_obj, machine, type_sect_offset);
	CreateSymbolTable(&f_obj, machine, symbol_table_offset);
	WriteFile(name, buffer, size);
	CallLinker("entry", name);
}
