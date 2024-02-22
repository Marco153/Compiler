#pragma once
#include "error_report.h"
#include "node.h"

int GetCurFileNameAndLine(char *buffer, int sz, int ln)
{
	char *fl_name = (char *)prs_globals.cur_file->name.c_str();
	return snprintf(buffer, sz, "%s(%d) ", fl_name, ln);
}
char *GetFileLn(int line, unit_file *fl)
{
	if (fl == nullptr)
	{
		return prs_globals.cur_file->lines[line];
	}
	else
		return fl->lines[line];

}

void ReportError(int line, int line_offset, char *str, int flags)
{
	char msg_hdr[256];

	std::vector<char *> *lines = &prs_globals.cur_file->lines;
	char* cur_line = (*lines)[line - 1];

	
	printf("%s%s\n", str, msg_hdr);
	
	printf("\x1b[31m");
	
	int line_idx_len = printf("%d| ", line);

	printf("%s\n", (char*)((*lines)[line - 1]));
	
	// drawing the up arrow to say which token wasnt found
	for (int i = 0; i < line_offset + line_idx_len; i++)
	{
		if (cur_line[i] == '\t')
			printf("\t");
		else
			printf(" ");
	}
	printf("^\n\n");
	printf("\x1b[0m");

	printf("%s\n", (char*)((*lines)[line + 1 - 1]));
	printf("%s\n\n", (char*)((*lines)[line + 2 - 1]));

	prs_globals.flags |= PSR_FLAGS_ERRO_REPORTED;
}

