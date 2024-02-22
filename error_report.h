#pragma once

struct unit_file;
enum type_error
{
	ERR_RETURN,
	ERR_MISMATCHED_TYPES,
	ERR_MSG,
};


#define VAR_ARGS(...)__VA_ARGS__
#define REPORT_ERROR(ln, ln_offset, args)\
					int written = GetCurFileNameAndLine(msg_hdr, 256, ln);\
					snprintf(&msg_hdr[written], 256, args);\
					ReportError(ln, ln_offset, msg_hdr, 0);

char *GetFileLn(int line, unit_file * = nullptr);
int GetCurFileNameAndLine(char *buffer, int sz, int ln);
void ReportError(int, int, char *, int);
