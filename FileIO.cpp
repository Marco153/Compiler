#pragma once
#include <iostream>
#include "Windows.h"
//#include "Array.h"

char *ReadEntireFile(char *name, int *read)
{
	HANDLE file = CreateFile(name, GENERIC_READ, FILE_SHARE_READ, 0, OPEN_EXISTING, 0, 0);
	LARGE_INTEGER file_size;
	GetFileSizeEx(file, &file_size);
	char *f = (char *)malloc(file_size.QuadPart + 1);

	int bytes_read;
	ReadFile(file, (void *)f, file_size.QuadPart, (LPDWORD)&bytes_read, 0);
	f[file_size.QuadPart] = 0;
	
	*read = bytes_read;

	CloseHandle(file);
	return f;
}
void WriteFile(char *name, void *data, int size)
{
	HANDLE file = CreateFile(name, GENERIC_WRITE, FILE_SHARE_READ, 0, CREATE_ALWAYS, 0, 0);

	int written;
	WriteFile(file, data, size,(LPDWORD) &written, nullptr);

	CloseHandle(file);
}
