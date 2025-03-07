#include "util.h"

wchar_t* get_working_directory() {
	wchar_t cwd[1024];
	int result = _wgetcwd(cwd, sizeof(cwd) / sizeof(wchar_t));

	if (!result) {
		wprintf("Failed to find cwd.\n");
	}

	return cwd;
}