#ifdef _WIN32
#define CAML_NAME_SPACE
#define CAML_INTERNALS
#include <caml/misc.h>
#include <caml/osdeps.h>
#include <caml/unixsupport.h>
#endif

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <stdlib.h>

/* FS case */

#ifdef __APPLE__

#include <fcntl.h>
#include <sys/param.h>
#include <unistd.h>

value ml_merlin_fs_exact_case(value path)
{
	CAMLparam1(path);
	CAMLlocal1(realpath);
	char realpath_c[MAXPATHLEN];

	realpath = path;

	int fd = open(String_val(path), O_EVTONLY | O_SYMLINK);
	if (fd != -1)
	{
		if (fcntl(fd, F_GETPATH, realpath_c) != -1)
		{
			realpath = caml_copy_string(realpath_c);
		}
		close(fd);
	}
	CAMLreturn(realpath);
}


#else

value ml_merlin_fs_exact_case(value path)
{
	return path;
}

#endif

#ifdef _WIN32

value ml_merlin_fs_exact_case_basename(value path)
{
  CAMLparam1(path);
  CAMLlocal1(result);
  HANDLE h;
  wchar_t * wname;
  WIN32_FIND_DATAW fileinfo;

  wname = caml_stat_strdup_to_utf16(String_val(path));
  h = FindFirstFileW(wname, &fileinfo);
  caml_stat_free(wname);

  if (h == INVALID_HANDLE_VALUE) {
    result = Val_int(0);
  } else {
    FindClose(h);
    result = caml_alloc (1, 0);
    Store_field(result, 0, caml_copy_string_of_utf16(fileinfo.cFileName));
  }

  CAMLreturn(result);
}

#else

value ml_merlin_fs_exact_case_basename(value path)
{
  return Val_int(0);
}

#endif

#ifdef _WIN32

/* File descriptor inheritance */

#include <windows.h>
#include <io.h>

value ml_merlin_dont_inherit_stdio(value vstatus)
{
  int status = Int_val(vstatus) ? 0 : HANDLE_FLAG_INHERIT;
  SetHandleInformation((HANDLE)_get_osfhandle(1), HANDLE_FLAG_INHERIT, status);
  SetHandleInformation((HANDLE)_get_osfhandle(2), HANDLE_FLAG_INHERIT, status);
  return Val_unit;
}

/* Run ppx-command without opening a sub console */

static int windows_system(const char *cmd)
{
    PROCESS_INFORMATION p_info;
    STARTUPINFOW s_info;
    HANDLE hp, p_stderr;
    DWORD handleInfo, flags, ret, err = ERROR_SUCCESS;

    memset(&s_info, 0, sizeof(s_info));
    memset(&p_info, 0, sizeof(p_info));
    s_info.cb = sizeof(s_info);
    s_info.dwFlags = STARTF_USESTDHANDLES;

    s_info.hStdInput = INVALID_HANDLE_VALUE;

    /* If needed, duplicate stderr to make sure it is inheritable */
    p_stderr = GetStdHandle(STD_ERROR_HANDLE);
    if (p_stderr == INVALID_HANDLE_VALUE) {
        err = GetLastError(); goto ret;
    }
    if (! GetHandleInformation(p_stderr, &handleInfo)) {
        err = GetLastError(); goto ret;
    }
    if (! (handleInfo & HANDLE_FLAG_INHERIT)) {
        hp = GetCurrentProcess();
        if (! DuplicateHandle(hp, p_stderr, hp, &(s_info.hStdError),
                              0, TRUE, DUPLICATE_SAME_ACCESS)) {
            err = GetLastError(); goto ret;
        }
    } else {
        s_info.hStdError = p_stderr;
    }

    /* Redirect stdout to stderr */
    s_info.hStdOutput = s_info.hStdError;

    flags = CREATE_NO_WINDOW | CREATE_UNICODE_ENVIRONMENT;
    WCHAR *utf16cmd = caml_stat_strdup_to_utf16(cmd);
    if (! CreateProcessW(NULL, utf16cmd, NULL, NULL,
                         TRUE, flags, NULL, NULL, &s_info, &p_info)) {
        err = GetLastError();
    }
    caml_stat_free(utf16cmd);

    /* Close the handle if we duplicated it above. */
    if (! (handleInfo & HANDLE_FLAG_INHERIT))
        CloseHandle(s_info.hStdError);

    if (err == ERROR_SUCCESS) {
        WaitForSingleObject(p_info.hProcess, INFINITE);
        GetExitCodeProcess(p_info.hProcess, &ret);
        CloseHandle(p_info.hProcess);
        CloseHandle(p_info.hThread);
        return ret;
    }
 ret:
    win32_maperr(err);
    uerror("windows_system", Nothing);
}

value ml_merlin_system_command(value command)
{
  return Val_int(windows_system(String_val(command)));
}

#else

value ml_merlin_dont_inherit_stdio(value vstatus)
{
  (void)vstatus;
  return Val_unit;
}

value ml_merlin_system_command(value command)
{
  return Val_int(system(String_val(command)));
}

#endif
