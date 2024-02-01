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
#include <caml/fail.h>
#include <caml/threads.h>
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
  (void)path;
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

static int windows_system(wchar_t *cmd, wchar_t *cwd, wchar_t *outfile, DWORD *ret)
{
    PROCESS_INFORMATION p_info;
    STARTUPINFOW s_info;
    SECURITY_ATTRIBUTES s_attrs;
    HANDLE hp, p_stderr, hf;
    DWORD handleInfo, flags, err = ERROR_SUCCESS;

    memset(&s_info, 0, sizeof(s_info));
    memset(&p_info, 0, sizeof(p_info));
    memset(&s_attrs, 0, sizeof(s_attrs));
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

    /* Redirect stdout to <outfile>, or to stderr if no <outfile> */
    if (outfile == NULL) {
        s_info.hStdOutput = s_info.hStdError;
        hf = INVALID_HANDLE_VALUE;
    } else {
        s_attrs.bInheritHandle = TRUE;
        s_attrs.nLength = sizeof(s_attrs);
        hf = CreateFileW(outfile,
            GENERIC_WRITE,
            FILE_SHARE_WRITE | FILE_SHARE_READ,
            &s_attrs,
            OPEN_ALWAYS,
            FILE_ATTRIBUTE_NORMAL,
            NULL);
        if (hf == INVALID_HANDLE_VALUE) {
            err = GetLastError(); goto ret;
        }
        s_info.hStdOutput = hf;
    }

    flags = CREATE_NO_WINDOW | CREATE_UNICODE_ENVIRONMENT;
    if (! CreateProcessW(NULL, cmd, NULL, NULL,
                         TRUE, flags, NULL, cwd, &s_info, &p_info)) {
        err = GetLastError();
    }

    /* Close the handle if we duplicated it above. */
    if (! (handleInfo & HANDLE_FLAG_INHERIT))
        CloseHandle(s_info.hStdError);

    if (err == ERROR_SUCCESS) {
        WaitForSingleObject(p_info.hProcess, INFINITE);
        GetExitCodeProcess(p_info.hProcess, ret);
        CloseHandle(p_info.hProcess);
        CloseHandle(p_info.hThread);
    }

    if (hf != INVALID_HANDLE_VALUE) {
        CloseHandle(hf);
    }
 ret:
    return err;
}

value ml_merlin_system_command(value v_command, value v_cwd, value v_opt_outfile)
{
  CAMLparam3(v_command, v_cwd, v_opt_outfile);
  DWORD ret, err;
  wchar_t *command = caml_stat_strdup_to_utf16(String_val(v_command));
  wchar_t *cwd = caml_stat_strdup_to_utf16(String_val(v_cwd));
  wchar_t *outfile = NULL;
  if (Is_some(v_opt_outfile)) {
    outfile = caml_stat_strdup_to_utf16(String_val(Some_val(v_opt_outfile)));
  }
  caml_release_runtime_system();
  err = windows_system(command, cwd, outfile, &ret);
  caml_acquire_runtime_system();
  caml_stat_free(command);
  caml_stat_free(cwd);
  if (outfile != NULL) caml_stat_free(outfile);

  if (err != ERROR_SUCCESS) {
    win32_maperr(err);
    uerror("windows_system", v_command);
  }

  CAMLreturn(Val_int(ret));
}

#else

value ml_merlin_dont_inherit_stdio(value vstatus)
{
  (void)vstatus;
  return Val_unit;
}

CAMLprim value ml_merlin_system_command(value v_command, value v_cwd, value v_opt_outfile)
{
  (void)v_command;
  (void)v_cwd;
  (void)v_opt_outfile;
  caml_invalid_argument("ml_merlin_system_command is only available on windows");
}

#endif
