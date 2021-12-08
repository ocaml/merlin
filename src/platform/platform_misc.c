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

static int windows_ppx_command(char_os *cmd, HANDLE fd_in, HANDLE fd_out)
{
    PROCESS_INFORMATION p_info;
    STARTUPINFOW s_info;
    DWORD flags, ret;

    flags = CREATE_NO_WINDOW | CREATE_UNICODE_ENVIRONMENT;
    memset(&s_info, 0, sizeof(s_info));
    memset(&p_info, 0, sizeof(p_info));
    s_info.cb = sizeof(s_info);
    s_info.dwFlags = STARTF_USESTDHANDLES;
    s_info.hStdInput = fd_in;
    s_info.hStdOutput = fd_out;
    s_info.hStdError = fd_out;

    ret = CreateProcessW(NULL, cmd, NULL, NULL, TRUE, flags, NULL, NULL, &s_info, &p_info);
    CloseHandle(fd_in);
    CloseHandle(fd_out);

    if (ret) {
        WaitForSingleObject(p_info.hProcess, INFINITE);
        GetExitCodeProcess(p_info.hProcess, &ret);
        CloseHandle(p_info.hProcess);
        CloseHandle(p_info.hThread);
        return ret;
    }
    return -1;
}

value ml_merlin_ppx_command(value vcmd, value vfd_in, value vfd_out)
{
  char_os *cmd;
  HANDLE fd_in = Handle_val(vfd_in), fd_out = Handle_val(vfd_out);
  int ret;

  cmd = caml_stat_strdup_to_os(String_val(vcmd));
  ret = windows_ppx_command(cmd, fd_in, fd_out);
  caml_stat_free(cmd);

  return Val_int(ret);
}

#else

value ml_merlin_dont_inherit_stdio(value vstatus)
{
  (void)vstatus;
  return Val_unit;
}

value ml_merlin_ppx_command(value vcmd, value vfd_in, value vfd_out)
{
  (void)vcmd;
  (void)vfd_in;
  (void)vfd_out;
  return Val_int(1);
}

#endif
