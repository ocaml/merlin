#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>

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
/* Timing functions */

#include <windows.h>

double ml_merlin_time_spent(value unit)
{
  double acc = 0.0;

  FILETIME createTime;
	FILETIME exitTime;
	FILETIME kernelTime;
	FILETIME userTime;
  if (GetProcessTimes(GetCurrentProcess(),
        &createTime, &exitTime, &kernelTime, &userTime) != -1 )
	{
    acc += (double)kernelTime.wMilliseconds + (double)userTime.wMilliseconds;
    acc += ((double)kernelTime.wSecond + (double)userTime.wSecond) * 1000.0;
	}

  return acc;
}

#else

#include <sys/resource.h>
#include <sys/times.h>

static double rusage_total_ms(struct rusage *rusage)
{
  double acc = 0.0;
  acc += ((double)rusage->ru_utime.tv_sec +
          (double)rusage->ru_stime.tv_sec) * 1000.0;
  acc += ((double)rusage->ru_utime.tv_usec +
          (double)rusage->ru_stime.tv_usec) / 1000.0;
  return acc;
}

double ml_merlin_time_spent(value unit)
{
  struct rusage usage;
  double acc = 0.0;

  (void)getrusage(RUSAGE_SELF, &usage);
  acc += rusage_total_ms(&usage);

  (void)getrusage(RUSAGE_CHILDREN, &usage);
  acc += rusage_total_ms(&usage);

  return acc;
}

#endif

value ml_merlin_time_spent_bc(value unit)
{
  return caml_copy_double(ml_merlin_time_spent(unit));
}
