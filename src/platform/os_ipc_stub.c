#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <sys/socket.h>

static unsigned char buffer[65536];

#define NO_EINTR(var, command) \
  do { (var) = command; } while ((var) == -1 && errno == EINTR)

#define unbyte(x,n) (((unsigned char)x) << (n * 8))

static ssize_t recv_buffer(int fd, int fds[3])
{
  struct iovec iov = { .iov_base = buffer, .iov_len = sizeof(buffer) };
  struct msghdr msg = {
    .msg_iov = &iov, .msg_iovlen = 1,
    .msg_controllen = CMSG_SPACE(3 * sizeof(int)),
  };
  msg.msg_control = alloca(msg.msg_controllen);
  memset(msg.msg_control, 0, msg.msg_controllen);

  ssize_t recvd;
  NO_EINTR(recvd, recvmsg(fd, &msg, 0));
  if (recvd == -1)
  {
    perror("recvmsg");
    return -1;
  }

  if (recvd < 4)
  {
    ssize_t recvd_;
    do {
      NO_EINTR(recvd_, recv(fd, buffer + recvd, sizeof(buffer) - recvd, 0));
      if (recvd_ > 0)
        recvd += recvd_;
    } while (recvd_ > 0 && recvd < 4);
  }

  size_t target = -1;

  if (recvd > 4)
  {
    target =
      unbyte(buffer[0],0) | unbyte(buffer[1],1) |
      unbyte(buffer[2],2) | unbyte(buffer[3],3);

    if (recvd < target)
    {
      ssize_t recvd_;
      do {
        NO_EINTR(recvd_, recv(fd, buffer + recvd, sizeof(buffer) - recvd, 0));
        if (recvd_ > 0)
          recvd += recvd_;
      } while (recvd_ > 0 && recvd < target);
    }
  }

  struct cmsghdr *cm = CMSG_FIRSTHDR(&msg);

  int *fds0 = (int*)CMSG_DATA(cm);
  int nfds = (cm->cmsg_len - CMSG_LEN(0)) / sizeof(int);

  /* Check malformed packet */
  if (nfds != 3 || recvd != target || buffer[recvd-1] != '\0')
  {
    int i;
    for (i = 0; i < nfds; ++i)
      close(fds0[i]);
    return -1;
  }

  fds[0] = fds0[0];
  fds[1] = fds0[1];
  fds[2] = fds0[2];

  return recvd;
}

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>

value ml_merlin_daemon_connect(value file_descr)
{
  CAMLparam1(file_descr);
  CAMLlocal3(ret, client, argv);

  int fds[3];
  ssize_t len = recv_buffer(Int_val(file_descr), fds);

  if (len == -1)
    ret = Val_unit;
  else {
    client = caml_alloc(4, 0);
    Store_field(client, 0, Val_int(fds[0]));
    Store_field(client, 1, Val_int(fds[1]));
    Store_field(client, 2, Val_int(fds[2]));

    ssize_t i, j;
    int argc = 0;
    for (i = 4; i < len; ++i)
      if (buffer[i] == '\0')
        argc += 1;

    argv = caml_alloc(argc, 0);

    argc = 0;
    for (i = 4, j = 4; i < len; ++i)
    {
      if (buffer[i] == '\0')
      {
        Store_field(argv, argc, caml_copy_string(&buffer[j]));
        j = i + 1;
        argc += 1;
      }
    }
    Store_field(client, 3, argv);

    ret = caml_alloc(1, 0);
    Store_field(ret, 0, client);
  }

  CAMLreturn(ret);
}
