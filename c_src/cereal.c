#include <erl_nif.h>

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <termios.h>
#include <fcntl.h>
#include <errno.h>
#include <signal.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>

#define TtyOpen(TTY) ((TTY) != -1)

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
  return 0;
}

static int
reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
  return 0;
}

static int
upgrade(ErlNifEnv* env, void** priv, void** old_priv, ERL_NIF_TERM load_info)
{
  return 0;
}

static void
unload(ErlNifEnv* env, void* priv)
{
  return;
}

ERL_NIF_TERM
mk_atom(ErlNifEnv* env, const char* atom)
{
  ERL_NIF_TERM ret;

  if(!enif_make_existing_atom(env, atom, &ret, ERL_NIF_LATIN1))
    {
      return enif_make_atom(env, atom);
    }

  return ret;
}

ERL_NIF_TERM
mk_error(ErlNifEnv* env, const char* mesg)
{
  return enif_make_tuple(env, mk_atom(env, "error"), mk_atom(env, mesg));
}

/**********************************************************************
 * Name: set_raw_tty_mode
 *
 * Desc: Configures the given tty for raw-mode.
 */

static ERL_NIF_TERM
set_raw_tty_mode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  struct termios ttymodes;

  int fd;

  if (enif_get_int(env, argv[0], &fd) < 1)
    {
      return enif_make_badarg(env);
    }

  /* Get ttymodes */

  if (tcgetattr(fd, &ttymodes) < 0)
    {
      return mk_error(env, "tcgetattr");
    }

  /* Configure for raw mode (see man termios) */
  ttymodes.c_cc[VMIN] = 1;         /* at least one character */
  ttymodes.c_cc[VTIME] = 0;        /* do not wait to fill buffer */

  ttymodes.c_iflag &= ~(ICRNL |    /* disable CR-to-NL mapping */
			INLCR |    /* disable NL-to-CR mapping */
			IGNCR |    /* disable ignore CR */
			ISTRIP |   /* disable stripping of eighth bit */
			IXON |     /* disable output flow control */
			BRKINT |   /* disable generate SIGINT on brk */
			IGNPAR |
			PARMRK |
			IGNBRK |
			INPCK);    /* disable input parity detection */

  ttymodes.c_lflag &= ~(ICANON |   /* enable non-canonical mode */
			ECHO |     /* disable character echo */
			ECHOE |    /* disable visual erase */
			ECHOK |    /* disable echo newline after kill */
			ECHOKE |   /* disable visual kill with bs-sp-bs */
			ECHONL |   /* disable echo nl when echo off */
			ISIG | 	   /* disable tty-generated signals */
			IEXTEN);   /* disable extended input processing */

  ttymodes.c_cflag |= CS8;         /* enable eight bit chars */
  ttymodes.c_cflag &= ~PARENB;     /* disable input parity check */

  ttymodes.c_oflag &= ~OPOST;      /* disable output processing */

  /* roland */
  ttymodes.c_cflag |= CLOCAL;

  /* Apply changes */

  if (tcsetattr(fd, TCSAFLUSH, &ttymodes) < 0)
    {
      return mk_error(env, "tcsetattr");
    }

  return mk_atom(env, "ok");
}

/**********************************************************************
 * Name: set_tty_speed
 *
 * Desc: set input and output speeds of a given connection.
 */

static ERL_NIF_TERM
set_tty_speed(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int fd;
  int new_ispeed;
  int new_ospeed;
  struct termios ttymodes;

  if (enif_get_int(env, argv[0], &fd) < 1)
    {
      return enif_make_badarg(env);
    }

  if (enif_get_int(env, argv[1], &fd) < 1)
    {
      return enif_make_badarg(env);
    }

  if (enif_get_int(env, argv[2], &fd) < 1)
    {
      return enif_make_badarg(env);
    }

  /* Get ttymodes */

  if (tcgetattr(fd,&ttymodes) < 0)
    {
      return mk_error(env, "tcgetattr");
    }

  if (cfsetispeed(&ttymodes,new_ispeed) < 0)
    {
      return mk_error(env, "cfsetispeed");
    }

  if (cfsetospeed(&ttymodes,new_ospeed) < 0)
    {
      return mk_error(env, "cfsetospeed");
    }

  /* Apply changes */

  if (tcsetattr(fd, TCSAFLUSH, &ttymodes) < 0)
    {
      return mk_error(env, "tcsetattr");
    }

  return mk_atom(env, "ok");
}

/**********************************************************************
 * Name: set_tty_flow
 *
 * Desc: enable/disable hardware flow control
 */

static ERL_NIF_TERM
set_tty_flow(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  int fd;
  int enable;
  struct termios ttymodes;

  if (enif_get_int(env, argv[0], &fd) < 1)
    {
      return enif_make_badarg(env);
    }

  if (enif_get_int(env, argv[1], &enable) < 1)
    {
      return enif_make_badarg(env);
    }

  /* Get ttymodes */

  if (tcgetattr(fd,&ttymodes) < 0)
    {
      return mk_error(env, "tcgetattr");
    }

  if (enable)
    ttymodes.c_cflag |= CRTSCTS;     /* enable RTS/CTS flow control */
  else
    ttymodes.c_cflag &= ~CRTSCTS;

  /* Apply changes */

  if (tcsetattr(fd, TCSAFLUSH, &ttymodes) < 0)
    {
      return mk_error(env, "tcsetattr");
    }

  return mk_atom(env, "ok");
}

/**********************************************************************
 * Name: open_tty
 *
 * Desc: open tty and return fd
 */

static ERL_NIF_TERM
open_tty(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  char ttyname[MAXPATHLEN];
  int ttyfd = -1;

  if (enif_get_string(env, argv[0], ttyname, sizeof(ttyname), ERL_NIF_LATIN1) < 1)
    {
      return enif_make_badarg(env);
    }

  ttyfd = open(ttyname, O_RDWR|O_NONBLOCK);

  if (!TtyOpen(ttyfd))
    {
      return mk_error(env, "open");
    }

  return enif_make_int(env, ttyfd);
}

static ErlNifFunc nif_funcs[] = {
  {"open_tty", 1, open_tty},
  {"set_raw_tty_mode", 1, set_raw_tty_mode},
  {"set_tty_speed", 3, set_tty_speed},
  {"set_tty_flow", 2, set_tty_flow}
};

ERL_NIF_INIT(cereal, nif_funcs, &load, &reload, &upgrade, &unload);
