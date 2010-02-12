#include <stdio.h>
#include <stdlib.h>
#include <sqlite3.h>
#include <string.h>

#include "erl_comm.h"
#include "erl_interface.h"
#include "ei.h"

#define MASTER_QUERY "select * from sqlite_master where type='table';"

#ifndef RELEASE
static FILE *log;
#endif

static ETERM *result;

void respond(ETERM *r);
void send_error(char *err_msg);
void send_result();
void send_ok();

// 4 sql = CREATE TABLE t1 (t1key INTEGER PRIMARY KEY, data TEXT, num double, timeEnter DATE)
static int list_tables(void *notUsed, int argc, char **argv, char **azColName) 
{
  if (result == 0) {
    result = erl_mk_empty_list();
  }
  
#ifndef RELEASE
  fprintf(log, "%d %s = %s\n", 2, azColName[2], argv[2]);
  fprintf(log, "\n");
  fflush(log);
#endif

  result = erl_cons(erl_mk_atom(argv[2]), result);

  return 0;
}

static int callback(void *notUsed, int argc, char **argv, char **azColName) 
{
  ETERM **record_list;
  int i;

  if (result == 0) {
    result = erl_mk_empty_list();
  }

  record_list = malloc(argc * sizeof(ETERM *));
  
#ifndef RELEASE
  fprintf(log, "runs %d\n", argc);
#endif
  for (i = 0; i < argc; i++) {
#ifndef RELEASE
    fprintf(log, "%s = %s\n", azColName[i], argv[i] ? argv[i] : "NULL");
#endif
    if (argv[i]) {
      record_list[i] = erl_mk_string(argv[i]);
    }
    else {
      record_list[i] = erl_mk_empty_list();
    }
  }
#ifndef RELEASE
  fprintf(log, "\n");
  fflush(log);
#endif

  result = erl_cons(erl_mk_tuple(record_list, argc), result);

  free(record_list);
  return 0;
}

int main(int argc, char **argv) 
{
  ETERM *tuplep;
  ETERM *fnp, *argp;
  byte buf[1024];

  sqlite3 *db;
  char *zErrMsg = 0;
  int rc;

#ifndef RELEASE
  log = fopen("/tmp/sqlite_port.log", "a+");
  fprintf(log, "******start log (%s)******\n", argv[1]);
  fflush(log);
#endif

  rc = sqlite3_open(argv[1], &db);
  if (rc) {
    sqlite3_close(db);
    exit(1);
  }

  erl_init(NULL, 0);

  while (read_cmd(buf) > 0) {
    tuplep = erl_decode(buf);
    fnp = erl_element(1, tuplep);
    argp = erl_element(2, tuplep);
    
    if (strncmp((const char *)ERL_ATOM_PTR(fnp), "close", 5) == 0) {
#ifndef RELEASE
      fprintf(log, "closing sqlite3_close\n");
      fflush(log);
#endif

      sqlite3_close(db);
      break;
    }
    else if (strncmp((const char *)ERL_ATOM_PTR(fnp), "list_tables", 11) == 0) {
#ifndef RELEASE
      fprintf(log, "calling list_tables\n");
#endif

      result = 0;

      rc = sqlite3_exec(db, MASTER_QUERY, list_tables, 0, &zErrMsg);
      if (rc != SQLITE_OK) {
	send_error(zErrMsg);
	sqlite3_free(zErrMsg);
      }
      else if (result != 0)  {
	send_result();
      }
      else {
	// not an error and no results. still need to return something
	send_ok();
      } 

#ifndef RELEASE
      fflush(log);
#endif

    }
    else if (strncmp((const char *)ERL_ATOM_PTR(fnp), "sql_exec", 8) == 0) {
#ifndef RELEASE
      fprintf(log, "calling sqlite3_exec %s\n", erl_iolist_to_string(argp));
#endif

      result = 0;

      rc = sqlite3_exec(db, erl_iolist_to_string(argp), callback, 0, &zErrMsg);
      if (rc != SQLITE_OK) {
	send_error(zErrMsg);
	sqlite3_free(zErrMsg);
      }
      else if (result != 0)  {
	send_result();
      }
      else {
	// not an error and no results. still need to return something
	send_ok();
      } 

#ifndef RELEASE
      fflush(log);
#endif
    }

    erl_free_compound(tuplep);
    erl_free_term(fnp);
    erl_free_term(argp);
  }

#ifndef RELEASE
  fprintf(log, "******end log******\n");
  fclose(log);
#endif
  return 0;
}
      
void send_error(char *err_msg) 
{
  ETERM *tup_list[2];
  ETERM *to_send;
	
  tup_list[0] = erl_mk_atom("sql_error");
  tup_list[1] = erl_mk_string(err_msg);
  to_send = erl_mk_tuple(tup_list, 2);

#ifndef RELEASE
  fprintf(log, "SQL Error: %s\n", err_msg);
#endif
  respond(to_send);

  erl_free_term(tup_list[0]);
  erl_free_term(tup_list[1]);
  erl_free_compound(to_send);
}

void send_result() 
{
#ifndef RELEASE
  fprintf(log, "returning at len %d\n", erl_term_len(result));
#endif
  respond(result);
	
  erl_free_compound(result);
  result = 0;
}

void send_ok() 
{
  ETERM *to_send;

  to_send = erl_mk_atom("ok");
#ifndef RELEASE
  fprintf(log, "returning ok at len %d\n", erl_term_len(to_send));
#endif
  respond(to_send);
  	
  erl_free_term(to_send);
}

void respond(ETERM *r) 
{
  int len = erl_term_len(r);
  byte  *buf = (byte*) malloc(len);
  bzero(buf, len);
  erl_encode(r, buf);
  write_cmd(buf, len);

#ifndef RELEASE
  fprintf(log, "sending response back\n");
  fprintf(log, "%d bytes sent\n", len);
#endif
  free(buf);
}
