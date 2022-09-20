// gcc sqlite3_practice.c -lsqlite3
// transcribed from
// https://qiita.com/kanegoon/items/37a9e6f3f56196c8c0bc

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>
#include <sqlite3.h>

#define LOOP_COUNTER 100000
#define DB_NAME "eval.db"
#define SQL_BEGIN "BEGIN;"
#define SQL_COMMIT "COMMIT"
#define SQL_CREATE "CREATE TABLE evaltab (no INT, data TEXT, time REAL);"
#define SQL_WAL "PRAGMA JOURNAL_MODE=WAL;"
const char *data =
  "01234567890123456789012345678901234567890123456789"
  "01234567890123456789012345678901234567890123456789";

int main () {
  sqlite3* db;
  int rc = SQLITE_OK;
  int i, j, k;
  char *zErrMsg = 0;
  char *zSQL = "INSERT INTO evaltab VALUES (?, ?, ?);";
  sqlite3_stmt *pStmt = 0;
  struct timespec tStart, tEnd;

  for(i=LOOP_COUNTER; i>0; i=(int)i/2){
    /* データベースファイルをオープン */
    rc = sqlite3_open(DB_NAME, &db);
    /* テーブルを作成 */
    rc = sqlite3_exec(db, SQL_CREATE, 0, NULL, &zErrMsg);
    /* INSERT文のパース  */
    rc = sqlite3_prepare_v2(db, zSQL, -1, &pStmt, NULL);
    k=0;
    clock_gettime(CLOCK_REALTIME, &tStart); /* 測定の開始 */
    for(j=0; j<LOOP_COUNTER; j++){
      if((j%i)==0){
	/* トランザクションの開始 */
	rc = sqlite3_exec(db, "BEGIN", 0, NULL, &zErrMsg);
      }
      /* バインド変数の処理 */
      rc = sqlite3_bind_int(pStmt, 1, j);
      rc = sqlite3_bind_text(pStmt, 2, data, -1, SQLITE_TRANSIENT);
      rc = sqlite3_bind_double(pStmt, 3, (double)time(NULL));
      /* INSERTの実行 */
      rc = sqlite3_step(pStmt);
      rc = sqlite3_reset(pStmt);
      if(((j+1)%i)==0 || LOOP_COUNTER<(j+1)){
	/* トランザクションの終了 */
	rc = sqlite3_exec(db, "COMMIT;", 0, NULL, &zErrMsg);
	k++;
      }
    }
    clock_gettime(CLOCK_REALTIME, &tEnd);   /* 測定の終了 */
    printf("%7d TRANS, %7d Records : ",k,i);
    if(tEnd.tv_nsec < tStart.tv_nsec){
      printf("%10ld.%09ld (sec)\n",tEnd.tv_sec - tStart.tv_sec - 1
	     ,tEnd.tv_nsec + 1000000000 - tStart.tv_nsec);
    }else{
      printf("%10ld.%09ld (sec)\n",tEnd.tv_sec - tStart.tv_sec
	     ,tEnd.tv_nsec - tStart.tv_nsec);
    }
    sqlite3_finalize(pStmt);  /* データベースファイルのクローズ */
    sqlite3_close(db);        /* 作成されたデータベースファイルを削除 */
    remove(DB_NAME);
  }
  return 0;
}
