program test_float;
uses
pqconnection,sqldb,sysutils,db;

function CreateConnection: TPQConnection;
begin
  result := TPQConnection.Create(nil);
  result.Hostname := 'db.nonodev.com';
  result.DatabaseName := 'ghd';
  result.UserName := 'bnl';
  result.Password := 'ld4BC9Q51FU9CYjC21gp';
end;

function CreateTransaction(pConnection: TPQConnection): TSQLTransaction;
begin
  result := TSQLTransaction.Create(pConnection);
  result.Database := pConnection;
end;

function CreateQuery(pTransaction: TSQLTransaction): TSQLQuery;
begin
  result := TSQLQuery.Create(pTransaction.Database);
  result.Database := pTransaction.Database;
  result.Transaction := pTransaction
end;

var
PQConn : TPQConnection;
T      : TSQLTransaction;
Q1, Q2, Q3 : TSQLQuery;

A : LongInt;
B : Double;
C : TDateTime;

sSql : String;
begin

  PQConn := CreateConnection ;
  PQConn.Open;
  T := CreateTransaction(PQConn);
  T.StartTransaction;

  Q1 := CreateQuery(T) ;
  sSql := 'create table TEST ( ';
  sSql += 'A integer not null primary key, ';
  sSql += 'B numeric(8,3) not null , ';
  sSql += 'C timestamp(3) without time zone not null ) ';

  Q1.SQL.Text := sSql;
  Q1.ExecSql;

  Q2 := CreateQuery(T) ;
  sSql := 'insert into TEST values (1, 1.0, :DT)';
  Q2.SQL.Text := sSql;
  Q2.Prepare;
  Q2.Params.ParamByName('DT').AsDateTime := now;

  Q2.ExecSql;

  Q3 := CreateQuery(T) ;
  sSql := 'select * from TEST order by A';
  Q3.SQL.Text := sSql;
  Q3.Open;
  if not Q3.Eof then
    begin
      A := Q3.FieldByName('A').AsLongint;
      B := Q3.FieldByName('B').AsFloat;
      C := Q3.FieldByName('C').AsDateTime;
      Writeln('A: ', A);
      Writeln('B: ', B);
      Writeln('C: ', C);
      Writeln('round(B): ', round(B));
      Writeln('round(C): ', round(C));
    end
  else
    writeln('Eos');

  Q3.Close;
  T.Rollback;
  Q1.Free;
  Q2.Free;
  Q3.Free;
  T.Free;
  PQConn.Close;
end.


