(* Lazarus+FPC 2.2.4+3.2.2 on Linux Lazarus+FPC 2.2.4+3.2.2 on Linux Lazarus+FP *)

unit Unit1;

(* This is an absolutely minimal program to test what happens when DB-aware     *)
(* controls find their network connection interrupted for an indeterminate      *)
(* length of time.                                              MarkMLl         *)

(* cf https://forum.lazarus.freepascal.org/index.php/topic,63982.msg485782.html *)
(*                                                                              *)
(* This assumes that the PostgreSQL server, database, user ID and Password are  *)
(* specified interactively, and that a test table has been created and given an *)
(* initial couple of records like

open2300=> create table testdb (data1 integer, data2 varchar(16), primary key (data1));
CREATE TABLE
open2300=> insert into testdb select 1 as data1, 'One' as data2;
INSERT 0 1
open2300=> insert into testdb select 2 as data1, 'Two' as data2;
INSERT 0 1
open2300=> select * from testdb;
 data1 | data2
-------+-------
     1 | One
     2 | Two
(2 rows)
                                                                                *)
(* Written with reference to the examples and multipart tutorial introduced at  *)
(* https://wiki.freepascal.org/Lazarus_Database_Overview                        *)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PQConnection, SQLDB, DB, Forms, Controls, Graphics,
  Dialogs, DBGrids, DBCtrls, StdCtrls, ExtCtrls, PQTEventMonitor, PQEventMonitor;

type

  { TForm1 }

  TForm1 = class(TForm)
    Bevel1: TBevel;
    ButtonConnect: TButton;
    ButtonOperate: TButton;
    CheckGroupStatus: TCheckGroup;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    LabelIsolation: TLabel;
    PQTEventMonitor1: TPQTEventMonitor;
    Timer1: TTimer;
    LabeledEditTable: TLabeledEdit;
    LabeledEditServer: TLabeledEdit;
    LabeledEditDatabase: TLabeledEdit;
    LabeledEditUser: TLabeledEdit;
    LabeledEditPassword: TLabeledEdit;
    ListBoxIsolation: TListBox;
    ListBoxOperation: TListBox;
    PQConnection1: TPQConnection;
    SQLQuery1: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    procedure ButtonConnectClick(Sender: TObject);
    procedure ButtonOperateClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure LabeledEditPasswordKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure LabeledEditUserKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure PQTEventMonitor1EventAlert(Sender: TObject; EventName: string;
      EventCount: longint; var CancelAlerts: boolean);
    procedure Timer1Timer(Sender: TObject);
    procedure OperationListPopulate(Sender: TObject);
    procedure StatusListPopulate(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

uses
  StrUtils, DBHealth;

{$R *.lfm}

{ TForm1 }

procedure TForm1.ButtonConnectClick(Sender: TObject);

const
  commitOnDisconnect= false;

var
  scratch: string;

begin
  if ButtonConnect.Caption = 'Connect' then begin

(* Set up the database name etc. and then connect.                              *)

    PQConnection1.HostName := LabeledEditServer.Text;
    PQConnection1.DatabaseName := LabeledEditDatabase.Text;
    SQLQuery1.SQL.Clear;
    SQLQuery1.SQL.Append('select * from ' + LabeledEditTable.Text);
    PQTEventMonitor1.Events.Clear;
    PQTEventMonitor1.Events.Append(LabeledEditTable.Text);
    PQConnection1.UserName := LabeledEditUser.Text;
    PQConnection1.Password := LabeledEditPassword.Text;

(* There's no direct support in the Lazarus/LCL standard components-            *)
(* TSQLTransaction, TSQLQuery etc.- for setting transaction isolation. See      *)
(* https://forum.lazarus.freepascal.org/index.php?topic=14756.0 for discussion  *)
(* of this applied to Firebird, but it doesn't appear to be portable. Also      *)
(* https://lists.lazarus-ide.org/pipermail/lazarus/2010-December/188178.html    *)
(*                                                                              *)
(* From the POV of PostgreSQL, https://github.com/lib/pq/issues/261 and         *)
(* https://www.postgresql.org/docs/current/runtime-config-client.html are       *)
(* relevant. Also https://www.postgresql.org/docs/12/libpq-connect.html which   *)
(* suggests that it shouldn't go in the connection object so I'm guessing...    *)

    SQLTransaction1.Params.Clear;
    scratch := '';
    if ListBoxIsolation.ItemIndex >= 0 then
      scratch := '''' +  Trim(ListBoxIsolation.Items[ListBoxIsolation.ItemIndex]) + '''';
    if scratch <> '' then
      SQLTransaction1.Params.Append('default_transaction_isolation=' + scratch);
    SQLQuery1.Open;                     (* Remaining components auto-activated  *)
    PQTEventMonitor1.Registered := true;        (* ...except for this one.      *)

// Hence if running psql to a server 172.27.172.128
//
// markMLl=> SELECT * FROM pg_stat_activity where application_name = 'psql';
//  datid | datname |  pid  | usesysid | usename | application_name | client_addr  | client_hostname | client_port |         backend_start         |          xact_start           |          query_start          |         state_change          | wait_event_type | wait_event | state  | backend_xid | backend_xmin |                              query
// -------+---------+-------+----------+---------+------------------+--------------+-----------------+-------------+-------------------------------+-------------------------------+-------------------------------+-------------------------------+-----------------+------------+--------+-------------+--------------+-----------------------------------------------------------------
//  32610 | markMLl | 20069 |    16388 | markMLl | psql             | 192.168.0.66 |                 |       55180 | 2023-08-05 15:43:20.768477+00 | 2023-08-05 15:53:40.998062+00 | 2023-08-05 15:53:40.998062+00 | 2023-08-05 15:53:40.998067+00 |                 |            | active |             |      5396833 | SELECT * FROM pg_stat_activity where application_name = 'psql';
// (1 row)
//
// $ telnet 172.27.172.128 113
// Trying 172.27.172.128...
// Connected to 172.27.172.128.
// Escape character is '^]'.
// 5432,55180
// 5432 , 55180 : USERID : UNIX :postgres

(* Clear the displayed password as soon as it is no longer needed; note the     *)
(* attempt at overwriting it in situ so as not to leave a freed copy on the     *)
(* heap. Fudge a few more bits of the GUI.                                      *)

    LabeledEditPassword.Text := PadRight('', Length(LabeledEditPassword.Text));
    LabeledEditPassword.Text := '';
    StatusListPopulate(self);
    OperationListPopulate(self);
    ListBoxOperation.Enabled := true;
    ButtonOperate.Enabled := true;
    LabeledEditServer.Enabled := false;
    LabeledEditDatabase.Enabled := false;
    ListBoxIsolation.Enabled := false;
    LabelIsolation.Enabled := false;
    LabeledEditTable.Enabled := false;
    LabeledEditUser.Enabled := false;
    LabeledEditPassword.Enabled := false;
    ButtonConnect.Caption := 'Disconnect'
  end else begin
    SQLQuery1.ApplyUpdates;
    if commitOnDisconnect then
      SQLTransaction1.Commit
    else
      SQLTransaction1.Rollback;
    PQConnection1.Connected := false;   (* Remaining components deactivated automatically *)
    PQTEventMonitor1.Registered := false; (* ...except for this one             *)
    PQConnection1.Password := PadRight('', Length(PQConnection1.Password));
    PQConnection1.Password := '';
    LabeledEditPassword.Enabled := true;
    LabeledEditUser.Enabled := true;
    LabeledEditTable.Enabled := true;
    LabelIsolation.Enabled := true;
    ListBoxIsolation.Enabled := true;
    LabeledEditDatabase.Enabled := true;
    LabeledEditServer.Enabled := true;
    ButtonOperate.Enabled := false;
    ListBoxOperation.Enabled := false;
    ButtonConnect.Caption := 'Connect'
  end
end { TForm1.ButtonConnectClick } ;


procedure TForm1.OperationListPopulate(Sender: TObject);

begin
  ListBoxOperation.Items.Clear;         (* Keep in step with "ItemIndex" below  *)
  ListBoxOperation.Items.Append('Update-commit');
  ListBoxOperation.Items.Append('Update-rollback');
  ListBoxOperation.Items.Append('Reset notifications');
  ListBoxOperation.Items.Append('Glitch connection');

(* Add more operations here.                                                    *)

  ListBoxOperation.ItemIndex := 0;
  Application.ProcessMessages
end { TForm1.OperationListPopulate } ;


procedure TForm1.ButtonOperateClick(Sender: TObject);

begin
  case ListBoxOperation.ItemIndex of    (* Keep in step with "Items" above      *)
    0: begin
         SQLQuery1.ApplyUpdates;
         SQLTransaction1.CommitRetaining;
         SQLQuery1.Refresh              (* Ensure sort order is correct         *)
       end;
    1: begin
         SQLQuery1.ApplyUpdates;
         SQLTransaction1.RollbackRetaining;
         SQLQuery1.Refresh              (* Undo changes to grid                 *)
       end;
    2: PQTEventMonitor1.Tag := 0;
    3: begin
         PQConnection1.Connected := false;
         Sleep(100);
         PQConnection1.Connected := true
       end;

(* Add more operations here.                                                    *)

  otherwise
  end
end { TForm1.ButtonOperateClick } ;


procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);

begin
  if ButtonOperate.Enabled then          (* GUI thinks connection is active      *)
    ButtonConnect.Click;
  Application.ProcessMessages
end { TForm1.FormClose } ;


procedure TForm1.LabeledEditPasswordKeyUp(Sender: TObject; var Key: Word;
                                                Shift: TShiftState);
begin
  if (key = $0d) and (ButtonConnect.Caption = 'Connect') then
    ButtonConnect.Click
end { TForm1.LabeledEditPasswordKeyUp } ;


procedure TForm1.LabeledEditUserKeyUp(Sender: TObject; var Key: Word;
                                                Shift: TShiftState);
begin
  if (key = $0d) and (ButtonConnect.Caption = 'Connect') then
    ActiveControl := LabeledEditPassword
end { TForm1.LabeledEditUserKeyUp } ;


procedure TForm1.PQTEventMonitor1EventAlert(Sender: TObject; EventName: string;
                                EventCount: longint; var CancelAlerts: boolean);

begin
  PQTEventMonitor1.Tag := PQTEventMonitor1.Tag + 1
end { TForm1.PQTEventMonitor1EventAlert } ;


procedure TForm1.StatusListPopulate(Sender: TObject);

begin
  CheckGroupStatus.Items.Clear;         (* Keep in step with "Checked[]" below  *)
  CheckGroupStatus.Items.Append('Connection');
  CheckGroupStatus.CheckEnabled[0] := false;    (* Can't be changed manually    *)
  CheckGroupStatus.Items.Append('Healthy');
  CheckGroupStatus.CheckEnabled[1] := false;
  CheckGroupStatus.Items.Append('Transaction');
  CheckGroupStatus.CheckEnabled[2] := false;
  CheckGroupStatus.Items.Append('Query');
  CheckGroupStatus.CheckEnabled[3] := false;
  CheckGroupStatus.Items.Append('Datasource');
  CheckGroupStatus.CheckEnabled[4] := false;
  CheckGroupStatus.Items.Append('Notifications');
  CheckGroupStatus.CheckEnabled[5] := false;
  CheckGroupStatus.Items.Append('Notified');
  CheckGroupStatus.CheckEnabled[6] := false
end { TForm1.StatusListPopulate } ;


procedure TForm1.Timer1Timer(Sender: TObject);

const
  pollServer: longint= 0;               (* Static variable                      *)
  howOften= 255;                        (* 50 ticks/sec hence roughly every 5 secs *)

// TODO : Stretch brief changes of state.
// The ideal would be if this always detected any change of state while a
// transaction was being committed or rolledback. Make sure that any manual
// glitches are longer than the timer period (typically 20 mSec).

begin
  Timer1.Enabled := false;
  try
    if CheckGroupStatus.Items.Count = 0 then begin
      StatusListPopulate(self);         (* Keep in step with "Items" above      *)
      pollServer := 0
    end;
    CheckGroupStatus.Checked[0] := PQConnection1.Connected;
    if pollServer = 0 then
      CheckGroupStatus.Checked[1] := DBHealthy(PQConnection1);
    pollServer := (pollServer + 1) mod howOften;
    CheckGroupStatus.Checked[2] := SQLTransaction1.Active;
    CheckGroupStatus.Checked[3] := SQLQuery1.Active;
    CheckGroupStatus.Checked[4] := DataSource1.Enabled;
    CheckGroupStatus.Checked[5] := PQTEventMonitor1.Registered;
    CheckGroupStatus.Checked[6] := PQTEventMonitor1.Tag > 0;

(* As an aid to debugging, display the last-known client-side port that was     *)
(* connected to the server's (e.g.) 5432 as a hint. This should match e.g.      *)
(*                                                                              *)
(* markMLl=> SELECT * FROM pg_stat_activity WHERE datname = 'open2300';         *)

    CheckGroupStatus.Hint := IntToStr(LocalPort);
    CheckGroupStatus.ShowHint := true
  finally
    Timer1.Enabled := true
  end
end { TForm1.IdleTimer1Timer } ;


end.

