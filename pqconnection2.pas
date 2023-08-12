(* Lazarus+FPC 2.2.4+3.2.2 on Linux Lazarus+FPC 2.2.4+3.2.2 on Linux Lazarus+FP *)

unit PQConnection2;

(* This exposes session properties of the connection object. It is placed at    *)
(* the same abstraction level as the PQConnection unit since that is the point  *)
(* at which linkage is determined to be either static or dynamic.  MarkMLl      *)

{$mode ObjFPC}{$H+}
{$Define LinkDynamically}               (* Should track setting in PQConnection *)

interface

uses
  Classes, SysUtils, Sockets, PQConnection,
{$IfDef LinkDynamically}
  postgres3dyn;
{$Else}
  postgres3;
{$EndIf}

(* Note that the PostgreSQL support units have their own definition of SockAddr *)
(* which is distinct from (and much larger than) the one defined by FPC. Any    *)
(* attempt to mix them will end in tears, and that's particularly the case      *)
(* where SizeOf() is involved.                                                  *)

type
  TSockAddr4= Sockets.TSockAddr;

function PQhost(conn: PPGconn): Pchar; cdecl;
function PQhostaddr(conn: PPGconn): Pchar; cdecl;  (* Not exposed as standard   *)
function PQport(conn: PPGconn): Pchar; cdecl;
function PQsocket(conn: PPGconn): longint; cdecl;

type
  TPQConnection2= class(TPQConnection)
  protected
    function GetLocalSockaddr4(out sa4: TSockAddr4): boolean;
    function GetServerSockaddr4(out sa4: TSockAddr4): boolean;
  public
    function GetServerHost(): string;
    function GetLocalPort(): integer;
    function GetServerPort(): integer;
    function GetLocalAddress4(): cardinal;
    function GetServerAddress4(): cardinal;
  end;


implementation

uses
  SQLDb;

{$IfDef LinkDynamically}


function PQhost(conn: PPGconn): Pchar; cdecl;

begin
  result := postgres3dyn.PQhost(conn)
end;


var
  ZPQhostaddr: function(conn: PPGconn): Pchar; cdecl;


function PQhostaddr(conn: PPGconn): Pchar; cdecl;

begin
  if not Assigned(ZPQhostaddr) then begin
    pointer(ZPQhostaddr) := GetProcedureAddress(Postgres3LibraryHandle,'PQhostaddr');
    if not Assigned(ZPQhostaddr) then
      result := nil
  end else
    result := ZPQhostaddr(conn)
end;


function PQport(conn: PPGconn): Pchar; cdecl;

begin
  result := postgres3dyn.PQport(conn)
end;


function PQsocket(conn: PPGconn): longint; cdecl;

begin
  result := postgres3dyn.PQsocket(conn)
end;


{$Else}


function PQhost(conn: PPGconn): Pchar; cdecl;

begin
  result := postgres3.PQhost(conn)
end;


function PQhostaddr(conn: PPGconn): Pchar; cdecl; external 'pq' name 'PQhostaddr';


function PQport(conn: PPGconn): Pchar; cdecl;

begin
  result := postgres3.PQport(conn)
end;


function PQsocket(conn: PPGconn): longint; cdecl;

begin
  result := postgres3.PQsocket(conn)
end;


{$EndIf}


function TPQConnection2.GetLocalSockaddr4(out sa4: TSockAddr4): boolean;

var
  sz: integer;
  handle2: pointer;

begin
  result := false;
  sz := SizeOf(TSockAddr4);
  fillChar(sa4{%H-}, sz, 0);
  if not Connected then
    exit;
  handle2 := GetHandle();
  if handle2 = nil then
    exit;
  handle2 := GetTransactionHandle(TSQLHandle(handle2));
  if handle2 = nil then
    exit;
  if fpgetsockname(PQSocket(handle2), @sa4, @sz) < 0 then
    exit;
  Assert(sa4.sin_family = AF_INET, 'No support for IPv6 session connections');
  if sa4.sin_family <> AF_INET then
    exit;
  result := true
end { TPQConnection2.GetLocalSockaddr4 } ;


function TPQConnection2.GetServerSockaddr4(out sa4: TSockAddr4): boolean;

var
  sz: integer;
  handle2: pointer;

begin
  result := false;
  sz := SizeOf(TSockAddr4);
  fillChar(sa4{%H-}, sz, 0);
  if not Connected then
    exit;
  handle2 := GetHandle();
  if handle2 = nil then
    exit;
  handle2 := GetTransactionHandle(TSQLHandle(handle2));
  if handle2 = nil then
    exit;
  sa4.sin_family := AF_INET;
  sa4.sin_port := htons(StrToInt(PQPort(handle2)));
  if PQHostAddr(handle2) = nil then
    exit;
  if PQHostAddr(handle2) = '' then
    exit;
  try
    sa4.sin_addr := StrToNetAddr(PQHostAddr(handle2))   (* Bytes net-ordered!!! *)
  except
    exit
  end;
  result := true
end { TPQConnection2.GetServerSockaddr4 } ;


function TPQConnection2.GetServerHost(): string;

var
  handle2: pointer;

begin
  result := '';
  if not Connected then
    exit;
  handle2 := GetHandle();
  if handle2 = nil then
    exit;
  handle2 := GetTransactionHandle(TSQLHandle(handle2));
  if handle2 = nil then
    exit;
  if PQHost(handle2) = nil then
    exit;
  if PQHost(handle2) = '' then
    exit;
  result := StrPas(PQHost(handle2))
end { TPQConnection2.GetServerHost } ;


function TPQConnection2.GetLocalPort(): integer;

var
  sa4: TSockAddr4;

begin
  if not GetLocalSockaddr4(sa4) then
    result := -1
  else
    result := ntohs(sa4.sin_port)
end { TPQConnection2.GetLocalPort } ;


function TPQConnection2.GetServerPort(): integer;

var
  sa4: TSockAddr4;

begin
  if not GetServerSockaddr4(sa4) then
    result := -1
  else
    result := ntohs(sa4.sin_port)
end { TPQConnection2.GetServerPort } ;


function TPQConnection2.GetLocalAddress4(): cardinal;

var
  sa4: TSockAddr4;

begin
  if not GetLocalSockaddr4(sa4) then
    result := $ffffffff
  else
    result := ntohl(cardinal(sa4.sin_addr))
end { TPQConnection2.GetLocalAddress4 } ;


function TPQConnection2.GetServerAddress4(): cardinal;

var
  sa4: TSockAddr4;

begin
  if not GetServerSockaddr4(sa4) then
    result := $ffffffff
  else
    result := ntohl(cardinal(sa4.sin_addr.s_addr))
end { TPQConnection2.GetServerAddress4 } ;


begin
{$IfDef LinkDynamically }
  Assert(TPQConnectionDef.DefaultLibraryName() <> '',
        'PQConnection2 is linked dynamically but PQConnection is linked statically');
{$Else                  }
  Assert(TPQConnectionDef.DefaultLibraryName() = '',
        'PQConnection2 is linked statically but PQConnection is linked dynamically';
{$EndIf                 }
{$if declared(ZPQhostaddr) }
  ZPQhostaddr := nil
{$endif                    }
end.

