(* Lazarus+FPC 2.2.4+3.2.2 on Linux Lazarus+FPC 2.2.4+3.2.2 on Linux Lazarus+FP *)

unit DBHealth;

(* Determine the client-side port number being used by the TCP connection to    *)
(* the server, and ask the server to verify it's OK using the ident protocol    *)
(* (port 113).                                                  MarkMLl         *)

{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}           (* Needed for in4_addr.IsNull only      *)

interface

uses
  Classes, SysUtils, PQConnection;

var
  LocalPort: integer= -1;

(* Return true if the database connection passed as the parameter appears to be
  healthy.
*)
function DBHealthy(pq: TPQConnection): boolean;


implementation

uses
  PQConnection2, Sockets;

(* Note that PGConn : PPGConn; -> postgres3types.inc -> TPGconn is grossly out  *)
(* of date, but it is defined (e.g. in libpq-int.h) as being internal to libpq  *)
(* so strictly FPC shouldn't be attempting to do anything with it at all.       *)
(*                                                                              *)
(* The result of this is that the only way we can access the state we need is   *)
(* to subclass QPConnection, and access some of the shared library entry points *)
(* directly.                                                                    *)

type
  in4_addr= in_addr;                    (* Explicitly a 32-bit IP4 address      *)
  TAddressArray= array of in4_addr;

const
  in4_null: in4_addr= (S_Addr: 0);      (* Initialise one field by name,        *)

type
  Tin4_addrHelper= record helper for in4_addr
    function IsNull: boolean;
  end;

operator = (const a, b: in4_addr): boolean; inline;

begin
  result := a.s_addr = b.s_addr
end { in4_addr = } ;


function Tin4_addrHelper.IsNull: boolean; inline;

begin
  result := self = in4_null
end { Tin4_addrHelper.IsNull } ;


(* Append a word to an address array, discarding duplicates.
*)
operator + (const a: TAddressArray; const l: in4_addr): TAddressArray;

var
  i: integer;

begin
  result := a;
  for i := 0 to High(result) do
    if l = result[i] then
      exit;
  SetLength(result, Length(result) + 1);
  result[High(result)] := l
end { + } ;


(* Return an array comprising the IP addresses allocated to the current host,
  in an indeterminate but non-random order.
*)
function SelfAddressList(): TAddressArray;

// Pinched from dialarm/trunk/udp79backbone.pas. There might be something a bit
// more conventional somewhere in List3264.

// TODO : Something better than using /proc/net/fib_trie, and supporting IP6.

var
  addressTable: TStringList;
  i: integer;
  a: in4_addr;
  scratch: string;

begin
  SetLength(result, 0);
  if not FileExists('/proc/net/fib_trie') then
    exit;
  addressTable := TStringList.Create;
  try
    try
      addressTable.LoadFromFile('/proc/net/fib_trie')
    except
      exit
    end;
    for i := 0 to addressTable.Count - 1 do
      if (Pos('host LOCAL', addressTable[i]) > 0) and (i > 0) then begin
        scratch := Trim(addressTable[i - 1]);
        if Pos('-- ', scratch) > 0 then begin
          while (scratch <> '') and not (scratch[1] in ['.', '0'..'9']) do
            Delete(scratch, 1, 1);
          a := in4_addr(StrToHostAddr(scratch));
          scratch := HostAddrToStr(in4_addr(a));        (* For debugging        *)
          if a <> in4_null then
            result += a
        end
      end
  finally
    addressTable.Free;
  end
end { SelfAddressList } ;


(* Test that the indicated client port is still in use, and that the IP address
  being used by the client end of the database connection still appears in the
  interface list.

  This should detect cases where e.g. an ISP has changed the IP address of a
  domestic installation.
*)
function testClientport(clientPort: integer; clientAddr4: cardinal): boolean;

var
  addressList: TAddressArray;
  i: integer;

begin
  result := false;
// TODO : Check client port (somehow).
  addressList := SelfAddressList;
  for i := 0 to Length(addressList) - 1 do
    if addressList[i] = in4_addr(clientAddr4) then
      exit(true)
end { testClientport } ;


(* Do an ident (port 113) check against the port and IP address being used by
  the server end of the database connection.

  This should detect cases where e.g. a server daemon process has timed out
  since a laptop has been in sleep state for an extended period.
*)
function testServerPort(port, remotePort: integer; remoteHost: cardinal): boolean;

var
  sock: TSocket;
  sockaddr: sockaddr_in;
  textIn, textOut: Text;
  ident: AnsiString;

begin
  result := false;
  if remoteHost = $ffffffff then
    exit;
  sock:= fpSocket(PF_INET, SOCK_STREAM, 0);
  if sock < 0 then
    exit;
  fillChar(sockaddr{%H-}, SizeOf(TSockAddr), 0);
  sockaddr.sin_family := AF_INET;
  sockaddr.sin_port := htons(113);
  sockaddr.sin_addr.s_addr := htonl(remoteHost);

// A lookup failure might indicate a permanent problem, or that a laptop etc.
// is coming back to life after being in a sleep state. The latter of those
// could be detected by periodic pings with the result saved with its UTC
// timestamp, or by simply recovering UTC from the OS every second and looking
// for discontinuities.
//
// I've spent more time than I intended on this, so am leaving it as-is since
// overall the program is a useful proof-of-concept.

  if not Sockets.Connect(sock, sockaddr, textIn{%H-}, textOut{%H-}) then
    exit;
  Reset(textIn);
  Rewrite(textOut);
  try
    ident := IntToStr(remotePort) + ',' + IntToStr(port);
    WriteLn(textOut, ident);

// Absolutely no provision for timeout etc.

// Write(HostAddrToStr(sockaddr.sin_addr), ':', ntohs(sockaddr.sin_port), '<');
// Flush(output);
    ReadLn(textIn, ident);
// WriteLn(ident);
// Flush(output);
    result := Pos('USERID', ident) > 0
  finally
    CloseFile(textOut);
    CloseFile(textIn);
    CloseSocket(sock)
  end
end { testServerPort } ;


(* Return true if the database connection passed as the parameter appears to be
  healthy.
*)
function DBHealthy(pq: TPQConnection): boolean;

var
  clientPort, serverPort: integer;
  clientAddr4, serverAddr4: cardinal;

begin
  clientPort := TPQConnection2(pq).GetLocalPort();
  LocalPort := clientPort;              (* GUI hint For debugging               *)
  clientAddr4 := TPQConnection2(pq).GetLocalAddress4();
  serverPort := TPQConnection2(pq).GetServerPort();
  serverAddr4 := TPQConnection2(pq).GetServerAddress4();
  result := testClientPort(clientPort, clientAddr4);
  result := result and testServerPort(clientPort, serverPort, serverAddr4)
end { DBHealthy } ;


end.

