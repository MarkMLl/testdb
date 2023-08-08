unit DBHealth;

(* Determine the client-side port number being used by the TCP connection to    *)
(* the server, and ask the server to verify it's OK using the ident protocol    *)
(* (port 113).                                                  MarkMLl         *)

{$mode ObjFPC}{$H+}

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
  postgres3, Sockets, SQLDb, UnixType { , Errors }, Resolve, NetDb;


type
  TPQConnection2= class(TPQConnection)
  protected
    function GetLocalPort(): integer;
    function GetServerPort(): integer;
  end;

(* Note that PGConn : PPGConn; -> postgres3types.inc -> TPGconn is grossly out  *)
(* of date, but it is defined (e.g. in libpq-int.h) as being internal to libpq  *)
(* so strictly FPC shouldn't be attempting to do anything with it at all.       *)


(********************************************************************************)
(*                                                                              *)
(* I haven't a clue when I originally wrote this lookup stuff: probably in the  *)
(* late '90s using Delphi. But it works...                                      *)
(*                                                                              *)
(********************************************************************************)


type
  Ip4Address= cuint32;
  TIpMath= cuint32;


procedure debugMsg(const s: AnsiString);

begin
//  WriteLn(StdErr, s)
end { debugMsg } ;


FUNCTION inet_ntoa(ipAddr: In_addr): STRING;

(* Emulate the corresponding Winsock function. Implementation is      *)
(* inefficient but avoids assignment of large numbers (>= $80000000)  *)
(* to 32-bit variables.                                               *)

VAR     ip32: TIpMath;                  (* Bytes in host order        *)

BEGIN
  ip32:= NtoHL(ipAddr.S_addr);
  RESULT:= IntToStr((ip32 SHR 24) MOD 256) + '.';
  RESULT:= RESULT + IntToStr((ip32 SHR 16) MOD 256) + '.';
  RESULT:= RESULT + IntToStr((ip32 SHR 8) MOD 256) + '.';
  RESULT:= RESULT + IntToStr(ip32 MOD 256)
END { Inet_ntoa } ;


VAR     ipAddressCount: INTEGER;
        ipAddressAlternative: TIpMath;


(* Resolve a hostname or an IP4 dotted-quad address, generally trying to "do the
  right thing".
*)
function LookUpHost4(const hostname: AnsiString): Ip4Address;

(* Hide unpleasantness. Result is in network order, resolves both names *)
(* and IP addresses.                                                    *)

VAR
  resolver: THostResolver;


  FUNCTION numericResolve(CONST hostname: STRING; VAR addr: TIpMath): BOOLEAN;

  (* Convert the parameter to a network-ordered IP address if possible, *)
  (* returning TRUE. On failure return FALSE, the address is undefined  *)
  (* in this case.                                                      *)

  VAR   left, right: STRING;
        dot, temp: INTEGER;

  BEGIN
    RESULT:= FALSE;
    addr:= 0;
    right:= Trim(hostname) + '.';
    dot:= Pos('.', right);
    WHILE dot > 0 DO BEGIN
      left:= right;
      SetLength(left, dot - 1);
      Delete(right, 1, dot);
      IF (Length(left) < 1) OR (Length(left) > 3) THEN
        EXIT;
      TRY
        temp:= StrToInt(left) MOD 256
      EXCEPT
        EXIT
      END;
      addr:= (addr SHL 8) OR temp;
      dot:= Pos('.', right)
    END;
    RESULT:= right = '';
    addr:= HtoNL(addr)
  END { numericResolve } ;


BEGIN
  RESULT:= 0;
  ipAddressCount:= 0;
  ipAddressAlternative:= 0;

(* This is straight from testrhre.pp. I really do not like this idiom at  *)
(* all since it makes it impossible to see whether a failure is because   *)
(* Create() has returned NIL or something's gone wrong later.             *)

//  With THostResolver.Create(Nil) do
//    try
//      If AddressLookup(hostname) then
//        RESULT:= Addresses[0].S_addr
//    finally
//      Free
//    end

  resolver:= THostResolver.Create(Nil);
  try
    IF resolver = NIL THEN
      debugMsg('THostResolver.Create() -> NIL');
    TRY

(* Linux resolver.AddressLookup() doesn't appear to like converting       *)
(* specials, in particular broadcast addresses.                           *)

      IF numericResolve(hostname, RESULT) THEN BEGIN
        ipAddressCount:= 1;
        EXIT
      END;

(* Using WinSock resolver.NameLookup() appears to resolve both names    *)
(* and IP addresses, under Linux (and probably other unices) only names.*)
(* As a general point try the numeric form first as being the faster    *)
(* and easier to reject and then try a name lookup.                     *)

(* Note that some versions of resolve.pas incorrectly return addresses  *)
(* in network rather than host order.                                   *)

      If resolver.AddressLookup(hostname) then BEGIN
        ipAddressCount:= resolver.AddressCount;
        IF resolver.AddressCount < 1 THEN
          debugMsg('resolver.AddressCount < 1');
(*$IFNDEF HAS_NETORDERRESOLVER *)
        debugMsg('resolver.Addresses[0] = ' + inet_ntoa(in_addr(HtoNL(resolver.Addresses[0].S_addr))));
        RESULT:= TIpMath(HtoNL(resolver.Addresses[0].S_addr));
        IF ipAddressCount > 1 THEN
          ipAddressAlternative:= TIpMath(HtoNL(resolver.Addresses[Random(ipAddressCount - 1) + 1].S_addr));
(*$ELSE                        *)
        debugMsg('resolver.Addresses[0] = ' + inet_ntoa(resolver.Addresses[0]));
        RESULT:= TIpMath(resolver.Addresses[0].S_addr);
        IF ipAddressCount > 1 THEN
          ipAddressAlternative:= TIpMath(resolver.Addresses[Random(ipAddressCount - 1) + 1].S_addr);
(*$ENDIF                       *)
        EXIT
      END ELSE
        debugMsg('resolver.AddressLookup(address) -> FALSE');
      If resolver.NameLookup(hostname) then BEGIN
        ipAddressCount:= resolver.AddressCount;
        IF resolver.AddressCount < 1 THEN
          debugMsg('resolver.AddressCount < 1');
(*$IFNDEF HAS_NETORDERRESOLVER *)
        debugMsg('resolver.Addresses[0] = ' + inet_ntoa(in_addr(HtoNL(resolver.Addresses[0].S_addr))));
        RESULT:= TIpMath(HtoNL(resolver.Addresses[0].S_addr));
        IF ipAddressCount > 1 THEN
          ipAddressAlternative:= TIpMath(HtoNL(resolver.Addresses[Random(ipAddressCount - 1) + 1].S_addr))
(*$ELSE                        *)
        debugMsg('resolver.Addresses[0] = ' + inet_ntoa(resolver.Addresses[0]));
        RESULT:= TIpMath(resolver.Addresses[0].S_addr);
        IF ipAddressCount > 1 THEN
          ipAddressAlternative:= TIpMath(resolver.Addresses[Random(ipAddressCount - 1) + 1].S_addr);
(*$ENDIF                       *)
      END ELSE
        debugMsg('resolver.NameLookup(name) -> FALSE')
    EXCEPT
      ipAddressCount:= 0
    END
  finally
    resolver.Free
  end
END { LookUpHost4 } ;


(********************************************************************************)


(* Return true if the database connection passed as the parameter appears to be
  healthy.
*)
function DBHealthy(pq: TPQConnection): boolean;

var
  clientPort, serverPort: integer;


  function testPort(port, remotePort: integer; const host: string): boolean;

  var
    sock: TSocket;
    sockaddr: sockaddr_in;
    textIn, textOut: Text;
    ident: AnsiString;

  begin
    result := false;
    sock:= fpSocket(PF_INET, SOCK_STREAM, 0);
    if sock < 0 then
      exit;
    fillChar(sockaddr, SizeOf(TSockAddr), 0);
    sockaddr.sin_family := AF_INET;
    sockaddr.sin_port := htons(113);

// I don't know whether this is really needed, or if there is an easy way of
// getting the IP address actually being used by the server. In any event, it
// could usefully be cached... although it is also a useful alternative to a
// ping which would need the binary to be blessed with elevated capabilities.

    sockaddr.sin_addr.s_addr := LookupHost4(host);
// TODO : Consider caching this particularly if ping is implemented.

// A lookup failure might indicate a permanent problem, or that a laptop etc.
// is coming back to life after being in a sleep state. The latter of those
// could be detected by periodic pings (but see above) with the result saved
// with its UTC timestamp, or by simply recovering UTC from the OS every second
// and looking for discontinuities.
//
// I've spent more time than I intended on this, so am leaving it as-is since
// overall the program is a useful proof-of-concept.

    if not Connect(sock, sockaddr, textIn, textOut) then
      exit;
    Reset(textIn);
    Rewrite(textOut);
    try
      ident := IntToStr(remotePort) + ',' + IntToStr(port);
      WriteLn(textOut, ident);
// WriteLn('> ', ident);

// Absolutely no provision for timeout etc.

      ReadLn(textIn, ident);
// WriteLn('< ', ident);
      result := Pos('USERID', ident) > 0
    finally
      CloseFile(textOut);
      CloseFile(textIn);
      CloseSocket(sock)
    end
  end { testPort } ;


begin
  clientPort := TPQConnection2(pq).GetLocalPort();
  LocalPort := clientPort;              (* For debugging                        *)
  serverPort := TPQConnection2(pq).GetServerPort();
  result := testPort(clientPort, serverPort, pq.HostName)
end { DBHealthy } ;


function socketToPort(socket: longint): integer;

var
  sockaddr: TSockAddr;
  sz: integer;

begin
  sz := SizeOf(TSockAddr);
  fillChar(sockaddr, sz, 0);
  if fpgetsockname(socket, @sockaddr, @sz) < 0 then
    result := -1
  else
    result := ntohs(sockaddr.sin_port)
// TODO : Check that the local IP address still appears in the interface list.
end { socketToPort } ;


function TPQConnection2.GetLocalPort(): integer;

var
  handle2: pointer;

begin
  if not Connected then
    exit(-1);
  handle2 := GetHandle;
  if handle2 = nil then
    exit(-1);
  handle2 := GetTransactionHandle(TSQLHandle(handle2));
  if handle2 = nil then
    exit(-1);
  result := socketToPort(PQSocket(handle2))
end { TPQConnection2.GetLocalPort } ;


function TPQConnection2.GetServerPort(): integer;

var
  handle2: pointer;

begin
  if not Connected then
    exit(-1);
  handle2 := GetHandle;
  if handle2 = nil then
    exit(-1);
  handle2 := GetTransactionHandle(TSQLHandle(handle2));
  if handle2 = nil then
    exit(-1);
  result := StrToInt(PQPort(handle2))
// TODO : Is the server IP address available in numeric form?
end { TPQConnection2.GetServerPort } ;


end.

