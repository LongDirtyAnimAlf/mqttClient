//{$i edefines.inc}

unit NBlockSock;

interface

uses
  SysUtils
  ,mormot.core.base
  ,mormot.lib.openssl11
  ,mormot.net.sock
  ,mormot.net.client
  ,mormot.net.async
  ,blcksock
  ,ssockets
  {$IF FPC_FULLVERSION >= 30200}
  //,opensslsockets
  {$ELSE}
  //,fpopenssl
  //,openssl
  {$ENDIF}
  ;

type
  THookCheckConnectBreak = function(Sender: TObject; Time: Integer): Boolean of object;

  TTCPNBlockSocket = class(TTCPBlockSocket)
  //TTCPNBlockSocket = class(TInetSocket)
  private
    FOnCheckConnectBreak: THookCheckConnectBreak;
    FConnectQuantum: Integer;
  public
    constructor Create;
    procedure NonBlockConnect(const Host, Port: string; ConnectTimeOut: Integer);
    function SetKeepAlive(TurnOn: Boolean): Boolean;
    function GetSocketError: Integer;
    //
    property OnCheckConnectBreak: THookCheckConnectBreak read FOnCheckConnectBreak write FOnCheckConnectBreak;
    property ConnectQuantum: Integer read FConnectQuantum write FConnectQuantum;
  end;

  //TSocketFamily = (
  //  SF_Any,
  //  SF_IP4,
  //  SF_IP6
  //  );

  TMQTTConnection = class(TCrtSocket)
  private
    FOnCheckConnectBreak: THookCheckConnectBreak;
    FConnectQuantum: Integer;
    FRaiseExcept: boolean;
    FFamily:TSocketFamily;
    FLastError:integer;
    FLastErrorDesc:string;
  public
    type
      TSSL = class
         CertCAFile : string;
         VerifyCert : boolean;
         SNIHost    : string;
         PrivateKeyFile : string;
         CertificateFile : string;
         SSLType: TSSLType;
         function GetSSLVersion:string;
      end;
    var
    SSL:TSSL;
    constructor Create;reintroduce;
    procedure NonBlockConnect(const aHost, aPort: string; aConnectTimeOut: Integer);
    function SetKeepAlive(TurnOn: Boolean): Boolean;reintroduce;
    function GetSocketError: Integer;
    function CanWrite(aTimeout: Integer):boolean;
    procedure SendString(Data: AnsiString);

    function CanReadEx(aTimeout: Integer): Boolean;
    function WaitingDataEx: Integer;
    function RecvByte(aTimeout: Integer): Byte;

    function RecvBufferStr(aLen: Integer; aTimeout: Integer): AnsiString;

    procedure SSLDoConnect;
    procedure CloseSocket;

    property OnCheckConnectBreak: THookCheckConnectBreak read FOnCheckConnectBreak write FOnCheckConnectBreak;
    property ConnectQuantum: Integer read FConnectQuantum write FConnectQuantum;
    property Family: TSocketFamily read FFamily Write FFamily;
    property RaiseExcept: Boolean read FRaiseExcept write FRaiseExcept;
    property LastError: Integer read FLastError;
    property LastErrorDesc: string read FLastErrorDesc;
  published
  end;

  TFPCTCPNBlockSocket = class(TInetSocket)
  private
  public
    constructor Create;
  end;


implementation

uses
  {$IFDEF LINUX}netdbn, Sockets, synaip,{$ENDIF} synsock;

constructor TTCPNBlockSocket.Create;
begin
  inherited;
  FOnCheckConnectBreak:= Nil;
  FConnectQuantum:= 1000;
end;

procedure TTCPNBlockSocket.NonBlockConnect(const Host, Port: string; ConnectTimeOut: Integer);
var
  TryTime: Integer;
  CW: Boolean;
  IP: String;
  {$IFDEF LINUX}
  H: THostEntry;
  {$ENDIF}
begin
  try
    Family:= SF_IP4;
    {$IFDEF LINUX}
    if not IsIP(Host) then
    begin
      if ResolveHostByName(Host, H) then
      begin
        IP:= NetAddrToStr(H.Addr);
        AddLog(LOG_INFO, 'hostname resolve to ' + IP);
      end
      else raise Exception.CreateFmt('can''t resolve hostname "%s"', [Host]);
    end
    else IP:= Host;
    {$ELSE}
    IP:= Host;
    {$ENDIF}
    if (Socket = INVALID_SOCKET) then CreateSocketByName(IP);
    NonBlockMode:= True;
    try
      Connect(IP, Port);
      TryTime:= 0;
      repeat
        if Assigned(OnCheckConnectBreak) then if OnCheckConnectBreak(Self, TryTime) then
        begin
          FLastError := WSAECONNABORTED;
          ExceptCheck;
          Exit
        end;
        CW:= CanWrite(FConnectQuantum);
        Inc(TryTime, FConnectQuantum);
      until (CW or (TryTime > ConnectTimeOut));
      if not CW then
      begin
        FLastError := WSAETIMEDOUT;
        ExceptCheck;
        Exit
      end;
      //
      FLastError:= GetSocketError;
      ExceptCheck;
      //
      GetSins;
    finally
      NonBlockMode:= False; // turn to block mode
    end;
  except
    AbortSocket;
    raise;
  end;
end;

function TTCPNBlockSocket.SetKeepAlive(TurnOn: Boolean): Boolean;
var Val: LongInt;
begin
  if TurnOn then Val:= 1 else Val:= 0;
  result:= (synsock.SetSockOpt(FSocket, integer(SOL_SOCKET), integer(SO_KEEPALIVE), @Val, SizeOf(Val)) = 0);
end;

function TTCPNBlockSocket.GetSocketError: Integer;
var
  RetVal, RetValLength: Integer;
begin
  RetVal:= -1;
  RetValLength:= SizeOf(RetVal);
  if synsock.GetSockOpt(Socket, SOL_SOCKET, SO_ERROR, @RetVal, RetValLength) < 0 then
  begin
    result:= WSAECONNREFUSED;
    Exit
  end;
  result:= RetVal;
end;

constructor TMQTTConnection.Create;
begin
  inherited Create;
end;

procedure TMQTTConnection.NonBlockConnect(const aHost, aPort: string; aConnectTimeOut: Integer);
begin
  OpenBind(aHost,aPort,{bind=}false,{tls=}false);
  //SendTimeout:=
  //ReceiveTimeout:=
end;

function TMQTTConnection.SetKeepAlive(TurnOn: Boolean): Boolean;
begin
  inherited SetKeepAlive(TurnOn);
  //Self.Sock^.SetKeepAlive(TurnOn);
  result:=true;
end;

function TMQTTConnection.GetSocketError: Integer;
var
  RetVal, RetValLength: Integer;
begin
  RetVal:= -1;
  RetValLength:= SizeOf(RetVal);
  if GetSockOpt(TSocket(Self.Sock), SOL_SOCKET, SO_ERROR, @RetVal, RetValLength) < 0 then
  begin
    result:= WSAECONNREFUSED;
    Exit
  end;
  result:= RetVal;
end;

function TMQTTConnection.CanWrite(aTimeout: Integer):boolean;
begin
end;

procedure TMQTTConnection.SendString(Data: AnsiString);
begin
end;

function TMQTTConnection.CanReadEx(aTimeout: Integer): Boolean;
begin
end;

function TMQTTConnection.WaitingDataEx: Integer;
begin
end;

function TMQTTConnection.RecvByte(aTimeout: Integer): Byte;
begin
end;

function TMQTTConnection.RecvBufferStr(aLen: Integer; aTimeout: Integer): AnsiString;
begin
end;

procedure TMQTTConnection.SSLDoConnect;
begin
  TLS.WithPeerInfo := true;
  TLS.IgnoreCertificateErrors := true;
  TLS.CACertificatesFile:=Self.SSL.CertCAFile;
  TLS.CertificateFile:=Self.SSL.CertificateFile;
  TLS.PrivateKeyFile:=Self.SSL.PrivateKeyFile;
  TLS.Enabled:=True;
  //OpenBind(aHost,aPort,{bind=}false,{tls=}true);
end;

procedure TMQTTConnection.CloseSocket;
begin
end;

function TMQTTConnection.TSSL.GetSSLVersion:string;
begin
end;

constructor TFPCTCPNBlockSocket.Create;
Var
  G : TSocketHandler;
begin
  inherited;
  //G:=GetSocketHandler(UseSSL);
  //FSocket:=TInetSocket.Create(AHost,APort,G);

end;





end.
