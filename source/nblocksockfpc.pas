//{$i edefines.inc}

unit NBlockSockFPC;

interface

uses
  Classes, SysUtils, fphttpclient, ssockets, sslsockets, opensslsockets;

type
  THookCheckConnectBreak = function(Sender: TObject; Time: Integer): Boolean of object;

  TSocketFamily = (
    SF_Any,
    SF_IP4,
    SF_IP6
    );

  {:Specify requested SSL/TLS version for secure connection.}
  TSSLType = (
    LT_all,
    LT_SSLv2,
    LT_SSLv3,
    LT_TLSv1,
    LT_TLSv1_1,
    LT_TLSv1_2,
    LT_SSHv2
    );

  ESynapseError = class(Exception)
  private
    FErrorCode: Integer;
    FErrorMessage: string;
  published
    property ErrorCode: Integer read FErrorCode Write FErrorCode;
    property ErrorMessage: string read FErrorMessage Write FErrorMessage;
  end;

  TCustomSSL = class
  private
    FCertCAFile: string;
    FSNIHost: string;
    FPrivateKeyFile: string;
    FCertificateFile: string;
    FSSLType: TSSLType;
    FVerifyCert: boolean;
    procedure SetSNIHost(avalue:string);
  public
    Handler:TSocketHandler;
    function GetSSLVersion: string;
  published
    property CertCAFile: string read FCertCAFile write FCertCAFile;
    property SNIHost:string read FSNIHost write SetSNIHost;
    property PrivateKeyFile: string read FPrivateKeyFile write FPrivateKeyFile;
    property CertificateFile: string read FCertificateFile write FCertificateFile;
    property SSLType: TSSLType read FSSLType write FSSLType;
    property VerifyCert: Boolean read FVerifyCert write FVerifyCert;
  end;

  TMyNetClient = class(TFPHTTPClient)
    property Socket;
  end;

  TTCPNBlockSocket = class(TObject)
  private
    aNetClient:TMyNetClient;
    FOnCheckConnectBreak: THookCheckConnectBreak;
    FConnectQuantum: Integer;

    FRaiseExcept: Boolean;
    FFamily: TSocketFamily;
    FFamilySave: TSocketFamily;

    FSSL: TCustomSSL;

    FLastError: integer;

    FLastErrorDesc: string;

    FHost:string;
    FPort:string;

    Procedure GetSocketHandler(Sender : TObject; Const UseSSL : Boolean; Out AHandler : TSocketHandler);
    procedure SetFamily(Value: TSocketFamily); virtual;

    function WaitSocket(ReadSocket,WriteSocket:boolean;Timeout: Integer): Boolean;

    function GetSocketError: Integer;
    procedure ExceptCheck;
  public
    constructor Create;
    destructor Destroy;override;
    procedure NonBlockConnect(const Host, Port: string; ConnectTimeOut: Integer);
    function SetKeepAlive(TurnOn: Boolean): Boolean;

    function CanWrite(Timeout: Integer): Boolean; virtual;
    procedure SendString(Data: AnsiString); virtual;

    function CanReadEx(Timeout: Integer): Boolean; virtual;

    function WaitingDataEx: Integer;

    function RecvByte(Timeout: Integer): Byte; virtual;
    function RecvBufferStr(Len: Integer; Timeout: Integer): AnsiString; virtual;

    procedure SSLDoConnect;

    procedure CloseSocket;

    property OnCheckConnectBreak: THookCheckConnectBreak read FOnCheckConnectBreak write FOnCheckConnectBreak;
    property ConnectQuantum: Integer read FConnectQuantum write FConnectQuantum;

    property RaiseExcept: Boolean read FRaiseExcept write FRaiseExcept;
    property Family: TSocketFamily read FFamily Write SetFamily;

    property SSL: TCustomSSL read FSSL;

    property LastError: Integer read FLastError;

    property LastErrorDesc: string read FLastErrorDesc;
  end;


implementation

uses
  Tools,sslbase{$ifdef UNIX},termio,sockets,BaseUnix,netdb,errors{$endif}{$ifdef MSWINDOWS},winsock{$endif};

{$IFDEF UNIX}
const
  ECONNABORTED=ESysECONNABORTED;
  ETIMEDOUT=ESysETIMEDOUT;
  EINPROGRESS=ESysEINPROGRESS;
  EWOULDBLOCK=ESysEWOULDBLOCK;
{$ENDIF}

procedure TCustomSSL.SetSNIHost(aValue:string);
begin
  FSNIHost:=aValue;
  if (Handler IS TSSLSocketHandler) then (Handler AS TSSLSocketHandler).SendHostAsSNI:=(Length(FSNIHost)>0);;
end;

function TCustomSSL.GetSSLVersion: string;
begin
  if (Handler IS TOpenSSLSocketHandler) then result:=(Handler AS TOpenSSLSocketHandler).SSL.Version;
end;

constructor TTCPNBlockSocket.Create;
begin
  inherited;
  aNetClient:=TMyNetClient.Create(nil);
  aNetClient.OnGetSocketHandler:=@GetSocketHandler;
  FOnCheckConnectBreak:= Nil;
  FConnectQuantum:= 0;
  FSSL:=TCustomSSL.Create;
end;

destructor TTCPNBlockSocket.Destroy;
begin
  aNetClient.DisconnectFromServer;
  aNetClient.Free;
  FSSL.Free;
  inherited;
end;

procedure TTCPNBlockSocket.GetSocketHandler(Sender : TObject; Const UseSSL : Boolean; Out AHandler : TSocketHandler);
Var
  SSLHandler : TSSLSocketHandler;
begin
  if UseSSL then
  begin
    SSLHandler:=TSSLSocketHandler.GetDefaultHandler;
    SSLHandler.CertificateData.CertCA.FileName:=SSL.CertCAFile;
    SSLHandler.VerifyPeerCert:=SSL.VerifyCert;
    SSLHandler.CertificateData.PrivateKey.FileName:=SSL.PrivateKeyFile;
    SSLHandler.CertificateData.Certificate.FileName:=SSL.CertificateFile;
    SSLHandler.SendHostAsSNI:=(Length(SSL.SNIHost)>0);
    case SSL.SSLType of
      LT_all       : SSLHandler.SSLType := stAny;
      LT_SSLv2     : SSLHandler.SSLType := stSSLv2;
      LT_SSLv3     : SSLHandler.SSLType := stSSLv3;
      LT_TLSv1     : SSLHandler.SSLType := stTLSv1;
      LT_TLSv1_1   : SSLHandler.SSLType := stTLSv1_1;
      LT_TLSv1_2   : SSLHandler.SSLType := stTLSv1_2;
    end;
    AHandler:=SSLHandler;
  end
  else
  begin
    AHandler:=TSocketHandler.Create;
  end;
  SSL.Handler:=AHandler;
end;

procedure TTCPNBlockSocket.SetFamily(Value: TSocketFamily);
begin
  FFamily := Value;
  FFamilySave := Value;
end;

function TTCPNBlockSocket.WaitSocket(ReadSocket,WriteSocket:boolean;Timeout: Integer): Boolean;
{$if defined(unix) or defined(windows)}
var
  FDS: TFDSet;
  PFRDS,PFWDS: PFDSet;
  TimeV: TTimeVal;
{$endif}
begin
  result:=False;
{$if defined(unix) or defined(windows)}
  PFRDS:=nil;
  PFWDS:=nil;
  TimeV.tv_usec := (Timeout mod 1000) * 1000;
  TimeV.tv_sec := Timeout div 1000;
{$endif}
{$ifdef unix}
  FDS := Default(TFDSet);
  fpFD_Zero(FDS);
  fpFD_Set(aNetClient.Socket.Handle, FDS);
  if ReadSocket then PFRDS:=@FDS;
  if WriteSocket then PFWDS:=@FDS;
  if (fpSelect(aNetClient.Socket.Handle + 1, PFRDS, PFWDS, @FDS, @TimeV)>0) then
  begin
    if ReadSocket then FDS:=PFRDS^;
    if WriteSocket then FDS:=PFWDS^;
    result:=(fpFD_ISSET(aNetClient.Socket.Handle, FDS)>0);
  end;
{$else}
{$ifdef windows}
  FDS := Default(TFDSet);
  FD_Zero(FDS);
  FD_Set(aNetClient.Socket.Handle, FDS);
  if ReadSocket then PFRDS:=@FDS;
  if WriteSocket then PFWDS:=@FDS;
  if (Select(aNetClient.Socket.Handle + 1, PFRDS, PFWDS, @FDS, @TimeV)>0) then
  begin
    if ReadSocket then FDS:=PFRDS^;
    if WriteSocket then FDS:=PFWDS^;
    result:=FD_ISSET(aNetClient.Socket.Handle, FDS);
  end;
{$endif}
{$endif}
end;

function TTCPNBlockSocket.CanWrite(Timeout: Integer): Boolean;
begin
  //result:=WaitSocket(False,True,TimeOut);
  result:=(SSL.Handler.Select([sosCanWrite],Timeout)<>[]);
end;

procedure TTCPNBlockSocket.SendString(Data: AnsiString);
var
  r,t : Longint;
begin
  if Data='' then exit;
  t:=0;
  repeat
     r:=aNetClient.WriteToSocket(Data[t+1],Length(Data)-t);
     inc(t,r);
  until (t=Length(Data)) or (r<=0);
end;

function TTCPNBlockSocket.CanReadEx(Timeout: Integer): Boolean;
const
  CHECKTIME=10;
var
  i:integer;
begin
  result:=false;
  i:=0;
  while true do
  begin
    if (i>0) then
    begin
      if (MainThreadID=GetCurrentThreadID) then CheckSynchronize(CHECKTIME) else sleep(CHECKTIME);
    end;
    //result:=WaitSocket(True,False,CHECKTIME);
    result:=(SSL.Handler.Select([sosCanRead],CHECKTIME)<>[]);
    if result then break;
    Inc(i,CHECKTIME);
    if (i>Timeout) AND (i>10) then break;
  end;
end;

function TTCPNBlockSocket.WaitingDataEx: Integer;
var
  x: Integer;
begin
  result:=0;
  {$ifdef windows}
  if (ioctlsocket(aNetClient.Socket.Handle,DWORD(FIONREAD),{%H-}x)=0) then
  begin
    result:=x;
  end;
  {$endif}
  {$ifdef UNIX}
  if (fpioctl(aNetClient.Socket.Handle,FIONREAD,{%H-}@x)>=0) then
  begin
    result:=x;
  end;
  {$endif}
end;

function TTCPNBlockSocket.RecvByte(Timeout: Integer): Byte;
var
  r:integer;
begin
  result:=0;
  //if ((WaitingDataEx>0) OR CanReadEx(Timeout)) then
  begin
    r:=aNetClient.ReadFromSocket(result,1);
  end;
end;

function TTCPNBlockSocket.RecvBufferStr(Len: Integer; Timeout: Integer): AnsiString;
var
  r:integer;
begin
  result:='';
  //if ((WaitingDataEx>0) OR CanReadEx(Timeout)) then
  begin
    SetLength(result,Len);
    r:=aNetClient.ReadFromSocket(result[1],Len);
    if (r<Len) then
      SetLength(result,r);
  end;
end;

procedure TTCPNBlockSocket.SSLDoConnect;
begin
  aNetClient.ReconnectToServer(FHost,StrToInt(FPort),True);
end;

procedure TTCPNBlockSocket.CloseSocket;
begin
  aNetClient.DisconnectFromServer;
end;

procedure TTCPNBlockSocket.NonBlockConnect(const Host, Port: string; ConnectTimeOut: Integer);
var
  TryTime: Integer;
  CW: Boolean;
  {$IFDEF LINUX}
  ip4: in_addr;
  H: THostEntry;
{$ENDIF}
begin
  {$IFDEF LINUX}
  if (NOT TryStrToHostAddr(Host,ip4)) then
  begin
    if ResolveHostByName(Host, H) then
    begin
      FHost:= NetAddrToStr(H.Addr);
      //AddLog(LOG_INFO, 'hostname resolve to ' + FHost);
    end
    else raise Exception.CreateFmt('can''t resolve hostname "%s"', [Host]);
  end
  else FHost:= Host;
  {$ELSE}
  FHost:= Host;
  {$ENDIF}
  FPort:=Port;

  aNetClient.ConnectTimeout:=ConnectTimeOut;
  aNetClient.ConnectToServer(FHost,StrToInt(FPort),False);

  TryTime:= 0;
  repeat
    if Assigned(OnCheckConnectBreak) then if OnCheckConnectBreak(Self, TryTime) then
    begin
      FLastError := ECONNABORTED;
      ExceptCheck;
      exit;
    end;
    CW:= CanWrite(FConnectQuantum);
    Inc(TryTime, FConnectQuantum);
  until (CW or (TryTime > ConnectTimeOut));
  if not CW then
  begin
    FLastError := ETIMEDOUT;
    ExceptCheck;
    exit;
  end;
  FLastError:= GetSocketError;
  ExceptCheck;
end;

function TTCPNBlockSocket.SetKeepAlive(TurnOn: Boolean): Boolean;
begin
  if TurnOn then
    aNetClient.Socket.SocketOptions:=aNetClient.Socket.SocketOptions+[soKeepAlive]
  else
    aNetClient.Socket.SocketOptions:=aNetClient.Socket.SocketOptions-[soKeepAlive];
  result:=true;
end;

function TTCPNBlockSocket.GetSocketError: Integer;
begin
  result:=aNetClient.Socket.LastError;
end;

procedure TTCPNBlockSocket.ExceptCheck;
begin
  if (LastError <> 0) and (LastError <> EINPROGRESS)
    and (LastError <> EWOULDBLOCK) then
  begin
    {$ifdef UNIX}
    FLastErrorDesc:=StrError(LastError);
    {$endif}
    if FRaiseExcept then
    begin
      raise EInvalidOpException.Create ('Fatal socket error: '+InttoStr(LastError));
    end;
  end;
end;


end.
