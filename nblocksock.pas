//{$i edefines.inc}

unit NBlockSock;

interface

uses
  SysUtils, blcksock;

type
  THookCheckConnectBreak = function(Sender: TObject; Time: Integer): Boolean of object;

  TTCPNBlockSocket = class(TTCPBlockSocket)
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


end.
