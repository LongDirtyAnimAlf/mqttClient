unit mqttClient;

// Original sources by Alexey Lutovinin
// https://github.com/crossrw/mqttClient

{$DEFINE DEBUG_MQTT}

interface

uses
  {$ifndef FPC}
  Windows,
  {$endif}
  SysUtils, Classes, Contnrs, SyncObjs,
  {$ifdef FPC}
  eventlog,
  {$endif FPC}
  BlckSock, NBlockSock, ssl_openssl;

const
  MAXRecMessagesCount = 1000;
  MAXPubMessagesCount = 4000;

type
  TErrorHook = procedure(Sender: TObject; const Value: Integer; const Desc: String) of object;
  TConnectHook = procedure(Sender: TObject) of object;
  TDisconnectHook = procedure(Sender: TObject) of object;

  TMQTTMessageType = (
      mtReserved0 = 0,                                                          // Reserved
      mtCONNECT = 1,                                                            // Client request to connect to Broker
      mtCONNACK = 2,                                                            // Connect Acknowledgment
      mtPUBLISH = 3,                                                            // Publish message
      mtPUBACK = 4,                                                             // Publish Acknowledgment
      mtPUBREC = 5,                                                             // Publish Received (assured delivery part 1)
      mtPUBREL = 6,                                                             // Publish Release (assured delivery part 2)
      mtPUBCOMP = 7,                                                            // Publish Complete (assured delivery part 3)
      mtSUBSCRIBE = 8,                                                          // Client Subscribe request
      mtSUBACK = 9,                                                             // Subscribe Acknowledgment
      mtUNSUBSCRIBE = 10,                                                       // Client Unsubscribe request
      mtUNSUBACK = 11,                                                          // Unsubscribe Acknowledgment
      mtPINGREQ = 12,                                                           // PING Request
      mtPINGRESP = 13,                                                          // PING Response
      mtDISCONNECT = 14,                                                        // Client is Disconnecting
      mtReserved15 = 15                                                         // Reserved
     );

  TMQTTStatus = record
      Connected: Boolean;
      LastErrorMsg: String;
      InBufferCount: Integer;
      WaitAckCount: Integer;
      ReceivedCount: LongWord;
      PublishedCount: LongWord;
      DroppedCount: LongWord;
  end;

  MQTTException = exception;

  TControlPacket = record
      FH: Byte;
      VH: String;
      Payload: String;
      //
      RQoS: Byte;
      RDup: Boolean;
      RRetain: Boolean;
  end;

  TMQTTMessage = class
    private
       FSendFlag: Boolean;
       FPacketId: Word;
    public
       FTopic: String;
       FMessage: String;
       FRetain: Boolean;
       FQoS: Byte;
       //
       constructor Create(const ATopic, AMessage: String; AQoS: Byte; ARetain: Boolean; APacketId: Word);
  end;

  TMQTTClient = class(TThread)
    private
       FHost: String;
       FPort: Word;
       FUserName: String;
       FPassword: String;
       FTimeOut: Integer;
       FTCP: TTCPNBlockSocket;
       FConnected: Boolean;
       FLoggedIn: Boolean;
       FWillTopic: String;
       FWillMessage: String;
       FClientId: String;
       FKeepAlive: Word;
       FWillQoS: Byte;
       FAddTimeStamp: Boolean;
       FLastControlSend: LongWord;
       FLastControlRecv: LongWord;
       FWaitPINGRESP: Boolean;
       FPacketId: Word;
       //
       FCritSection: TCriticalSection;
       FSubscribeList: TObjectList;
       FSubscribeListChanged: Boolean;
       FRecMessages: TObjectQueue;                                              // буфер принятых сообщений
       //
       FSendMessagesChanged: Boolean;
       FSendMessages: TObjectList;                                              // буфер уже отправленных сообщений, которые ожидают ACK
       FPubMessages: TObjectQueue;                                              // буфер сообщений ожидающих отправку
       FBuffering: Boolean;
       //
       FLastErrorMessage: String;
       FPublishedCount: LongWord;
       FReceivedCount: LongWord;
       FDroppedCount: LongWord;
       //
       FUseSSL: Boolean;
       FSSLCertCAFile: String;
       FPrivateKeyFile: String;
       FCertificateFile: String;
       //
       FOnError: TErrorHook;
       FOnConnect: TConnectHook;
       FOnDisconnect: TDisconnectHook;
       //
       {$ifdef FPC}
       FLog:TEventLog;
       {$endif FPC}
        //
       procedure DoConnect;
       procedure DoLogin;
       procedure AfterLogin;
       procedure DoDisconnect(SendDISCONNECT: Boolean);
       procedure DoSubscribe;
       procedure DoReceive;
       procedure DoSend;
       //
       procedure GetNextPacketId;
       function GetPubMessage: TMQTTMessage;
       procedure ThreadSleep(ms: Integer);
       //
       procedure SendControlPacket(const Data: TControlPacket);
       function ReceiveControlPacket(var Data: TControlPacket; const WaitTimeOut: Integer = 10): Boolean;
       procedure PublishMessage(MQTTMsg: TMQTTMessage);
       procedure CheckKeepAlive;
       //
       function UTCTimeStamp: String;
       //
       function NeedTerminate(Sender: TObject; Time: Integer): Boolean;
       procedure SetLastErrorMessage(const S: String);

       procedure DoOnConnect;
       procedure DoOnDisconnect;
       procedure DoOnError;

       {$ifdef FPC}
        procedure AddLog(EventType : TEventType; const Msg : String);
       {$endif FPC}
    public
       constructor Create;
       destructor Destroy; override;
       //
       procedure Subscribe(const ATopic: String);
       procedure Publish(const ATopic, AMessage: String; const AQoS: Byte = 0; const ARetain: Boolean = False);
       function GetMessage: TMQTTMessage;
       //
       procedure GetStatus(var MQTTStatus: TMQTTStatus);
       //
       property Host: String read FHost write FHost;
       property Port: Word read FPort write FPort;
       property UserName: String read FUserName write FUserName;
       property Password: String read FPassword write FPassword;
       property WillTopic: String read FWillTopic write FWillTopic;
       property WillMessage: String read FWillMessage write FWillMessage;
       property KeepAlive: Word read FKeepAlive write FKeepAlive;
       property WillQoS: Byte read FWillQoS write FWillQoS;
       property ClientId: String read FClientId write FClientId;
       property Buffering: Boolean read FBuffering write FBuffering;
       property AddTimeStamp: Boolean read FAddTimeStamp write FAddTimeStamp;
       //
       property SSLUse: Boolean read FUseSSL write FUseSSL;
       property SSLCertCAFile: String read FSSLCertCAFile write FSSLCertCAFile;
       property SSLPrivateKeyFile: String read FPrivateKeyFile write FPrivateKeyFile;
       property SSLCertificateFile: String read FCertificateFile write FCertificateFile;

       property OnError: TErrorHook read FOnError write FOnError;
       property OnConnect: TConnectHook read FOnConnect write FOnConnect;
       property OnDisconnect: TDisconnectHook read FOnDisconnect write FOnDisconnect;
    protected
       procedure Execute; override;
  end;

implementation

uses
  DateUtils,
  SynaUtil, SynaChar;

const
  {$ifdef FPC}
  LOG_DEBUG   = etDebug;
  LOG_INFO    = etInfo;
  LOG_WARNING = etWarning;
  LOG_ERR     = etError;
  {$else}
  LOG_DEBUG   = 1;
  LOG_INFO    = 2;
  LOG_WARNING = 3;
  LOG_ERR     = 4;
  {$endif FPC}

{$ifndef FPC}
procedure AddLog(Level:integer; Msg:string);
const
  LOGFILE = 'delphilog.txt';
var
  F:TextFile;
begin
  AssignFile(F, LOGFILE);
  try
    if NOT FileExists(LOGFILE) then
      Rewrite(F)
    else
      Append(F);
    write(F,'['+DateTimeToStr(Now)+' ');
    case Level of
      1:write(F,'DEBUG  ');
      2:write(F,'INFO   ');
      3:write(F,'WARNING');
      4:write(F,'ERROR  ');
      else write(F,'UNKNOWN');
    end;
    write(F,'] ');
    writeln(F,Msg);
  finally
    CloseFile(F);
  end;
end;
{$endif}

type
  TSubscribeItem = class
    FTopic: String;
    FSended: Boolean;
    FAck: Boolean;
    FPacketId: Word;
    //
    constructor Create(const ATopic: String);
    procedure Reset;
  end;

function RemainingLength(MsgLen: Integer): String;
var
  Digit: Integer;
  R: String;
begin
  R:= '';
  repeat
    Digit:= MsgLen mod 128;
    MsgLen:= MsgLen div 128;
    if MsgLen > 0 then Digit:= Digit or $80;
    R:= R + Char(Digit);
  until MsgLen = 0;
  result:= R;
end;

function FixedHeader(AMessageType: TMQTTMessageType; ADup: Boolean; AQOS: Byte; ARetain: Boolean): Byte;
var
  FH: Byte;
begin
  // |7 6 5 4  | 3    | 2  1 | 0      |
  // |Message  | DUP  | QoS  | RETAIN |
  // |type     | flag |      |        |
  FH:= Byte(Ord(AMessageType) shl 4) + (AQoS shl 1);
  if ARetain then FH:= FH or 1;
  if ADup then FH:= FH or 8;
  result:= FH;
end;

function FixedHeaderType(FH: Byte): TMQTTMessageType;
begin
  result:= TMQTTMessageType(FH shr 4);
end;

function StrToMQTT(const S: String): String;
begin
  result:= Char(Length(S) div 256) + Char(Length(S) mod 256) + S;
end;

function MQTTMessageType(FH: Byte): String;
begin
  Case FixedHeaderType(FH) of
    mtReserved0:   result:= 'Reserved0';
    mtCONNECT:     result:= 'CONNECT';
    mtCONNACK:     result:= 'CONNACK';
    mtPUBLISH:     result:= 'PUBLISH';
    mtPUBACK:      result:= 'PUBACK';
    mtPUBREC:      result:= 'PUBREC';
    mtPUBREL:      result:= 'PUBREL';
    mtPUBCOMP:     result:= 'PUBCOMP';
    mtSUBSCRIBE:   result:= 'SUBSCRIBE';
    mtSUBACK:      result:= 'SUBACK';
    mtUNSUBSCRIBE: result:= 'UNSUBSCRIBE';
    mtUNSUBACK:    result:= 'UNSUBACK';
    mtPINGREQ:     result:= 'PINGREQ';
    mtPINGRESP:    result:= 'PINGRESP';
    mtDISCONNECT:  result:= 'DISCONNECT';
    mtReserved15:  result:= 'Reserved15';
    else
      result:= Format('unknown (FH:%u)', [FH]);
  end;
end;

// -----------------------------------------------------------------------------

constructor TSubscribeItem.Create(const ATopic: String);
begin
  inherited Create;
  //
  FTopic:= ATopic;
  Reset;
end;

procedure TSubscribeItem.Reset;
begin
  FSended:= False;
  FAck:= False;
  FPacketId:=0;
end;

// -----------------------------------------------------------------------------

constructor TMQTTMessage.Create(const ATopic, AMessage: String; AQoS: Byte; ARetain: Boolean; APacketId: Word);
begin
  inherited Create;
  //
  FTopic:= ATopic;
  FMessage:= AMessage;
  FQoS:= AQoS;
  FRetain:= ARetain;
  FPacketId:= APacketId;
  FSendFlag:= False;
end;

// -----------------------------------------------------------------------------

constructor TMQTTClient.Create;
begin
  inherited Create(True);
  //
  {$ifdef FPC}
  FLog:=TEventLog.Create(nil);
  FLog.LogType:=ltFile;
  FLog.FileName:='mqttdebug.log';
  {$endif FPC}
  //
  FHost:= '';
  FPort:= 1883;
  FLastErrorMessage:= '';
  //
  FUserName:= '';
  FPassword:= '';
  FWillTopic:= '';
  FWillMessage:= '';
  FLastControlSend:= 0;
  FLastControlRecv:= 0;
  FWaitPINGRESP:= False;
  FKeepAlive:= 0;
  FWillQoS:= 0;
  FClientId:= 'client' + IntToStr(Random(MaxInt) + 1);
  FPacketId:= 1;
  FAddTimeStamp:= False;
  //
  FTCP:= TTCPNBlockSocket.Create;
  FTCP.ConnectQuantum:= 200;
  FTCP.RaiseExcept:= True;
  FTCP.Family:= SF_IP4;
  FTCP.OnCheckConnectBreak:={$ifdef FPC}@{$endif FPC}NeedTerminate;
  //
  FCritSection:= TCriticalSection.Create;
  FSubscribeList:= TObjectList.Create;
  FSubscribeListChanged:= False;
  FRecMessages:= TObjectQueue.Create;
  //
  FPubMessages:= TObjectQueue.Create;
  FSendMessages:= TObjectList.Create;
  FSendMessagesChanged:= False;
  FBuffering:= False;
  //
  FUseSSL:= False;
  FSSLCertCAFile:= '';
  FPrivateKeyFile:= '';
  FCertificateFile:= '';
  //
  FPublishedCount:= 0;
  FReceivedCount:= 0;
  FDroppedCount:= 0;
  //
  FTimeOut:= 5000;
  FConnected:= False;
  FLoggedIn:= False;
end;

destructor TMQTTClient.Destroy;
begin
  inherited;
  //
  DoDisconnect(True);
  //
  FreeAndNil(FSendMessages);
  //
  while (FPubMessages.Count>0) do TMQTTMessage(FPubMessages.Pop).Free;
  FreeAndNil(FPubMessages);
  //
  while (FRecMessages.Count>0) do TMQTTMessage(FRecMessages.Pop).Free;
  FreeAndNil(FRecMessages);
  //
  FreeAndNil(FSubscribeList);
  FreeAndNil(FCritSection);
  FreeAndNil(FTCP);

  {$ifdef FPC}
  FLog.Destroy;
  {$endif FPC}
end;

// возвращает строку с добавленными в начале двумя байтами длинны

procedure TMQTTClient.GetNextPacketId;
begin
  Inc(FPacketId);
  if FPacketId = 0 then Inc(FPacketId);
  Assert((FPacketId > Low(Word)), 'Message ID too low');
  Assert((FPacketId < High(Word)), 'Message ID has gotten too big');
end;

procedure TMQTTClient.SendControlPacket(const Data: TControlPacket);
var
  Packet: String;
begin
  {$IFDEF DEBUG_MQTT}
  AddLog(LOG_DEBUG, Format('TMQTTClient.SendCtrlPacket(%s), FH:%.2x VHlen:%d PayloadLen:%d', [MQTTMessageType(Data.FH), Data.FH, Length(Data.VH), Length(Data.Payload)]));
  {$ENDIF}
  //
  Packet:= Char(Data.FH) + RemainingLength(Length(Data.VH) + Length(Data.Payload)) + Data.VH + Data.Payload;
  if FTCP.CanWrite(FTimeOut) then
  begin
    FTCP.SendString(Packet);
    FLastControlSend:= GetTick;
  end
  else raise MQTTException.Create('can''t send control packet');
end;

function TMQTTClient.ReceiveControlPacket(var Data: TControlPacket; const WaitTimeOut: Integer): Boolean;
var
  RL, Digit, Mul: Integer;
begin
  result:= False;
  if not FTCP.CanReadEx(WaitTimeOut) then exit;
  if FTCP.WaitingDataEx <= 0 then raise MQTTException.Create('connection reset by peer');
  Data.FH:= FTCP.RecvByte(FTimeOut);
  {$IFDEF DEBUG_MQTT}
  AddLog(LOG_DEBUG, Format('TMQTTClient.Pre-receiveControlPacket(%s), QoS:%d FH:%.2x RL:%d Bytes:%d', [MQTTMessageType(Data.FH), Data.RQoS, Data.FH, RL, FTCP.WaitingDataEx]));
  {$ENDIF}
  RL:= 0;
  Mul:= 1;
  repeat
    Digit:= FTCP.RecvByte(FTimeOut);
    RL:= RL + (Digit and $7F)*Mul;
    Mul:= Mul * 128;
  until (Digit and $80) = 0;
  Data.VH:= FTCP.RecvBufferStr(RL, FTimeOut);
  Data.Payload:= '';
  //
  Data.RRetain:= (Data.FH and 1) <> 0;
  Data.RQoS:= (Data.FH shr 1) and 3;
  Data.RDup:= (Data.FH and 8) <> 0;
  //
  result:= True;
  FLastControlRecv:= GetTick();
  {$IFDEF DEBUG_MQTT}
  AddLog(LOG_DEBUG, Format('TMQTTClient.ReceiveControlPacket(%s), QoS:%d FH:%.2x RL:%d', [MQTTMessageType(Data.FH), Data.RQoS, Data.FH, RL]));
  {$ENDIF}
  //
  if Data.RQoS > 2 then raise MQTTException.Create('invalid value of QoS');
  if Data.RQoS > 1 then raise MQTTException.Create('unsupported QoS > 1');
end;

procedure TMQTTClient.CheckKeepAlive;
var
  CtrlPacket: TControlPacket;
  TickNow: LongWord;
begin
  TickNow:= GetTick();
  if TickDelta(FLastControlRecv, TickNow) > FKeepAlive*1000 then raise MQTTException.Create('keepalive timeout expired');
  if (not FWaitPINGRESP) and (TickDelta(FLastControlSend, TickNow) > (FKeepAlive*3000 div 4)) then
  begin
    CtrlPacket.FH:= FixedHeader(mtPINGREQ, False, 0, False);
    CtrlPacket.VH:= '';
    CtrlPacket.Payload:= '';
    SendControlPacket(CtrlPacket);
    FWaitPINGRESP:= True;
  end;
end;

function TMQTTClient.UTCTimeStamp: String;
{$ifndef FPC}
const
  StartOfEpoch = 25569;
var
  st: TSystemTime;
{$endif}
begin
  {$ifdef FPC}
  result:= Format('@%d%.3d', [SecondsBetween(NowUTC, UnixDateDelta), GetTick mod 1000]);
  {$else}
  GetSystemTime(st);
  result:= Format('@%d%.3d', [SecondsBetween(SysUtils.SystemTimeToDateTime (st), StartOfEpoch), GetTick mod 1000]);
  {$endif FPC}
end;

function TMQTTClient.NeedTerminate(Sender: TObject; Time: Integer): Boolean;
begin
  result:= Terminated;
end;

procedure TMQTTClient.SetLastErrorMessage(const S: String);
begin
  FCritSection.Enter;
  FLastErrorMessage:= S;
  FCritSection.Leave;
end;

procedure TMQTTClient.DoConnect;
begin
  {$IFDEF DEBUG_MQTT}
  AddLog(LOG_DEBUG, 'TMQTTClient.DoConnect');
  {$ENDIF}
  //
  if FConnected then DoDisconnect(True);
  //
  FTCP.NonBlockConnect(FHost, IntToStr(FPort), FTimeOut);
  {$IFDEF DEBUG_MQTT}
  AddLog(LOG_INFO, Format('MQTTClient: connected to %s.%u', [FHost, FPort]));
  {$ENDIF}
  if not FTCP.SetKeepAlive(True) then
  begin
    {$IFDEF DEBUG_MQTT}
    AddLog(LOG_WARNING, 'MQTTClient: can''t set SO_KEEPALIVE option');
    {$ENDIF}
  end;
  //
  if FUseSSL then
  begin
    FTCP.SSL.CertCAFile := FSSLCertCAFile;
    if (Pos('amazonaws.com',Host)=0) then
    begin
      if (Length(FSSLCertCAFile)>0) then FTCP.SSL.VerifyCert := True;
    end
    else
    begin
      FTCP.SSL.SNIHost := Self.Host;
    end;
    FTCP.SSL.PrivateKeyFile := FPrivateKeyFile;
    FTCP.SSL.CertificateFile := FCertificateFile;
    FTCP.SSL.SSLType := LT_TLSv1_2;
    //
    FTCP.SSLDoConnect;
    FTCP.SSL.SNIHost := '';
    {$IFDEF DEBUG_MQTT}
    AddLog(LOG_INFO, Format('MQTTClient: SSL/TLS connection established, version: %s', [FTCP.SSL.GetSSLVersion()]));
    {$ENDIF}
  end;
  FWaitPINGRESP:= False;
  FConnected:=(FTCP.LastError=0);
end;

procedure TMQTTClient.DoLogin;
var
  CtrlPacket: TControlPacket;
  CF: Byte;
begin
  {$IFDEF DEBUG_MQTT}
   AddLog(LOG_DEBUG, 'TMQTTClient.DoLogin');
  {$ENDIF}
  with CtrlPacket do
  begin
    FH:= FixedHeader(mtCONNECT, False, 0, False);
    //
    VH:= StrToMQTT('MQTT');          // protocol name
    VH:= VH + Char(4);               // protocol level
    // client identifier
    Payload:= StrToMQTT(FClientId);
    // connect flag:
    //   7      6      5      4   3  2     1       0
    //   _      _      _      _____  _     _       _
    //   user   pass   will   will   will  clean   reserv
    //   name   flag   retain QoS    flag  session
    //   flag
    CF:= 2;                                          // clean session
    if Length(FWillTopic) > 0 then
    begin
      CF:= CF + 4;                                    // will flag
      CF:= CF + FWillQoS shl 3;                       // will QoS
      //CF:= CF + 32;                                 // will retain ???
      Payload:= Payload + StrToMQTT(FWillTopic) + StrToMQTT(FWillMessage);
    end;
    if (Length(FUserName) > 0) then
    begin
      CF:= CF + 128;                                  // user name flag
      Payload:= Payload + StrToMQTT(FUserName);
      if Length(FPassword) > 0 then
      begin
        CF:= CF + 64;                                  // password flag
        Payload:= Payload + StrToMQTT(FPassword);
      end;
    end;
    VH:= VH + Char(CF);
    // keep alive bytes
    VH:= VH + Char(FKeepAlive div 256) + Char(FKeepAlive mod 256);
  end;
  //
  SendControlPacket(CtrlPacket);
  if not ReceiveControlPacket(CtrlPacket, 10000) then raise MQTTException.Create('broker not answer');
  if FixedHeaderType(CtrlPacket.FH) <> mtCONNACK then raise MQTTException.CreateFmt('unexpected packet FH:%.2x instead CONNACK', [CtrlPacket.FH]);
  if Length(CtrlPacket.VH) < 2 then raise MQTTException.Create('CONNACK not have return code field');
  Case CtrlPacket.VH[2] of
     #0:
     begin
      {$IFDEF DEBUG_MQTT}
      AddLog(LOG_INFO, 'TMQTTClient: connection accepted');
      {$ENDIF}
     end;
     #1: raise MQTTException.Create('connection refused, unacceptable protocol version');
     #2: raise MQTTException.Create('connection refused, identifier rejected');
     #3: raise MQTTException.Create('connection refused, server unavailable');
     #4: raise MQTTException.Create('connection refused, bad user name or password');
     #5: raise MQTTException.Create('connection refused, not authorized');
     else raise MQTTException.CreateFmt('connection refused, return code:0x%.2x', [Byte(CtrlPacket.VH[2])]);
  end;
  SetLastErrorMessage('');
  FLoggedIn:= True;
  AfterLogin;
end;

procedure TMQTTClient.AfterLogin;
var
  I: Integer;
begin
  FPacketId:= 1;
  for I:= 1 to FSendMessages.Count do (FSendMessages.Items[I-1] as TMQTTMessage).FSendFlag:= False;
  FSendMessagesChanged:= FSendMessages.Count > 0;
  FCritSection.Enter;
  try
    for I:= 1 to FSubscribeList.Count do (FSubscribeList.Items[I-1] as TSubscribeItem).Reset;
    FSubscribeListChanged:= True;
  finally
    FCritSection.Leave;
  end;
end;

procedure TMQTTClient.DoSubscribe;
const
  MaximumQoS = #1;
var
  CtrlPacket: TControlPacket;
  SI: TSubscribeItem;
  I: Integer;
begin
  with CtrlPacket do
  begin
    Payload:= '';
    //
    FCritSection.Enter;
    try
      for I:= 1 to FSubscribeList.Count do
      begin
        SI:= FSubscribeList.Items[I-1] as TSubscribeItem;
        if not SI.FSended then
        begin
          SI.FPacketId:= FPacketId;
          SI.FSended:= True;
          //
          Payload:= Payload + StrToMQTT(SI.FTopic) + MaximumQoS;
        end;
      end;
      FSubscribeListChanged:= False;
    finally
      FCritSection.Leave;
    end;
    if Length(Payload) > 0 then
    begin
      FH:= FixedHeader(mtSUBSCRIBE, False, 1, False);
      // PacketId
      VH:= Char(FPacketId div 256) + Char(FPacketId mod 256);
      SendControlPacket(CtrlPacket);
      GetNextPacketId;
    end;
  end; // with
end;

procedure TMQTTClient.DoDisconnect(SendDISCONNECT: Boolean);
var
  Data: TControlPacket;
begin
  {$IFDEF DEBUG_MQTT}
  AddLog(LOG_INFO, 'TMQTTClient.DoDisconnect');
  {$ENDIF}
  //
  if SendDISCONNECT and FLoggedIn and FTCP.CanWrite(1) then
  begin
    Data.FH:= FixedHeader(mtDISCONNECT, False, 0, False);
    Data.VH:= '';
    Data.Payload:= '';
    SendControlPacket(Data);
  end;
  //
  FTCP.CloseSocket;
  FConnected:= False;
  FLoggedIn:= False;
end;

procedure TMQTTClient.DoReceive;
var
  RecData: TControlPacket;
  SI: TSubscribeItem;
  aMQTTMessage:TMQTTMessage;
  AppMessage: String;
  TopicName: String;
  Index: Integer;
  PacketId: Word;
  I: Integer;
begin
  FillChar({%H-}RecData,SizeOf(RecData),0);
  if ReceiveControlPacket({%H-}RecData) then
  begin
    Case FixedHeaderType(RecData.FH) of
      mtPINGRESP: FWaitPINGRESP:= False;                                           // PINGRESP
      mtSUBACK:
      begin                                                              // SUBACK
        PacketId:= Byte(RecData.VH[1]) * 256 + Byte(RecData.VH[2]);
        FCritSection.Enter;
        try
          for I:= 1 to FSubscribeList.Count do
          begin
            SI:= FSubscribeList.Items[I-1] as TSubscribeItem;
            if SI.FSended and (not SI.FAck) and (SI.FPacketId = PacketId) then
            begin
              SI.FAck:= True;
              {$IFDEF DEBUG_MQTT}
              AddLog(LOG_INFO, Format(' subscribe on "%s" confirmed', [SI.FTopic]));
             {$ENDIF}
            end;
          end;
        finally
          FCritSection.Leave;
        end;
      end;
      mtPUBLISH:
      begin                                                             // PUBLISH
        Index:= Byte(RecData.VH[1]) * 256 + Byte(RecData.VH[2]);
        TopicName:= Copy(RecData.VH, 3, Index);
        Index:= Index + 3;
        if RecData.RQoS > 0 then
        begin
          PacketId:= Byte(RecData.VH[Index]) * 256 + Byte(RecData.VH[Index+1]);
          Index:= Index + 2;
        end
        else PacketId:= 0;
        AppMessage:= Copy(RecData.VH, Index, Length(RecData.VH) - Index + 1);
        //
        {$IFDEF DEBUG_MQTT}
        AddLog(LOG_DEBUG, Format(' packetId:%u topic:"%s" message:"%s"', [PacketId, TopicName, AppMessage]));
        {$ENDIF}
        FCritSection.Enter;
        try
          if (FRecMessages.Count<MAXRecMessagesCount) then
          begin
            aMQTTMessage:=TMQTTMessage.Create(TopicName, AppMessage, RecData.RQoS, RecData.RRetain, PacketId);
            FRecMessages.Push(aMQTTMessage);
            //Inc(FReceivedCount);
          end;
          Inc(FReceivedCount);
        finally
          FCritSection.Leave;
        end;
        if RecData.RQoS = 1 then
        begin
          RecData.FH:= FixedHeader(mtPUBACK, False, 0, False);
          RecData.VH:= Char(PacketId div 256) + Char(PacketId mod 256);
          RecData.Payload:= '';
          SendControlPacket(RecData);
        end;
      end;
      mtPUBACK:
      begin                                                              // PUBACK
        PacketId:= Byte(RecData.VH[1]) * 256 + Byte(RecData.VH[2]);
        for I:= 1 to FSendMessages.Count do
        begin
          if (FSendMessages.Items[I-1] as TMQTTMessage).FPacketId = PacketId then
          begin
            FSendMessages.Delete(I-1);
            //
            {$IFDEF DEBUG_MQTT}
            AddLog(LOG_DEBUG, Format(' packetId:%u confirmed', [PacketId]));
            {$ENDIF}
            //
            Break;
          end;
        end;
      end;
    else
      begin
        // To prevent FPC warnings
      end;
    end;
  end;
end;

procedure TMQTTClient.DoSend;
var
  MM: TMQTTMessage;
  I: Integer;
begin
  if FSendMessagesChanged then
  begin
    for I:= 1 to FSendMessages.Count do
    begin
      MM:= FSendMessages.Items[I-1] as TMQTTMessage;
      if not MM.FSendFlag then
      begin
        PublishMessage(MM);
      end;
    end;
    FSendMessagesChanged:= False;
  end;
  repeat
    if (FSendMessages.Count > 50) then Break;
    MM:= GetPubMessage();
    if MM = Nil then Break;
    PublishMessage(MM);
    if MM.FQoS = 1 then
    begin
      FSendMessages.Add(MM);
    end
    else
    begin
      FreeAndNil(MM);
    end;
  until False;
end;

procedure TMQTTClient.PublishMessage(MQTTMsg: TMQTTMessage);
var
  CtrlPacket: TControlPacket;
begin
  with CtrlPacket do
  begin
    FH:= FixedHeader(mtPUBLISH, False, MQTTMsg.FQoS, MQTTMsg.FRetain);
    VH:= StrToMQTT(MQTTMsg.FTopic);
    if MQTTMsg.FQoS > 0 then
    begin
      VH:= VH + Char(FPacketId div 256) + Char(FPacketId mod 256);
      MQTTMsg.FPacketId:= FPacketId;
      GetNextPacketId;
    end;
    Payload:= MQTTMsg.FMessage;
  end;
  SendControlPacket(CtrlPacket);
  MQTTMsg.FSendFlag:= True;
end;

procedure TMQTTClient.Execute;
begin
  {$IFDEF DEBUG_MQTT}
  AddLog(LOG_DEBUG, 'TMQTTClient.Execute');
  {$ENDIF}
  //
  while not Terminated do
  begin
    try
      DoConnect;
      if (NOT FConnected) then
      begin
        Synchronize({$ifdef FPC}@{$endif FPC}DoOnError);
        exit;
      end
      else Synchronize({$ifdef FPC}@{$endif FPC}DoOnConnect);
      DoLogin;
      while not Terminated do
      begin
        if FSubscribeListChanged then DoSubscribe;
        DoReceive;
        DoSend;
        if FKeepAlive > 0 then CheckKeepAlive;
      end;
    except
      on E: MQTTException do
      begin
       {$IFDEF DEBUG_MQTT}
       AddLog(LOG_WARNING, Format('%s',[E.Message]));
       {$ENDIF}
       SetLastErrorMessage(E.Message);
       DoDisconnect(False);
       ThreadSleep(2000);
      end;
      on E: ESynapseError do
      begin
        {$IFDEF DEBUG_MQTT}
        AddLog(LOG_WARNING, Format('%s',[E.ErrorMessage]));
        {$ENDIF}
        SetLastErrorMessage(E.ErrorMessage);
        DoDisconnect(False);
        ThreadSleep(2000);
      end;
      on E: Exception do
      begin
        {$IFDEF DEBUG_MQTT}
        AddLog(LOG_ERR, Format('%s',[E.Message]));
        {$ENDIF}
        SetLastErrorMessage(E.Message);
        DoDisconnect(False);
        ThreadSleep(2000);
      end;
    end;
  end;
end;

procedure TMQTTClient.Subscribe(const ATopic: String);
var
  I: Integer;
begin
  FCritSection.Enter;
  try
    for I:= 1 to FSubscribeList.Count do
    begin
      if TSubscribeItem(FSubscribeList.Items[I-1]).FTopic = ATopic then exit;
    end;
    FSubscribeList.Add(TSubscribeItem.Create(ATopic));
    FSubscribeListChanged:= True;
  finally
    FCritSection.Leave;
  end;
end;

function TMQTTClient.GetMessage: TMQTTMessage;
begin
  FCritSection.Enter;
  try
    if (FRecMessages.Count>0) then
      result:= TMQTTMessage(FRecMessages.Pop)
    else
      result:= nil;
  finally
    FCritSection.Leave;
  end;
end;

function TMQTTClient.GetPubMessage: TMQTTMessage;
begin
  FCritSection.Enter;
  try
    if FPubMessages.Count > 0 then result:= TMQTTMessage(FPubMessages.Pop) else result:= Nil;
  finally
    FCritSection.Leave;
  end;
end;

procedure TMQTTClient.ThreadSleep(ms: Integer);
const
  SleepStep = 50;
var
  Waiting: Integer;
begin
  Waiting:= 0;
  while (not Terminated) and (Waiting < ms) do
  begin
    Waiting:= Waiting + SleepStep;
    Sleep(SleepStep);
  end;
end;

procedure TMQTTClient.Publish(const ATopic, AMessage: String; const AQoS: Byte; const ARetain: Boolean);
var
  UTF8Message: String;
begin
  if FLoggedIn or FBuffering then
  begin
    UTF8Message:= CharsetConversion(AMessage, CP1251, UTF_8);
    FCritSection.Enter;
    try
      if (FPubMessages.Count > MAXPubMessagesCount) then
      begin
        FPubMessages.Pop.Free;
        Inc(FDroppedCount);
       end;
      Inc(FPublishedCount);
      if FAddTimeStamp then
        FPubMessages.Push(TMQTTMessage.Create(ATopic, UTF8Message + UTCTimeStamp, AQoS, ARetain, 0))
      else
        FPubMessages.Push(TMQTTMessage.Create(ATopic, UTF8Message, AQoS, ARetain, 0));
    finally
      FCritSection.Leave;
    end;
  end;
end;

procedure TMQTTClient.GetStatus(var MQTTStatus: TMQTTStatus);
begin
  MQTTStatus.Connected:= FLoggedIn;
  //
  FCritSection.Enter;
  try
    MQTTStatus.InBufferCount  := FPubMessages.Count;
    MQTTStatus.WaitAckCount   := FSendMessages.Count;
    MQTTStatus.PublishedCount := FPublishedCount;
    MQTTStatus.ReceivedCount  := FReceivedCount;
    MQTTStatus.DroppedCount   := FDroppedCount;
    MQTTStatus.LastErrorMsg   := FLastErrorMessage;
  finally
    FCritSection.Leave;
  end;
end;

procedure TMQTTClient.DoOnConnect;
begin
  if Assigned(FOnConnect) then
    FOnConnect(Self);
end;

procedure TMQTTClient.DoOnDisconnect;
begin
  if Assigned(FOnDisconnect) then
    FOnDisconnect(Self);
end;

procedure TMQTTClient.DoOnError;
begin
  if Assigned(FOnError) then
    FOnError(Self, FTCP.LastError, FTCP.LastErrorDesc);
end;

{$ifdef FPC}
procedure TMQTTClient.AddLog(EventType : TEventType; const Msg : String);
begin
  FLog.Log(EventType,Msg);
end;
{$endif FPC}

end.
