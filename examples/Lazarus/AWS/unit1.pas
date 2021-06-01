unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  mqttClient;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure ConnectionSuccess(Sender: TObject);
  public

  end;

var
  Form1: TForm1;
  MyMQTTClient: TMQTTClient;

implementation

uses
  ssl_openssl_lib;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  if (NOT IsSSLloaded) then InitSSLInterface;
  MyMQTTClient:=TMQTTClient.Create;
  MyMQTTClient.ClientId:='SOHIT_Climate_Lab1';
  MyMQTTClient.Host:='a1imtw9krxr315-ats.iot.eu-west-1.amazonaws.com';
  MyMQTTClient.Port:=8883;
  MyMQTTClient.SSLCertCAFile:=Application.Location+'Lab1Credentials\VeriSign-Class 3-Public-Primary-Certification-Authority-G5.pem';
  MyMQTTClient.SSLCertificateFile:=Application.Location+'Lab1Credentials\324ddf45b4-certificate.pem.crt';
  MyMQTTClient.SSLPrivateKeyFile:=Application.Location+'Lab1Credentials\324ddf45b4-private.pem.key';
  MyMQTTClient.SSLUse:=True;
  MyMQTTClient.AddTimeStamp:=True;
  MyMQTTClient.Subscribe('$aws/things/'+MyMQTTClient.ClientId+'/shadow/update');
  MyMQTTClient.Subscribe('$aws/things/'+MyMQTTClient.ClientId+'/shadow/update/accepted');
  MyMQTTClient.Subscribe('$aws/things/'+MyMQTTClient.ClientId+'/shadow/update/rejected');
  MyMQTTClient.OnConnect:=@ConnectionSuccess;
  MyMQTTClient.Start;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  MyMQTTClient.Publish('$aws/things/'+MyMQTTClient.ClientId+'/shadow/update','hallo !!!');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(MyMQTTClient) then
  begin
    MyMQTTClient.Terminate;
    MyMQTTClient.WaitFor;
    MyMQTTClient.Destroy;
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  aMessage: TMQTTMessage;
begin
  if Assigned(MyMQTTClient) then
  begin
    repeat
      aMessage:=MyMQTTClient.GetMessage;
      if Assigned(aMessage) then
      begin
        Memo1.Lines.Append(aMessage.FTopic+': '+aMessage.FMessage);
        aMessage.Free;
      end
      else break;
    until false;
  end;
end;

procedure TForm1.ConnectionSuccess(Sender: TObject);
begin
  Memo1.Lines.Append('Connected');
end;

end.

