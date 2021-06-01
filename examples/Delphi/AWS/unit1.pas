unit unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  mqttClient, Vcl.ExtCtrls;

type
  TForm9 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Memo1: TMemo;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    MyMQTTClient: TMQTTClient;
  public
    { Public declarations }
  end;

var
  Form9: TForm9;

implementation

{$R *.dfm}

procedure TForm9.Button1Click(Sender: TObject);
begin
  MyMQTTClient:=TMQTTClient.Create;
  MyMQTTClient.ClientId:='SOHIT_Climate_Lab1';
  MyMQTTClient.Host:='a1imtw9krxr315-ats.iot.eu-west-1.amazonaws.com';
  MyMQTTClient.Port:=8883;
  MyMQTTClient.SSLCertCAFile:=ExtractFilePath(Application.ExeName)+'Lab1Credentials\VeriSign-Class 3-Public-Primary-Certification-Authority-G5.pem';
  MyMQTTClient.SSLCertificateFile:=ExtractFilePath(Application.ExeName)+'Lab1Credentials\324ddf45b4-certificate.pem.crt';
  MyMQTTClient.SSLPrivateKeyFile:=ExtractFilePath(Application.ExeName)+'Lab1Credentials\324ddf45b4-private.pem.key';
  MyMQTTClient.SSLUse:=True;
  MyMQTTClient.AddTimeStamp:=True;
  //MyMQTTClient.Subscribe('$aws/things/'+MyMQTTClient.ClientId+'/shadow/update');
  //MyMQTTClient.Subscribe('$aws/things/'+MyMQTTClient.ClientId+'/shadow/update/accepted');
  //MyMQTTClient.Subscribe('$aws/things/'+MyMQTTClient.ClientId+'/shadow/update/rejected');
  MyMQTTClient.Start;
end;

procedure TForm9.Button2Click(Sender: TObject);
begin
  MyMQTTClient.Publish('$aws/things/'+MyMQTTClient.ClientId+'/shadow/update','hallo !!!');
end;

procedure TForm9.FormDestroy(Sender: TObject);
begin
  if Assigned(MyMQTTClient) then
  begin
    MyMQTTClient.Terminate;
    MyMQTTClient.WaitFor;
    MyMQTTClient.Destroy;
  end;
end;

procedure TForm9.Timer1Timer(Sender: TObject);
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

end.
