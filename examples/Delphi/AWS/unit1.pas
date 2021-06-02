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
    procedure ConnectionSuccess(Sender: TObject);
  public
    { Public declarations }
  end;

var
  Form9: TForm9;

implementation

{$R *.dfm}

const
  LAB='General';

procedure TForm9.Button1Click(Sender: TObject);
begin
  MyMQTTClient:=TMQTTClient.Create;
  MyMQTTClient.ClientId:='SOHIT_Climate_'+LAB;
  MyMQTTClient.Host:='a1imtw9krxr315-ats.iot.eu-west-1.amazonaws.com';
  MyMQTTClient.Port:=8883;
  MyMQTTClient.OnConnect:=ConnectionSuccess;

  if LAB='Lab1' then
  begin
    MyMQTTClient.SSLCertCAFile:=ExtractFilePath(Application.ExeName)+LAB+'Credentials\VeriSign-Class 3-Public-Primary-Certification-Authority-G5.pem';
    MyMQTTClient.SSLCertificateFile:=ExtractFilePath(Application.ExeName)+LAB+'Credentials\324ddf45b4-certificate.pem.crt';
    MyMQTTClient.SSLPrivateKeyFile:=ExtractFilePath(Application.ExeName)+LAB+'Credentials\324ddf45b4-private.pem.key';
  end;

  if LAB='Lab3' then
  begin
    MyMQTTClient.SSLCertCAFile:=ExtractFilePath(Application.ExeName)+LAB+'Credentials\VeriSign-Class 3-Public-Primary-Certification-Authority-G5.pem';
    MyMQTTClient.SSLCertificateFile:=ExtractFilePath(Application.ExeName)+LAB+'Credentials\7423f4c7bd-certificate.pem.crt';
    MyMQTTClient.SSLPrivateKeyFile:=ExtractFilePath(Application.ExeName)+LAB+'Credentials\7423f4c7bd-private.pem.key';
  end;

  if LAB='General' then
  begin
    MyMQTTClient.ClientId:='';
    MyMQTTClient.SSLCertCAFile:=ExtractFilePath(Application.ExeName)+LAB+'Credentials\AmazonRootCA1.pem';
    MyMQTTClient.SSLCertificateFile:=ExtractFilePath(Application.ExeName)+LAB+'Credentials\9c2f98cfdc-certificate.pem.crt';
    MyMQTTClient.SSLPrivateKeyFile:=ExtractFilePath(Application.ExeName)+LAB+'Credentials\9c2f98cfdc-private.pem.key';
  end;

  MyMQTTClient.SSLUse:=True;
  MyMQTTClient.AddTimeStamp:=True;
  //MyMQTTClient.Subscribe('$aws/things/'+MyMQTTClient.ClientId+'/shadow/update');
  //MyMQTTClient.Subscribe('$aws/things/'+MyMQTTClient.ClientId+'/shadow/update/accepted');
  //MyMQTTClient.Subscribe('$aws/things/'+MyMQTTClient.ClientId+'/shadow/update/rejected');

  //MyMQTTClient.Subscribe('SOHITClimate');
  MyMQTTClient.Start;
end;

procedure TForm9.Button2Click(Sender: TObject);
begin
  //MyMQTTClient.Publish('$aws/things/'+MyMQTTClient.ClientId+'/shadow/update','hallo !!!');
  MyMQTTClient.Publish('SOHITClimate','hallo !!!');
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

procedure TForm9.ConnectionSuccess(Sender: TObject);
begin
  Memo1.Lines.Append('Connected');
end;

end.
