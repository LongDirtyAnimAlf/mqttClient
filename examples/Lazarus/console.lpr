program console;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  mqttClient;

var
  MyMQTTClient:TMQTTClient;
begin
  MyMQTTClient:=TMQTTClient.Create;
  //MyMQTTClient.ClientId:='SOHIT_Climate_'+LAB;
  MyMQTTClient.Host:='a1imtw9krxr315-ats.iot.eu-west-1.amazonaws.com';
  MyMQTTClient.Port:=8883;
  MyMQTTClient.Free;
end.

