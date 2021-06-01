program AWSMQTT;

uses
  Vcl.Forms,
  unit1 in 'unit1.pas' {Form9};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm9, Form9);
  Application.Run;
end.
