//{$i edefines.inc}

unit Tools;

interface

uses
  SysUtils;

  function GetTick:QWord;
  function TickDelta(TickOld, TickNew: QWord): QWord;
  function ProgramPathWithBundle(aPath:string): string;

implementation

function GetTick:QWord;
begin
  result:=GetTickCount64;
end;

function TickDelta(TickOld, TickNew: QWord): QWord;
begin
  Result := TickNew - TickOld;
  if ((NOT Result) < Result) then Result := (NOT Result);
end;

{$ifdef Darwin}
function ProgramPathWithBundle(aPath:string): string;
const
  BundlePostFix='.app/Contents/MacOS/';
var
  i:integer;
begin
  Result:=ExtractFilePath(aPath);
  i:=Pos(BundlePostFix,Result);
  if (i>0) then
  begin
    Delete(Result,i,MaxInt);
    Result:=ExtractFilePath(Result);
  end;
end;
{$else}
function ProgramPathWithBundle(aPath:string): string;
begin
  Result:=ExtractFilePath(aPath);
end;
{$endif}

end.
