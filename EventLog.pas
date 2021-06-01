unit EventLog;

interface

type
  TEventLog = class
  private
    class procedure CheckEventLogHandle;
    class procedure Write(AEntryType: Word; AEventId: Cardinal; AMessage: string); static;
  public
    class var Source: string;
    class destructor Destroy;

    class procedure WriteInfo(AMessage: string); static;
    class procedure WriteWarning(AMessage: string); static;
    class procedure WriteError(AMessage: string); static;

    class procedure AddEventSourceToRegistry; static;
  end;

threadvar EventLogHandle: THandle;

implementation

uses Windows, Registry, SysUtils;

class destructor TEventLog.Destroy;
begin
  if EventLogHandle > 0 then
  begin
    DeregisterEventSource(EventLogHandle);
  end;
end;

class procedure TEventLog.WriteInfo(AMessage: string);
begin
  Write(EVENTLOG_INFORMATION_TYPE, 2, AMessage);
end;

class procedure TEventLog.WriteWarning(AMessage: string);
begin
  Write(EVENTLOG_WARNING_TYPE, 3, AMessage);
end;

class procedure TEventLog.WriteError(AMessage: string);
begin
  Write(EVENTLOG_ERROR_TYPE, 4, AMessage);
end;

class procedure TEventLog.CheckEventLogHandle;
begin
  if EventLogHandle = 0 then
  begin
   EventLogHandle := RegisterEventSource(nil, PChar(Source));
  end;
  if EventLogHandle <= 0 then
  begin
    raise Exception.Create('Could not obtain Event Log handle.');
  end;
end;

class procedure TEventLog.Write(AEntryType: Word; AEventId: Cardinal; AMessage: string);
begin
  CheckEventLogHandle;
  ReportEvent(EventLogHandle, AEntryType, 0, AEventId, nil, 1, 0, @AMessage, nil);
end;

// This requires admin rights. Typically called once-off during the application's installation
class procedure TEventLog.AddEventSourceToRegistry;
var
  reg: TRegistry;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    if reg.OpenKey('\SYSTEM\CurrentControlSet\Services\Eventlog\Application\' + Source, True) then
    begin
      reg.WriteString('EventMessageFile', ParamStr(0)); // The application exe's path
      reg.WriteInteger('TypesSupported', 7);
      reg.CloseKey;
    end
    else
    begin
      raise Exception.Create('Error updating the registry. This action requires administrative rights.');
    end;
  finally
    reg.Free;
  end;
end;

initialization

TEventLog.Source := 'My Application Name';

end.
