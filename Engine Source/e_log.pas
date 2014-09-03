unit e_log;

interface

uses
  SysUtils;

type
  TWriteMode=(WM_NEWFILE,  WM_OLDFILE);
  TRecordCategory=(MSG_FATALERROR, MSG_WARNING, MSG_NOTIFY);

procedure e_InitLog(fFileName: String; fWriteMode: TWriteMode);
procedure e_WriteLog(TextLine: String; RecordCategory: TRecordCategory;
                     WriteTime: Boolean = True);

implementation

var
  FirstRecord: Boolean;
  FileName: String;

{ TLog }

procedure e_WriteLog(TextLine: String; RecordCategory: TRecordCategory;
                     WriteTime: Boolean = True);
var
  LogFile: TextFile;
  Prefix: ShortString;
begin
 if FileName = '' then Exit;

 Assign(LogFile, FileName);
 if FileExists(FileName) then
  Append(LogFile)
   else Rewrite(LogFile);
 if FirstRecord then
 begin
  Writeln(LogFile, '--- Log started at '+TimeToStr(GetTime)+' ---');
  FirstRecord := False;
 end;
 case RecordCategory of
  MSG_FATALERROR: Prefix := '!!!';
  MSG_WARNING:    Prefix := '!  ';
  MSG_NOTIFY:     Prefix := '***';
 end;
 if WriteTime then
  Writeln(LogFile, '['+TimeToStr(GetTime)+'] '+Prefix+' '+TextLine) else
   Writeln(LogFile, Prefix+' '+TextLine);
 Close(LogFile);
end;

procedure e_InitLog(fFileName: String; fWriteMode: TWriteMode);
begin
 FileName := fFileName;
 if fWriteMode = WM_NEWFILE then
  if FileExists(FileName) then
   DeleteFile(FileName);
 FirstRecord := True;
end;

end.
