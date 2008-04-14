unit PageSaver;

interface

uses
  Classes, SysUtils;

type
  TPageSaver = class(TThread)
  private
    FFileName: string;
    FContent: string;
  protected
    procedure Execute; override;
  public
    constructor Create(const FileName, Content: string);
  end;

implementation

uses
  FileCtrl;

{ Important: Methods and properties of objects in VCL can only be used in a
  method called using Synchronize, for example,

      Synchronize(UpdateCaption);

  and UpdateCaption could look like,

    procedure TPageSaver.UpdateCaption;
    begin
      Form1.Caption := 'Updated in a thread';
    end; }

{ TPageSaver }

constructor TPageSaver.Create(const FileName, Content: string);
begin
  inherited Create(True);
  FFileName := FileName;
  FContent := Content;

  if FFilename = '' then
    raise Exception.Create('TmbHTMLPage: Empty filename not allowed');

  FreeOnTerminate := True;
  Resume;
end;

procedure TPageSaver.Execute;
var
  FileStream: TFileStream;
begin
  ForceDirectories(ExtractFilePath(FFilename));

  if ExtractFileExt(FFileName) = '' then
    FFileName := FFileName + '.htm';

  if FileExists(FFilename) then
    if not DeleteFile(FFilename) then
      raise Exception.Create('TmbHTMLPage: file access denied');

  FileStream := TFileStream.Create(FFileName, fmCreate);
  try
    FileStream.WriteBuffer(Pointer(FContent)^, Length(FContent));
  finally
    FileStream.Free;
  end;
end;

end.
