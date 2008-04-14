{ Leest informatie over Delphi uit de registry }
unit brDelphiRegU;

interface

type
  TDelphiVersion = (dv3, dv4, dv5, dv6, dv7);

function DelphiPath(DelphiVersion: TDelphiVersion): string;

implementation

uses
  Registry, Windows;

function DelphiPath(DelphiVersion: TDelphiVersion): string;
const
  SEARCH = '$(DELPHI)';
var
  DelphiRootDir: string;
  DelphiReg: string;
  iPos: integer;

  { Converteerd het path zoals dat vanaf Delphi 4 wordt gezet naar een standaard
    path. Path staat in de vorm $(DELPHI)\Lib;$(DELPHI)\Bin;$(DELPHI)\Imports }
  function ConvertPath(const Path: string): string;
  begin
    Result := Path;

    repeat
      iPos := Pos(SEARCH, Result);

      if iPos <> 0 then
      begin
        Delete(Result, iPos, length(SEARCH));
        Insert(DelphiRootDir, Result, iPos);
      end;

    until iPos = 0;

  end;

begin

  with TRegistry.Create do
  try
    RootKey := HKEY_LOCAL_MACHINE;

    DelphiReg := '';

    if DelphiVersion = dv4 then
      DelphiReg := 'Software\Borland\Delphi\4.0'
    else

    if DelphiVersion = dv5 then
      DelphiReg := 'Software\Borland\Delphi\5.0';

    if DelphiVersion = dv6 then
      DelphiReg := 'Software\Borland\Delphi\6.0';

    if DelphiVersion = dv7 then
      DelphiReg := 'Software\Borland\Delphi\7.0';

    if DelphiReg <> '' then
      if OpenKey(DelphiReg, false) then
      begin
        DelphiRootDir := ReadString('RootDir');
        CloseKey;
      end;

    RootKey := HKEY_CURRENT_USER;

    if DelphiVersion = dv3 then
    begin
      if OpenKey('Software\Borland\Delphi\3.0\Library', false) then
      begin
        Result := ReadString('SearchPath');
        CloseKey;
      end;
    end
    else
      if OpenKey(DelphiReg + '\Library', false) then
      begin
        Result := ConvertPath(ReadString('Search Path'));
        CloseKey;
      end;

  finally
    Free;
  end;

end;

end.
