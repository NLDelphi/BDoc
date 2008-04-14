unit MainFormU;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, JvFormPlace, JvDragDrop, JvProgressDlg,
  JvBaseDlg, JvBrowseFolder, ImgList, ActnList,
  JvRadioGroup, ExtCtrls, ToolWin, Buttons, jpeg,
  JvTimeLimit, ehshelprouter, ehsbase,
  ehswhatsthis, JvDialogs, JvComponent, JvStatusBar, JvLabel, JvHotLink;

type
  TMainForm = class(TForm)
    PageControl: TPageControl;
    SettingSheet1: TTabSheet;
    StartSheet: TTabSheet;
    ProjectsSheet: TTabSheet;
    SettingSheet2: TTabSheet;
    ExecuteSheet: TTabSheet;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    LibPathEdit: TEdit;
    OutdirEdit: TEdit;
    TitleEdit: TEdit;
    ActionList: TActionList;
    NextAction: TAction;
    PriorAction: TAction;
    StartAction: TAction;
    NewProjectCheck: TRadioButton;
    ExistProjectCheck: TRadioButton;
    Label5: TLabel;
    OpenProjectAction: TAction;
    OpenBDocDialog: TJvOpenDialog;
    ProjectLabel: TLabel;
    ProjectList: TListBox;
    ProjButtonPanel: TToolBar;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ProjAddAction: TAction;
    OpenProjectDialog: TJvOpenDialog;
    ImageList1: TImageList;
    SpeedButton1: TSpeedButton;
    ProjDelAction: TAction;
    SpeedButton2: TSpeedButton;
    ImportPathAction: TAction;
    SpeedButton3: TSpeedButton;
    OutDirSelectAction: TAction;
    JvSelectDirectory: TJvBrowseForFolderDialog;
    PropDocSelect: TRadioGroup;
    MethDocSelect: TRadioGroup;
    HTMLHelpSheet: TTabSheet;
    CompilerEdit: TEdit;
    Label2: TLabel;
    CompilerSearchAction: TAction;
    SpeedButton5: TSpeedButton;
    OpenCompilerDialog: TJvOpenDialog;
    SaveProjectAction: TAction;
    DragDrop: TJvDragDrop;
    SpeedButton4: TSpeedButton;
    JvFormPlace1: TJvFormPlace;
    Panel1: TPanel;
    Image1: TImage;
    HTMLHelpSelect: TRadioGroup;
    SaveSettingsDialog: TJvSaveDialog;
    Panel2: TPanel;
    Label6: TLabel;
    ProjectGroupCheck: TRadioButton;
    ProjectsCheck: TRadioButton;
    OpenGroupAction: TAction;
    OpenGroupDialog: TJvOpenDialog;
    SpeedButton6: TSpeedButton;
    ProjectgroupLabel: TLabel;
    ActionLabel: TLabel;
    StatusBar: TJvStatusBar;
    ProgressBar: TProgressBar;
    ToolBar1: TToolBar;
    ToolButton5: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton4: TToolButton;
    ToolButton3: TToolButton;
    WhatsThis: TWhatsThis;
    HelpRouter: THelpRouter;
    HelpButton: TToolButton;
    ToolButton8: TToolButton;
    HelpAction: TAction;
    ToolButton10: TToolButton;
    DVersionCombo: TComboBox;
    Image2: TImage;
    Image3: TImage;
    JvHotLink1: TJvHotLink;
    JvHotLink2: TJvHotLink;
    procedure JvThreadExecute(Sender: TObject; params: Pointer);
    procedure NextActionExecute(Sender: TObject);
    procedure PriorActionExecute(Sender: TObject);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure OpenProjectActionExecute(Sender: TObject);
    procedure ProjAddActionExecute(Sender: TObject);
    procedure ProjDelActionExecute(Sender: TObject);
    procedure ImportPathActionExecute(Sender: TObject);
    procedure OutDirSelectActionExecute(Sender: TObject);
    procedure CompilerSearchActionExecute(Sender: TObject);
    procedure StartActionExecute(Sender: TObject);
    procedure SaveProjectActionExecute(Sender: TObject);
    procedure OpenGroupActionExecute(Sender: TObject);
    procedure ProjectGroupCheckClick(Sender: TObject);
    procedure ProjectsCheckClick(Sender: TObject);
    procedure NewProjectCheckClick(Sender: TObject);
    procedure DragDropDrop(Sender: TObject; Pos: TPoint;
      Value: TStringList);
    procedure PageControlChange(Sender: TObject);
    procedure LibPathEditChange(Sender: TObject);
    procedure OutdirEditChange(Sender: TObject);
    procedure TitleEditChange(Sender: TObject);
    procedure PropDocSelectClick(Sender: TObject);
    procedure MethDocSelectClick(Sender: TObject);
    procedure CompilerEditChange(Sender: TObject);
    procedure HTMLHelpSelectClick(Sender: TObject);
    procedure OutdirEditExit(Sender: TObject);
    procedure HelpActionExecute(Sender: TObject);
  private
    FChanged: Boolean;
    procedure Start;
    procedure GroupLoadFile(Sender: TObject; Index: Integer);
    procedure DocSavePage(Sender: TObject);
    procedure SaveSettings(FileName: string = '');
    procedure LoadSettings(FileName: string);
    procedure FindCompiler;
    procedure CheckAssociate;
    procedure DisplayActions;
    procedure SetStatus(const Value: string);
    procedure ClearSettings;
    function DialogSaveSettings: Boolean;
    procedure AddProjectFile(const FileName: string);
  public
    property Status: string write SetStatus;

    constructor Create(AOwner: TComponent); override;
    function CloseQuery: Boolean; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

uses
  ProjectItemsU, brDelphiRegU, ProjectDoc, Registry, JclStrings,
  IniFiles, jclShell, jvFunctions;

const
  IniGeneral = 'General';
  IniProjects = 'Projects';

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;
  PageControl.ActivePageIndex := 0;
  ProjectgroupLabel.Caption := '';
  ProjectLabel.Caption := '';
  CheckAssociate;
  FindCompiler;

  if ParamCount > 0 then
  begin
    LoadSettings(ParamStr(1));
    DisplayActions;
    PageControl.ActivePage := ExecuteSheet;
  end;
end;

procedure TMainForm.Start;
var
  Group: TPrjGroup;
  ProjDoc: TProjectHTML;
  i: Integer;
  OK, Stop: Boolean;
  HelpFile, HelpInput: string;
begin
  Application.ProcessMessages;

  Group := TprjGroup.Create(nil);
  try
    LowestMethodSection := TClassSection(MethDocSelect.ItemIndex);
    LowestPropertySection := TClassSection(PropDocSelect.ItemIndex);

    Group.OnLoadFile := GroupLoadFile;
    Group.SearchPath := LibPathEdit.Text;

    if ProjectsCheck.Checked then
      for i := 0 to ProjectList.Items.Count - 1 do
        with Group.NewProject do
          FileName := ProjectList.Items[i]
    else
      Group.FileName := ProjectgroupLabel.Caption;

    Group.LoadFromFile;

    Status := 'HTML bestanden worden aangemaakt';

    ProjDoc := TProjectHTML.Create(self);
    try
      ProjDoc.OutputDir := OutdirEdit.Text;
      ProjDoc.Title := TitleEdit.Text;
      ProjDoc.LicenseName := 'Door Bergler ICT gedoneerd aan de NLDelphi community';

      ProjDoc.Group := Group;
      ProgressBar.Max := trunc(ProjDoc.FileCount * 1.1);
      ProjDoc.OnSavePage := DocSavePage;
      ProjDoc.CreateHelpFiles := HTMLHelpSelect.ItemIndex >= 1;
      ProjDoc.Execute;

      if HTMLHelpSelect.ItemIndex >= 2 then
      begin
        Stop := False;
        HelpInput := OutdirEdit.Text + '\' + TitleEdit.Text + '.hhp';
        HelpFile := ChangeFileExt(HelpInput, '.chm');

        if FileExists(HelpFile) then
        begin
          repeat
            OK := DeleteFile(HelpFile);

            if not OK then
              Stop := MessageDlg(
                HelpFile + ' kan niet worden verwijderd. '+#13+#10+
                'Nogmaals proberen?', mtInformation, [mbYes,mbNo], 0) = mrNo;
          until OK or Stop;
        end;

        if Stop then
          MessageDlg('Aanmaken documentatie is onderbroken',
            mtWarning, [mbOK], 0)
        else
        begin
          Status := 'HTML helpfile wordt gecompileerd';
          ShellExecAndWait(PChar(CompilerEdit.Text),
            ' "' + HelpInput + '"', '', SW_HIDE);

          if HTMLHelpSelect.ItemIndex >= 3 then
            ShellExec(Handle, 'Open', HelpFile, '', '', SW_SHOW);
        end;

        Status := 'Bestanden worden verwijderd';

        ProgressBar.Max := ProjDoc.PageList.Count;

        for i := 0 to ProjDoc.PageList.Count-1 do
        begin
          DeleteFile(ProjDoc.PageList[i]);
          ProgressBar.StepIt;
        end;
      end;

    finally
      ProjDoc.Free;
      Status := '';
    end;
  finally
    Group.Free;
  end;
end;

procedure TMainForm.JvThreadExecute(Sender: TObject; params: Pointer);
begin
  Start;
end;

procedure TMainForm.GroupLoadFile(Sender: TObject; Index: Integer);
begin
  Status := 'Bestanden worden ingelezen';
  Application.ProcessMessages;
  ProgressBar.Max := TPrjGroup(Sender).Projects.Count-1;
  ProgressBar.Position := Index;
end;

procedure TMainForm.NextActionExecute(Sender: TObject);
begin
  PageControl.ActivePageIndex := PageControl.ActivePageIndex + 1;
end;

procedure TMainForm.PriorActionExecute(Sender: TObject);
begin
  PageControl.ActivePageIndex := PageControl.ActivePageIndex - 1;
end;

procedure TMainForm.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  NextAction.Enabled := PageControl.ActivePageIndex < PageControl.PageCount - 1;
  PriorAction.Enabled := PageControl.ActivePageIndex > 0;
  ProjAddAction.Enabled := ProjectsCheck.Checked;
  ProjDelAction.Enabled := ProjectsCheck.Checked and (ProjectList.SelCount > 0);
  OpenProjectAction.Enabled := ExistProjectCheck.Checked;
  OpenGroupAction.Enabled := ProjectGroupCheck.Checked;
  SaveProjectAction.Enabled := FChanged;
  StartAction.Enabled :=
  ( (HTMLHelpSelect.ItemIndex = 0) or (FileExists(CompilerEdit.Text)) ) and
    (TitleEdit.Text <> '');
  CompilerEdit.Enabled := (not FileExists(CompilerEdit.Text));
  CompilerSearchAction.Enabled := (not FileExists(CompilerEdit.Text));
end;

procedure TMainForm.OpenProjectActionExecute(Sender: TObject);
begin
  if OpenBDocDialog.Execute then
    LoadSettings(OpenBDocDialog.FileName)
end;

procedure TMainForm.ProjAddActionExecute(Sender: TObject);
var
  i: Integer;
begin
  if OpenProjectDialog.Execute then
  begin
    for i := 0 to OpenProjectDialog.Files.Count-1 do
      AddProjectFile(OpenProjectDialog.Files[i]);
    FChanged := True;
  end;
end;

procedure TMainForm.ProjDelActionExecute(Sender: TObject);
var
  i: Integer;
begin
  with ProjectList do
  begin
    for i := Items.Count - 1 downto 0 do
      if Selected[i] then
        Items.Delete(i);
  end;
  FChanged := True;
end;

procedure TMainForm.ImportPathActionExecute(Sender: TObject);
begin
  LibPathEdit.Text := DelphiPath(TDelphiVersion(StrToIntDef(DVersionCombo.Text,5)-3));
end;

procedure TMainForm.OutDirSelectActionExecute(Sender: TObject);
begin
  if JvSelectDirectory.Execute then
  begin
    OutdirEdit.Text := JvSelectDirectory.Directory;
    OutdirEdit.SetFocus;
  end;
end;

procedure TMainForm.CompilerSearchActionExecute(Sender: TObject);
var
  Path: string;
begin
  OpenCompilerDialog.InitialDir := Path;

  if OpenCompilerDialog.Execute then
    CompilerEdit.Text := OpenCompilerDialog.FileName;
end;

procedure TMainForm.StartActionExecute(Sender: TObject);
begin
  ProgressBar.Visible := True;
  Screen.Cursor := crHourGlass;
  PageControl.Enabled := False;
  Application.ProcessMessages;
  try
    Start;
  finally
    PageControl.Enabled := True;
    ProgressBar.Visible := False;
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.SaveSettings(FileName: string);
var
  i: Integer;
begin
  if FileName <> '' then
    ProjectLabel.Caption := FileName;

  with TIniFile.Create(ProjectLabel.Caption) do
  try
    WriteString(IniGeneral, 'LibraryPath', LibPathEdit.Text);
    WriteString(IniGeneral, 'Outdir', OutdirEdit.Text);
    WriteString(IniGeneral, 'Title', TitleEdit.Text);
    WriteInteger(IniGeneral, 'PropDoc', PropDocSelect.ItemIndex);
    WriteInteger(IniGeneral, 'MethDoc', MethDocSelect.ItemIndex);
    WriteInteger(IniGeneral, 'HTMLHelp', HTMLHelpSelect.ItemIndex);
    WriteString(IniGeneral, 'ProjGroup', ProjectgroupLabel.Caption);

    EraseSection(IniProjects);

    for i := 0 to ProjectList.Items.Count -1 do
      WriteString(IniProjects, 'Project' + IntToStr(i), ProjectList.Items[i]);
  finally
    Free;
  end;
  FChanged := False;
end;

procedure TMainForm.SaveProjectActionExecute(Sender: TObject);
begin
  DialogSaveSettings;
end;

procedure TMainForm.ClearSettings;
begin
  ProjectLabel.Caption := '';
  ExistProjectCheck.Checked := False;

  LibPathEdit.Text := '';
  OutdirEdit.Text := '';
  TitleEdit.Text := '';
  PropDocSelect.ItemIndex := 0;
  MethDocSelect.ItemIndex := 0;
  HTMLHelpSelect.ItemIndex := 0;
  ProjectgroupLabel.Caption := '';
  ProjectList.Items.Clear;
  ProjectGroupCheck.Checked := False;
  ProjectsCheck.Checked := False;
  FChanged := False;
end;

procedure TMainForm.LoadSettings(FileName: string);
var
  i: Integer;
begin
  ProjectLabel.Caption := FileName;
  ExistProjectCheck.Checked := True;

  with TIniFile.Create(ProjectLabel.Caption) do
  try
    LibPathEdit.Text := ReadString(IniGeneral, 'LibraryPath', '');
    OutdirEdit.Text := ReadString(IniGeneral, 'Outdir', '');
    TitleEdit.Text := ReadString(IniGeneral, 'Title', '');
    PropDocSelect.ItemIndex := ReadInteger(IniGeneral, 'PropDoc', 0);
    MethDocSelect.ItemIndex := ReadInteger(IniGeneral, 'MethDoc', 0);
    HTMLHelpSelect.ItemIndex := ReadInteger(IniGeneral, 'HTMLHelp', 0);
    ProjectgroupLabel.Caption := ReadString(IniGeneral, 'ProjGroup', '');

    ProjectList.Items.Clear;

    ReadSectionValues(IniProjects, ProjectList.Items);
    for i := 0 to ProjectList.Items.Count -1 do
      ProjectList.Items[i] := StrAfter('=', ProjectList.Items[i]);
  finally
    Free;
  end;

  if ProjectgroupLabel.Caption <> '' then
    ProjectGroupCheck.Checked := True;

  if ProjectList.Items.Count > 0 then
    ProjectsCheck.Checked := True;

  FChanged := False;
  SaveSettingsDialog.FileName := FileName;
end;

procedure TMainForm.FindCompiler;
var
  Path: string;
begin
  try
    with TRegistry.Create do
    try
      if OpenKey('\Software\Microsoft\HTML Help Workshop', False) then
        Path := ReadString('InstallDir');
    finally
      Free;
    end;
  except
    Path := '';
  end;

  if FileExists(Path + '\hhc.exe') then
    CompilerEdit.Text := Path + '\hhc.exe';
end;

procedure TMainForm.OpenGroupActionExecute(Sender: TObject);
begin
  if OpenGroupDialog.Execute then
  begin
    ProjectgroupLabel.Caption := OpenGroupDialog.FileName;
    FChanged := True;
  end;
end;

procedure TMainForm.ProjectGroupCheckClick(Sender: TObject);
begin
  ProjectList.Items.Clear;
end;

procedure TMainForm.ProjectsCheckClick(Sender: TObject);
begin
  ProjectgroupLabel.Caption := '';
  FChanged := True;
end;

procedure TMainForm.NewProjectCheckClick(Sender: TObject);
begin
  ClearSettings;
end;

procedure TMainForm.CheckAssociate;

begin
  with TRegistry.Create do
  try
    OpenKey('Software\Bergler\BDoc\2', True);

    if ReadString('Path') <> Application.ExeName then
    begin
      WriteString('Path', Application.ExeName);
      AssociateExtension(Application.ExeName + ', 1', 'BDoc',
        Application.ExeName, 'bd');
    end;
  finally
    Free;
  end;
end;

procedure TMainForm.AddProjectFile(const FileName: string);
begin
  if ProjectList.Items.IndexOf(FileName) = -1 then
    ProjectList.Items.Add(FileName);
end;

procedure TMainForm.DragDropDrop(Sender: TObject; Pos: TPoint;
  Value: TStringList);
var
  i: Integer;
  Ext: string;
begin
  for i := Value.Count-1 downto 0 do
  begin
    Ext := ExtractFileExt(Value[i]);

    if (Ext = '.dpr') then
    begin
      AddProjectFile(Value[i]);
      ProjectsCheck.Checked := True;
    end
    else if (Ext = '.bpg') then
    begin
      ProjectgroupLabel.Caption := Value[i];
      ProjectGroupCheck.Checked := True;
      Break;
    end;
  end;

  PageControl.ActivePage := ProjectsSheet;
end;

procedure TMainForm.PageControlChange(Sender: TObject);
begin
  if PageControl.ActivePage = ExecuteSheet then
    DisplayActions;
end;

procedure TMainForm.DisplayActions;
var
  Text: string;
begin
  Text := '';

  if ProjectGroupCheck.Checked then
    Text := Text +
      '- Er wordt documentatie aangemaakt voor de projectgroep ' +
      ProjectgroupLabel.Caption
  else if ProjectList.Items.Count = 1 then
    Text := Text + '- Er wordt documentatie aangemaakt voor het project' +
      ProjectList.Items[0]
  else
    Text := Text + '- Er wordt documentatie aangemaakt voor diverse projecten';

  Text := Text + #13#13;

  if HTMLHelpSelect.ItemIndex = 0 then
    Text := Text + '- Er worden geen HTML help bestanden aangemaakt'
  else
  begin
    Text := Text + '- Er worden HTML help bestanden aangemaakt';
    Text := Text + #13#13;

    if HTMLHelpSelect.ItemIndex > 1 then
    begin
      Text := Text + '- De HTML help bestanden worden gecompileerd';
      Text := Text + #13#13;

      if HTMLHelpSelect.ItemIndex > 2 then
      begin
        Text := Text + '- Het helpbestand wordt na compilatie opgestart';
        Text := Text + #13#13;
      end;

      Text := Text + '- De aangemaakte HTML bestanden worden verwijderd';
    end
    else
      Text := Text + '- De aangemaakte HTML bestanden worden niet verwijderd';
  end;

  ActionLabel.Caption := Text;
end;

procedure TMainForm.SetStatus(const Value: string);
begin
  StatusBar.Panels[0].Text := Value;
  Application.ProcessMessages;
end;

procedure TMainForm.DocSavePage(Sender: TObject);
begin
  ProgressBar.StepIt;
  Application.ProcessMessages;
end;

procedure TMainForm.LibPathEditChange(Sender: TObject);
begin
  FChanged := True;
end;

procedure TMainForm.OutdirEditChange(Sender: TObject);
begin
  FChanged := True;
end;

procedure TMainForm.TitleEditChange(Sender: TObject);
begin
  FChanged := True;
end;

procedure TMainForm.PropDocSelectClick(Sender: TObject);
begin
  FChanged := True;
end;

procedure TMainForm.MethDocSelectClick(Sender: TObject);
begin
  FChanged := True;
end;

procedure TMainForm.CompilerEditChange(Sender: TObject);
begin
  FChanged := True;
end;

procedure TMainForm.HTMLHelpSelectClick(Sender: TObject);
begin
  FChanged := True;
end;

function TMainForm.CloseQuery: Boolean;
var
  UserResult: Word;
begin
  Result := inherited CloseQuery;

  if FChanged then
  begin
    UserResult := MessageDlg('Gewijzigde instellingen opslaan?',
      mtConfirmation, [mbYes,mbNo,mbCancel], 0);
    if UserResult = mrYes then
      Result := DialogSaveSettings
    else if UserResult = mrCancel then
      Result := False;
  end;
end;

function TMainForm.DialogSaveSettings: Boolean;
begin
  Result := SaveSettingsDialog.Execute;

  if Result then
    SaveSettings(SaveSettingsDialog.FileName);
end;

procedure TMainForm.OutdirEditExit(Sender: TObject);
begin
  if StrRight(OutdirEdit.Text, 1) = '\' then
    OutdirEdit.Text := StrChopRight(OutdirEdit.Text, 1);
end;

procedure TMainForm.HelpActionExecute(Sender: TObject);
begin
  HelpRouter.HelpContent;
end;

end.
