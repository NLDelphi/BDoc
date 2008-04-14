program BDoc;

{%ToDo 'BDoc.todo'}

uses
  Forms,
  MainFormU in 'MainFormU.pas' {MainForm},
  ProjectItemsU in 'ProjectItemsU.pas',
  ProjectDoc in 'ProjectDoc.pas',
  PageSaver in 'PageSaver.pas',
  NLDHtmlUtils in '..\NLDHTMLUtils\NLDHtmlUtils.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Bergler Documentatie';
  Application.HelpFile := 'BDoc.chm';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
