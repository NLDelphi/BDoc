unit ProjectDoc;

interface

uses
  Classes, ProjectItemsU, NLDHtmlUtils;

type
  TProjectHTML = class(TComponent)
  private
    FClassSections: TClassSections;
    FGroup: TPrjGroup;
    FOutputDir: string;
    FTitle: string;
    FConstPage: TNLDHtmlPage;
    FProcsPage: TNLDHtmlPage;
    FUnitsPage: TNLDHtmlPage;
    FIntfPage: TNLDHtmlPage;
    FClassPage: TNLDHtmlPage;
    FTypePage: TNLDHtmlPage;
    FProjPage: TNLDHtmlPage;
    FPageList: TStringList;
    FOnSavePage: TNotifyEvent;
    FCreateHelpFiles: Boolean;
    FLicenseName: string;
    function ItemToFileName(Item: TprjItem; Prefix: string = '';
      IncludePath: Boolean = False): string;
    procedure SavePage(Page: TNLDHtmlPage; Copyright: Boolean = True);
    procedure AddIndexTable(Page: TNLDHtmlPage; Index: array of string);
    procedure AddSourceLine(Page: TNLDHtmlPage; Item: TprjItem);
    procedure AddUnitLink(Page: TNLDHtmlPage; Item: TPrjItem);
    procedure AddItemInfo(Page: TNLDHtmlPage; Item: TPrjItem);
    function ParseKeywords(Item: TprjItem; const Text: string): string;
    procedure CreateProcPage;
    procedure CreateUnitsPage;
  protected
    function NewPage(Item: TprjItem; const Title: string): TNLDHtmlPage;
    function NewLinkTable(Page: TNLDHtmlPage): TNLDHtmlTable;
    procedure AddClassesForParent(Page: TNLDHtmlPage;
                                  UnitList: TprjUnitList;
                                  const ParentName: string;
                                  ClassType: TClassType = ctClass);
    procedure AddClassTree(Par: TNLDHtmlPar; prjClass: TprjCustomClass; Level: Integer);
    procedure AddItemList(Page: TNLDHtmlPage; ItemList: TprjItemList);
    procedure AddClassItemList(Page: TNLDHtmlPage; ItemList: TPrjClassItemList;
                               Sections: TClassSections);
    procedure AddClassItemInfo(Page: TNLDHtmlPage; ClassItem: TPrjClassItem);
    procedure AddDescription(Page: TNLDHtmlPage; Item: TPrjItem);
    procedure CreateClassesPage(ClassList: TprjCustomClassList);
    procedure CreateClassPages(ClassList: TprjCustomClassList);
    procedure CreateConstantPages;
    procedure CreateConstantsPage;
    procedure CreateGroupPage;
    procedure CreateImages;
    procedure CreateMethodPage(prjMethod: TprjMethod);
    procedure CreateProjectPages;
    procedure CreatePropertyPage(prjPoperty: TprjProperty);
    procedure CreateTypePages;
    procedure CreateTypesPage;
    procedure CreateUnitPages;
    procedure CreateIndexPages;
    procedure CreateCSS;
    procedure CreateTOC;
    procedure CreateHelp;
  public
    property Group: TPrjGroup read FGroup Write FGroup;
    property CreateHelpFiles: Boolean
      read FCreateHelpFiles write FCreateHelpFiles;
    property PageList: TStringList read FPageList;

    procedure Execute;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OutputDir: string read FOutputDir Write FOutputDir;
    property LicenseName: string read FLicenseName write FLicenseName;
    property Title: string read FTitle Write FTitle;
    function FileCount: Integer;
    property OnSavePage: TNotifyEvent read FOnSavePage write FOnSavePage;
  end;

implementation

uses
{$IFDEF DEBUG} uDBG, {$ENDIF}
  SysUtils, FileCtrl, Graphics, Forms, PageSaver;

{$R BMPScopes.RES}

const
  ImageNames: array[0..4] of string =
              ( 'BMP_PRIVATE', 'BMP_PUBLIC', 'BMP_PROTECTED',
                'BMP_PUBLISHED', 'BMP_VIRTUAL');
  BMP_PRIVATE = 0;
  BMP_PUBLIC = 1;
  BMP_PROTECTED = 2;
  BMP_PUBLISHED = 3;
  BMP_VIRTUAL = 4;
  BMP_EXTENSION = '.BMP';
  FileExt = '.htm';
  cssFile = '<link rel="stylesheet" type="text/css" href="bdoc.css">';
  BR = #13#10;

{ TProjectHTML }

procedure TProjectHTML.CreateGroupPage;
var
  i: Integer;
  Table: TNLDHtmlTable;
begin
{$IFDEF DEBUG}
  Debugger.EnterProc('TProjectHTML.CreateGroupPage');
{$ENDIF}

  FProjPage := NewPage(Group, 'Projects');

  Table := FProjPage.AddTable;
  for i := 0 to Group.Projects.Count - 1 do
  begin
    with Table.AddRow do
      with AddCol do
        AddHyperlink(ItemToFileName(Group.Projects[i]),
          Group.Projects[i].ItemName);
  end;

  SavePage(FProjPage);

{$IFDEF DEBUG}
  Debugger.LeaveProc('TProjectHTML.CreateGroupPage');
{$ENDIF}
end;

procedure TProjectHTML.Execute;
begin
  if FGroup = nil then
    Exit;

  ForceDirectories(FOutputDir);

  CreateGroupPage;
  CreateProjectPages;
  CreateUnitPages;
  CreateClassPages(Group.RecursiveClassList);
  CreateClassPages(Group.RecursiveInterfaceList);
  CreateTypePages;
  CreateConstantPages;
  CreateImages;
  CreateClassesPage(Group.RecursiveClassList);
  CreateClassesPage(Group.RecursiveInterfaceList);
  CreateTypesPage;
  CreateConstantsPage;
  CreateIndexPages;
  CreateCSS;
  CreateTOC;

  if FCreateHelpFiles then
    CreateHelp;
end;
                                                                             
function TProjectHTML.NewPage(Item: TprjItem;
                              const Title: string): TNLDHtmlPage;
var
  i: Integer;
begin
  Result := TNLDHtmlPage.Create(nil);

  if Item is TPrjGroup then
    Result.Filename := ItemToFileName(Item, Title, True)
  else
    Result.Filename := ItemToFileName(Item, '', True);

  Result.Title := Title;

  Result.Head.AddText('<meta name="GENERATOR" content="Bergler Documentation">');
  Result.Head.AddText(cssFile);

  with Result.Head.AddObject do
  begin
    ObjectType := 'application/x-oleobject';
    ClassID := 'clsid:1e2a7bd0-dab9-11d0-b93a-00c04fc99f9e';
    AddParam('Keyword', Item.ItemName);
    for i := 0 to Item.Keywords.Count-1 do
      AddParam('Keyword', Item.Keywords[i].Keyword);
  end;

  with Result.Body.AddPar do
  begin
    CssClass := 'topbar';
    with AddTable, AddRow do
    begin
      with AddCol do
        AddText(FTitle);
    end;
  end;

  with Result.Body.AddPar do
  begin
    CssClass := 'header';
    AddText(Title);
  end;

{ TODO : Indexpagina uitbreiden }
{
    with FIndexPage.AddList do
      with AddListItem, AddObject do
      begin
        ObjectType := 'text/sitemap';
        AddParam('Name', Item.ItemName);
        AddParam('Local', Result.Filename);
      end;
}
end;

function TProjectHTML.ItemToFileName(Item: TprjItem;
  Prefix: string = ''; IncludePath: Boolean = False): string;
begin
  if IncludePath then
    Result := Format('%0:s\%3:s%1:.6d%2:s',
      [FOutputDir, Item.Tag, FileExt, Prefix])
  else
    Result := Format('%2:s%0:.6d%1:s', [Item.Tag, FileExt, Prefix]);
end;

procedure TProjectHTML.SavePage(Page: TNLDHtmlPage; Copyright: Boolean = True);
begin
  FPageList.Add(Page.FileName);

  if Copyright then
    if Page.Body.Text <> '' then
      with Page.Body.AddPar do
      begin
        ExtraParams := 'align="right" style="font-size: 8pt"';
        AddLine('BDoc licentie: ' + FLicenseName);
      end;

  Page.Save;

  if Assigned(FOnSavePage) then
    FOnSavePage(Self);
end;

procedure TProjectHTML.CreateProjectPages;
var
  i: Integer;
  Page: TNLDHtmlPage;
  Project: TprjProject;
begin
{$IFDEF DEBUG}
  Debugger.EnterProc('TProjectHTML.CreateProjectPages');
{$ENDIF}

  for i := 0 to Group.Projects.Count - 1 do
  begin
    Project := Group.Projects[i];

    Page := NewPage(Project, Project.ItemName);

    AddIndexTable(Page, ['Forms', 'Frames', 'DataModules', 'Uses']);

    AddDescription(Page, Project);

    with Page.Body.AddAnchor do
      Text := 'Forms';

    with Page.Body.AddPar do
    begin
      CssClass := 'chapter';
      AddText('Forms');
    end;

    AddClassesForParent(Page, Project.RecursiveUnitList, 'TForm');

    with Page.Body.AddAnchor do
      Text := 'Frames';

    with Page.Body.AddPar do
    begin
      CssClass := 'chapter';
      AddText('Frames');
    end;

    AddClassesForParent(Page, Project.RecursiveUnitList, 'TFrame');

    with Page.Body.AddAnchor do
      Text := 'Datamodules';

    with Page.Body.AddPar do
    begin
      CssClass := 'chapter';
      AddText('Datamodules');
    end;

    AddClassesForParent(Page, Project.RecursiveUnitList, 'TDataModule');

    with Page.Body.AddAnchor do
      Text := 'Uses';

    with Page.Body.AddPar do
    begin
      CssClass := 'chapter';
      AddText('Uses');
      AddText(' (recursive)' );
    end;

    AddItemList(Page, Project.RecursiveUnitList);

    SavePage(Page);
  end;

{$IFDEF DEBUG}
  Debugger.LeaveProc('TProjectHTML.CreateProjectPages');
{$ENDIF}
end;

function TProjectHTML.NewLinkTable(Page: TNLDHtmlPage): TNLDHtmlTable;
begin
  Result := Page.Body.AddTable;
  Result.CssClass := 'linktable';
end;

procedure TProjectHTML.AddClassesForParent(Page: TNLDHtmlPage;
                                           UnitList: TprjUnitList;
                                           const ParentName: string;
                                           ClassType: TClassType = ctClass);
var
  UnitIndex: Integer;
  ClassIndex: Integer;
  prjUnit: TprjUnit;
  prjClass: TprjCustomClass;
  ClassList: TprjCustomClassList;
  Par: TNLDHtmlPar;
  Found: Boolean;
begin
  UnitList.Sort;

  Par := Page.Body.AddPar;
  Par.AddLine(ParentName);
  Found := False;

  for UnitIndex := 0 to UnitList.Count-1 do
  begin
    prjUnit := UnitList[UnitIndex];

    if ClassType = ctClass then
      ClassList := prjUnit.Classes
    else
      ClassList := prjUnit.Interfaces;

    for ClassIndex := 0 to ClassList.Count-1 do
    begin
      prjClass := ClassList[ClassIndex];

      if prjClass.InheritsFrom(ParentName) then
      begin
        AddClassTree(Par, prjClass, 1);
        Found := True;
      end;
    end;
  end;

  if not Found then
    Par.Text := '(none)';
end;

procedure TProjectHTML.AddClassTree(Par: TNLDHtmlPar; prjClass: TprjCustomClass;
  Level: Integer);

  function LevelString(Level: integer): string;
  var
    iIndex: integer;
  begin
    Result := '';

    for iIndex := 0 to Level-2 do
      Result := Result + '&nbsp;&nbsp;|&nbsp;&nbsp;';

    if Level > 0 then
      Result := Result + '&nbsp;&nbsp;|——';

  end;

var
  i: Integer;
begin
  Par.CssClass := 'classtree';
  Par.AddText(LevelString(Level));
  Par.AddHyperlink(ItemToFileName(prjClass), prjClass.ItemName);
  Par.AddLine('');

  for i := 0 to prjClass.ChildClasses.Count-1 do
    AddClassTree(Par, prjClass.ChildClasses[i], Level+1);
end;

procedure TProjectHTML.AddItemList(Page: TNLDHtmlPage;
   ItemList: TprjItemList);
var
  i: Integer;
  Table: TNLDHtmlTable;
begin
  if ItemList.Count = 0 then
  begin
    with Page.Body.AddPar do
      AddText('(none)');
    Exit;
  end;

  Table := NewLinkTable(Page);
  ItemList.Sort;

  for i := 0 to ItemList.Count - 1 do
  begin
    with Table.AddRow do
      with AddCol do
        AddHyperlink(ItemToFileName(ItemList[i]), ItemList[i].ItemName);
  end;
end;

procedure TProjectHTML.CreateUnitPages;
var
  MethodIndex: Integer;
  i: Integer;
  Page: TNLDHtmlPage;
  prjUnit: TprjUnit;

begin
{$IFDEF DEBUG}
  Debugger.EnterProc('TProjectHTML.CreateUnitPages');
  Debugger.LogInteger('Aantal units: ', Group.Units.Count);
{$ENDIF}

  for i := 0 to Group.Units.Count - 1 do
  begin
    prjUnit := Group.Units[i];
    Page := NewPage(prjUnit, prjUnit.ItemName);
    try
      AddIndexTable(Page, ['Classes', 'Interfaces', 'Types', 'Constants',
                           'Procedures', 'Uses', 'Used by', 'Used in projects']);

      AddDescription(Page, prjUnit);

      Page.Body.AddAnchorText('Classes', 'chapter');
      AddItemList(Page, prjUnit.Classes);

      Page.Body.AddAnchorText('Interfaces', 'chapter');
      AddItemList(Page, prjUnit.Interfaces);

      Page.Body.AddAnchorText('Types', 'chapter');
      AddItemList(Page, prjUnit.Types);

      Page.Body.AddAnchorText('Constants', 'chapter');
      AddItemList(Page, prjUnit.Constants);

      Page.Body.AddAnchorText('Procedures', 'chapter');
      AddItemList(Page, prjUnit.Procedures);

      Page.Body.AddAnchorText('Uses', 'chapter');
      AddItemList(Page, prjUnit.Units);

      Page.Body.AddAnchorText('Used by', 'chapter');
      AddItemList(Page, prjUnit.UsedBy);

      Page.Body.AddAnchorText('Used in projects', 'chapter');
      AddItemList(Page, prjUnit.UsedByProjects);

      SavePage(Page);
    finally
      Page.Free;
    end;

    for MethodIndex := 0 to prjUnit.Procedures.Count - 1 do
      CreateMethodPage(prjUnit.Procedures[MethodIndex]);

  end;
{$IFDEF DEBUG}
  Debugger.LeaveProc('TProjectHTML.CreateUnitPages');
{$ENDIF}
end;

procedure TProjectHTML.AddIndexTable(Page: TNLDHtmlPage; Index: array of string);
var
  i: Integer;
  Table: TNLDHtmlTable;
begin
  Table := NewLinkTable(Page);
  with Table.AddRow do
    for i := 0 to Length(Index) - 1 do
      with AddCol do
        AddHyperlink('#' + Index[i], Index[i]);
end;

procedure TProjectHTML.CreateTypePages;
var
  UnitIndex: Integer;
  TypeIndex: Integer;
  PrjUnit: TprjUnit;
  Page: TNLDHtmlPage;
  PrjType: TprjType;
begin
{$IFDEF DEBUG}
  Debugger.EnterProc('TProjectHTML.CreateTypePages');
{$ENDIF}

  for UnitIndex := 0 to Group.RecursiveUnitList.Count - 1 do
  begin
    PrjUnit := Group.RecursiveUnitList[UnitIndex];

    for TypeIndex := 0 to PrjUnit.Types.Count-1 do
    begin
      PrjType := PrjUnit.Types[TypeIndex];

      Page := NewPage(PrjType, PrjType.ItemName);
      try
        AddItemInfo(Page, PrjType);

        SavePage(Page);
      finally
        Page.Free;
      end;
    end;

  end;
{$IFDEF DEBUG}
  Debugger.LeaveProc('TProjectHTML.CreateTypePages');
{$ENDIF}
end;

procedure TProjectHTML.CreateConstantPages;
var
  UnitIndex: Integer;
  ConstantIndex: Integer;
  PrjUnit: TprjUnit;
  Page: TNLDHtmlPage;
  PrjConstant: TprjConstant;
begin
{$IFDEF DEBUG}
  Debugger.EnterProc('TProjectHTML.CreateConstantPages');
{$ENDIF}

  for UnitIndex := 0 to Group.RecursiveUnitList.Count - 1 do
  begin
    PrjUnit := Group.RecursiveUnitList[UnitIndex];

    for ConstantIndex := 0 to PrjUnit.Constants.Count-1 do
    begin
      PrjConstant := PrjUnit.Constants[ConstantIndex];

      Page := NewPage(PrjConstant, PrjConstant.ItemName);
      try
        AddItemInfo(Page, PrjConstant);

        SavePage(Page);
      finally
        Page.Free;
      end;
    end;
  end;
{$IFDEF DEBUG}
  Debugger.LeaveProc('TProjectHTML.CreateConstantPages');
{$ENDIF}
end;


procedure TProjectHTML.CreateClassPages(ClassList: TprjCustomClassList);

  type
    TDerivedType = (dtMethods, dtProperties);

  procedure AddDerived(Page: TNLDHtmlPage; prjClass: TprjCustomClass; DerivedType: TDerivedType);
  var
    ParentClass: TprjCustomClass;
  begin
    ParentClass := prjClass.ParentClass;

    if ParentClass <> nil then
    begin
      with Page.Body.AddPar do
      begin
        CssClass := 'subchapter';
        AddText('Derived from ');
        AddHyperlink(ItemToFileName(ParentClass), ParentClass.ItemName);
        AddLine('');
      end;

      if DerivedType = dtMethods then
        AddClassItemList(Page, ParentClass.Methods, FClassSections - [csPrivate])
      else
        AddClassItemList(Page, ParentClass.Properties, FClassSections - [csPrivate]);

      AddDerived(Page, ParentClass, DerivedType);
    end;
  end;


  procedure AddHierarchy(Page: TNLDHtmlPage; prjClass: TprjCustomClass);
  var
     Table: TNLDHtmlTable;
     i: Integer;
     ClassList: TList;
  begin
    Table := NewLinkTable(Page);

    ClassList := TList.Create;
    try
      repeat
        ClassList.Add(prjClass);
        prjClass := prjClass.ParentClass;
      until prjClass = nil;

      with Table.AddRow, AddCol do
        AddText(TprjCustomClass(ClassList[ClassList.Count-1]).ParentName);

      for i := ClassList.Count-1 downto 1 do
      begin
        prjClass := TprjCustomClass(ClassList[i]);

        with Table.AddRow do
          with AddCol do
            AddHyperlink(ItemToFileName(prjClass), prjClass.ItemName);
      end;
    finally
      ClassList.Free;
    end;
  end;

var
  I: Integer;
  ClassIndex: Integer;
  Page: TNLDHtmlPage;
  prjClass: TprjCustomClass;
begin
{$IFDEF DEBUG}
  Debugger.EnterProc('TProjectHTML.CreateClassPages');
{$ENDIF}

  for ClassIndex := 0 to ClassList.Count - 1 do
  begin
    prjClass := ClassList[ClassIndex];

    Page := NewPage(prjClass, prjClass.ItemName);
    try
      if prjClass is TprjClass then
        AddIndexTable(Page, ['Hierarchy', 'Properties', 'Methods', 'Interfaces'])
      else
        AddIndexTable(Page, ['Hierarchy', 'Properties', 'Methods', 'Classes']);

      AddItemInfo(Page, prjClass);

      Page.Body.AddAnchorText('Properties', 'chapter');
      AddClassItemList(Page, prjClass.Properties, FClassSections);
      AddDerived(Page, prjClass, dtProperties);

      Page.Body.AddAnchorText('Methods', 'chapter');
      AddClassItemList(Page, prjClass.Methods, FClassSections);
      AddDerived(Page, prjClass, dtMethods);

      if prjClass is TprjClass then
      begin
        Page.Body.AddAnchorText('Interfaces', 'chapter');
        AddItemList(Page, TprjClass(prjClass).Interfaces);
      end
      else
        if prjClass is TprjInterface then
        begin
          Page.Body.AddAnchorText('Classes', 'chapter');
          AddItemList(Page, TprjInterface(prjClass).Classes);
        end;

      Page.Body.AddAnchorText('Hierarchy', 'chapter');
      AddHierarchy(Page, prjClass);

      SavePage(Page);
    finally
      Page.Free;
    end;

    for I := 0 to prjClass.Methods.Count - 1 do
      CreateMethodPage(prjClass.Methods[i]);

    for I := 0 to prjClass.Properties.Count - 1 do
      CreatePropertyPage(prjClass.Properties[i]);
  end;
{$IFDEF DEBUG}
  Debugger.LeaveProc('TProjectHTML.CreateClassPages');
{$ENDIF}
end;

procedure TProjectHTML.AddClassItemList(Page: TNLDHtmlPage;
  ItemList: TPrjClassItemList; Sections: TClassSections);

  function ImageName(Section: TClassSection): string;
  var
    i: Integer;
  begin
    case Section of
      csPrivate: i := BMP_PRIVATE;
      csProtected: i := BMP_PROTECTED;
      csPublic: i := BMP_PUBLIC;
      csPublished: i := BMP_PUBLISHED;
    else
      i := -1;
    end;

    if i = -1 then
      Result := ''
    else
      Result := FOutputDir + '\' +
                ImageNames[i] + '.bmp';
  end;

var
  i: Integer;
  Table: TNLDHtmlTable;
begin

  if ItemList.Count = 0 then
  begin
    with Page.Body.AddPar do
      AddText('(none)');
    Exit;
  end;

  Table := NewLinkTable(Page);
  ItemList.Sort;

  for i := 0 to ItemList.Count - 1 do
  begin
    if TprjClass(ItemList[i]).ClassSecion in Sections then
    begin
      with Table.AddRow do
        with AddCol do
        begin
          AddImage(ImageName(TprjClass(ItemList[i]).ClassSecion));
          AddHyperlink(ItemToFileName(ItemList[i]), ItemList[i].ItemName);

          if ItemList[i] is TprjMethod then
            if TprjMethod(ItemList[i]).IsVirtual then
              AddImage(ImageNames[BMP_VIRTUAL] + BMP_EXTENSION);
        end;
    end;
  end;
end;

procedure TProjectHTML.CreateImages;
var
  FileName: String;
  i: integer;
begin
{$IFDEF DEBUG}
  Debugger.EnterProc('TProjectHTML.CreateImages');
{$ENDIF}

  with TBitmap.Create do
  try

    for i := 0 to High(ImageNames) do
    begin
      LoadFromResourceName(HInstance, ImageNames[i]);
      FileName := FOutputDir + '\' + ImageNames[i] + BMP_EXTENSION;
      SaveToFile(FileName);
      FPageList.Add(FileName);
    end;
  finally
    Free;
  end;

{$IFDEF DEBUG}
  Debugger.LeaveProc('TProjectHTML.CreateImages');
{$ENDIF}
end;


constructor TProjectHTML.Create(AOwner: TComponent);
begin
  inherited;
  FClassSections := [csPrivate, csProtected,
                   csPublic, csPublished, csUndefined];
  FPageList := TStringList.Create;
  FCreateHelpFiles := True;
  FLicenseName := 'Demonstratieversie';
end;

procedure TProjectHTML.CreateMethodPage(prjMethod: TprjMethod);
var
  Page: TNLDHtmlPage;
  Title: string;
begin

  if prjMethod.prjClass is TprjDummyClass then
    Title := prjMethod.ItemName
  else
    Title := prjMethod.prjClass.ItemName + '.' + prjMethod.ItemName;

  Page := NewPage(prjMethod, Title);
  try
    AddClassItemInfo(Page, prjMethod);

    SavePage(Page);
  finally
    Page.Free;
  end;

end;

procedure TProjectHTML.CreatePropertyPage(prjPoperty: TprjProperty);
var
  Page: TNLDHtmlPage;
begin
  Page := NewPage(prjPoperty,
    prjPoperty.prjClass.ItemName + '.' + prjPoperty.ItemName);
  try
    AddClassItemInfo(Page, prjPoperty);

    SavePage(Page);
  finally
    Page.Free;
  end;
end;


procedure TProjectHTML.AddClassItemInfo(Page: TNLDHtmlPage;
  ClassItem: TPrjClassItem);
var
  Par: TNLDHtmlPar;
begin
  Par := Page.Body.AddPar;
  Par.CssClass := 'subchapter';

  if ClassItem.prjClass is TprjDummyClass then
  begin
    Par.AddHyperlink(ItemToFileName(ClassItem.prjClass.prjFile),
      ClassItem.prjClass.prjFile.ItemName);
  end
  else
  begin
    Par.AddHyperlink(ItemToFileName(ClassItem.prjClass),
      ClassItem.prjClass.ItemName);
    Par.AddText(' (' + ClassSectionText(ClassItem.ClassSecion) + ')');
  end;

  AddSourceLine(Page, ClassItem);
  AddDescription(Page, ClassItem);
end;

procedure TProjectHTML.CreateClassesPage(ClassList: TprjCustomClassList);
var
  i: Integer;
  Page: TNLDHtmlPage;
  Done: TStringList;
  prjClass: TprjCustomClass;
  ClassType: TClassType;
  Par: TNLDHtmlPar;
begin
  if ClassList is TprjInterfaceList  then
  begin
    ClassType := ctInterface;
    FIntfPage := NewPage(Group, 'Interfaces');
    Page := FIntfPage;
  end
  else
  begin
    FClassPage := NewPage(Group, 'Classes');

    Page := FClassPage;
    ClassType := ctClass;
  end;

  Done := TStringList.Create;
  try
    Par := Page.Body.AddPar;
    Par.Preformatted := True;

    ClassList.Sort;

    for i := 0 to ClassList.Count - 1 do
    begin
      prjClass := ClassList[i];
      if (prjClass.ParentClass = nil) and
         (Done.IndexOf(prjClass.ParentName) = -1) then
      begin
        AddClassesForParent(Page, Group.Units, prjClass.ParentName, ClassType);
        Done.Add(prjClass.ParentName);
      end;
    end;

    Par.PreFormatted := False;

    SavePage(Page);
  finally
    Done.Free;
  end;
end;

function ReplaceWord(S, Delims, Search, Replace: string): string;
var
  i: Integer;
  Word: string;
  LastStart: Integer;
begin
  Result := S;
  LastStart := 1;
  i := 1;

  while i <= Length(Result) do
  begin
    if Pos(Result[i], Delims) > 0 then
    begin
      if SameText(Word, Search) then
      begin
        if (LastStart >= 1) and (Result[LastStart] = '[') then
        begin
          Delete(Result, LastStart, 1);
          Dec(LastStart);
        end;

        if ((i + 1) <= Length(Result)) and (Result[i] = ']') then
          Delete(Result, i, 1);

        Delete(Result, LastStart + 1, i - LastStart - 1);
        Insert(Replace, Result, LastStart + 1);
      end;
      LastStart := i;
      Word := '';
    end
    else
      Word := Word + Result[i];

    Inc(i);
  end;

end;

function TProjectHTML.ParseKeywords(Item: TprjItem;
  const Text: string): string;
var
  i: Integer;
  Link: string;
const
  Delims = '; ()[],';
begin
  Result := Text;

  Item.Keywords.Sort(SortFunction);

  for i := 0 to Item.Keywords.Count - 1 do
  begin
    if Item.Keywords[i].Item <> nil then
    begin
      Link := Hyperlink(ItemToFileName(Item.Keywords[i].Item),
        Item.Keywords[i].Keyword);
      Result := ReplaceWord(Result, Delims, Item.Keywords[i].Keyword, Link);
    end;
    Result := StringReplace(Result, '[' + Item.Keywords[i].Keyword + ']',
      Item.Keywords[i].Keyword, [rfReplaceAll, rfIgnoreCase]);
  end;

end;

procedure TProjectHTML.AddDescription(Page: TNLDHtmlPage; Item: TPrjItem);
var
  i: Integer;
  Text: TStringList;
  Par: TNLDHtmlPar;
begin
  with Page.Body.AddPar do
  begin
    CssClass := 'chapter';
    AddText('Description');
  end;

  Par := Page.Body.AddPar;
  Par.CssClass := 'description';
  Par.Preformatted := True;

  Text := TStringList.Create;
  try
    Text.AddStrings(Item.Description);

    Text.Text := ParseKeywords(Item, Text.Text);

    for i := 0 to Text.Count - 1 do
      Par.AddLine(Text[i]);
{
DEBUG

    Par := Page.Body.AddPar;
    Par.CssClass := 'description';
    Par.AddLine('Keywords:');
    for i := 0 to Item.Keywords.Count - 1 do
      Par.AddLine(Item.Keywords[i].Keyword);
}
  finally
    Text.Free;
  end;

end;

procedure TProjectHTML.CreateTypesPage;
begin
{$IFDEF DEBUG}
  Debugger.EnterProc('TProjectHTML.CreateTypesPage');
{$ENDIF}

  FTypePage := NewPage(Group, 'Types');
  Group.RecursiveTypeList.Sort;
  AddItemList(FTypePage, Group.RecursiveTypeList);
  SavePage(FTypePage);

{$IFDEF DEBUG}
  Debugger.LeaveProc('TProjectHTML.CreateTypesPage');
{$ENDIF}
end;

procedure TProjectHTML.CreateConstantsPage;
var
  ConstantsList: TprjConstantList;
  UnitIndex: Integer;
  ConstantIndex: Integer;
begin
{$IFDEF DEBUG}
  Debugger.EnterProc('TProjectHTML.CreateConstantsPage');
{$ENDIF}

  ConstantsList := TprjConstantList.Create(Group, False);
  try
    for UnitIndex := 0 to Group.RecursiveUnitList.Count-1 do
      for ConstantIndex := 0 to Group.RecursiveUnitList[UnitIndex].Constants.Count-1 do
        ConstantsList.Add(Group.RecursiveUnitList[UnitIndex].Constants[ConstantIndex]);

    FConstPage := NewPage(Group, 'Constants');
    AddItemList(FConstPage, ConstantsList);

    SavePage(FConstPage);
  finally
    ConstantsList.Free;
  end;
{$IFDEF DEBUG}
  Debugger.LeaveProc('TProjectHTML.CreateConstantsPage');
{$ENDIF}
end;

procedure TProjectHTML.AddSourceLine(Page: TNLDHtmlPage; Item: TprjItem);
begin
  with Page.Body.AddPar do
  begin
    CssClass := 'code';
    PreFormatted := True;

    AddLine(ParseKeywords(Item, Item.SourceLine));
  end;
end;

procedure TProjectHTML.AddUnitLink(Page: TNLDHtmlPage; Item: TPrjItem);
begin
  with Page.Body.AddPar do
  begin
    CssClass := 'chapter';
    AddText('Unit');
  end;

  with Page.Body.AddPar do
  begin
    CssClass := 'subchapter';
    AddHyperlink(ItemToFileName(Item.prjFile), Item.prjFile.ItemName);
  end;
end;

procedure TProjectHTML.AddItemInfo(Page: TNLDHtmlPage; Item: TPrjItem);
begin
  AddSourceLine(Page, Item);
  AddUnitLink(Page, Item);
  AddDescription(Page, Item);
end;

procedure TProjectHTML.CreateIndexPages;
var
  Page: TNLDHtmlPage;
begin
{$IFDEF DEBUG}
  Debugger.EnterProc('TProjectHTML.CreateIndexPages');
{$ENDIF}

  Page := TNLDHtmlPage.Create(nil);
  try
    Page.Title := FTitle;
    Page.AddFrame('contents', 'main', FTitle + 'left' + FileExt, '0');
    Page.AddFrame('main', '', Format('%sstart.htm', [FTitle]));

    Page.FileName := FOutputDir + '\' + 'Index.htm';
    SavePage(Page);
  finally
    Page.Free;
  end;

  Page := TNLDHtmlPage.Create(nil);
  try
    Page.Head.AddText(cssFile);

    with Page.Body.AddPar do
    begin
      AddText('<OBJECT id=hhctrl type="application/x-oleobject"');
      AddText('        classid="clsid:adb880a6-d8ff-11cf-9377-00aa003b7a11"');
      AddText('        codebase="hhctrl.ocx#Version=4,74,8793,0"');
      AddText('        width=100%');
      AddText('        height=100%>');
      AddText('    <PARAM name="Command" value="Contents">');
      AddText(Format('    <PARAM name="Item1" value="%stoc.hhc">', [FTitle]));
      AddText('</OBJECT>');
    end;

    Page.FileName := FOutputDir + '\' + FTitle + 'Left.htm';

    SavePage(Page, False);
  finally
    Page.Free;
  end;
{$IFDEF DEBUG}
  Debugger.LeaveProc('TProjectHTML.CreateIndexPages');
{$ENDIF}
end;

destructor TProjectHTML.Destroy;
begin
  FConstPage.Free;
  FIntfPage.Free;
  FProcsPage.Free;
  FUnitsPage.Free;
  FClassPage.free;
  FTypePage.Free;
  FProjPage.Free;
  FPageList.Free;
  inherited;
end;

procedure TProjectHTML.CreateCSS;
var
  Css: TStrings;
begin
  Css := TStringList.Create;
  try
    if FileExists(ExtractFilePath(Application.ExeName) + 'BDoc.css') then
    begin
      Css.LoadFromFile(ExtractFilePath(Application.ExeName) + 'BDoc.css');
      Css.SaveToFile(FOutputDir + '\BDoc.css');
      FPageList.Add(FOutputDir + '\BDoc.css');
    end;
  finally
    Css.Free;
  end;
end;

procedure TProjectHTML.CreateTOC;

  procedure AddClass(List: TNLDHtmlList; prjClass: TprjCustomClass);
  var
    i: Integer;
  begin
    with List.AddListItem do
      AddSiteMap(prjClass.ItemName, ItemToFileName(prjClass), 'main');

    for i := 0 to prjClass.ChildClasses.Count - 1 do
      AddClass(List.AddList, prjClass.ChildClasses[i]);
  end;
var
  Page: TNLDHtmlPage;
  i, j: Integer;
  List: TNLDHtmlList;
  BaseClass: TprjCustomClass;
  BaseClasses: TStringList;
begin
{$IFDEF DEBUG}
  Debugger.EnterProc('TProjectHTML.CreateTOC');
{$ENDIF}

  Page := TNLDHtmlPage.Create(nil);
  try
    Page.FileName := FOutputDir + '\' + FTitle + 'toc.hhc';

    with Page.Body.AddObject do
    begin
      ObjectType := 'text/site properties';
      AddParam('ImageType', 'Folder');
      AddParam('FrameName', 'main');
    end;

    with Page.Body.AddList do
    begin
      with AddListItem do
        AddSiteMap('Start', Format('%sstart.htm', [FTitle]), 'main');

      with AddListItem do
        AddSiteMap('Projects', ExtractFileName(FProjPage.FileName), 'main');

      with AddList do
        for i := 0 to Group.Projects.Count - 1 do
          with AddListItem do
            AddSiteMap(Group.Projects[i].ItemName,
              ItemToFileName(Group.Projects[i]), 'main');
    end;

    with Page.Body.AddList do
    begin
      with AddListItem do
        AddSiteMap('Classes', ExtractFileName(FClassPage.FileName), 'main');

      BaseClasses := TStringList.Create;
      try
        { Eerst lijst aanmaken van basisclasses, dus roots }
        for i := 0 to Group.RecursiveClassList.Count - 1 do
          if Group.RecursiveClassList[i].ParentClass = nil then
            if BaseClasses.IndexOf(
              Group.RecursiveClassList[i].ParentName) = -1 then
              BaseClasses.Add(Group.RecursiveClassList[i].ParentName);

        { Voor iedere root de branches maken }
        for i := 0 to BaseClasses.Count-1 do
        begin
          List := AddList;
          with List.AddListItem do
            AddSiteMap(BaseClasses[i], '', 'main');

          for j := 0 to Group.RecursiveClassList.Count - 1 do
            if Group.RecursiveClassList[j].InheritsFrom(BaseClasses[i]) then
              AddClass(List.AddList, Group.RecursiveClassList[j]);
        end;
      finally
        BaseClasses.Free;
      end;
    end;

    with Page.Body.AddList do
    begin
      with AddListItem do
        AddSiteMap('Interfaces', ExtractFileName(FIntfPage.FileName), 'main');

      List := AddList;
      with List do
        for i := 0 to Group.RecursiveInterfaceList.Count - 1 do
        begin
          BaseClass := Group.RecursiveInterfaceList[i];

          if BaseClass.ParentClass = nil then
            AddClass(List, BaseClass);
        end;
    end;

    Group.RecursiveProcedureList.Sort;
    CreateProcPage;

    with Page.Body.AddList do
    begin
      with AddListItem do
        AddSiteMap('Procedures', ExtractFileName(FProcsPage.FileName), 'main');

      List := AddList;
      with List do
        for i := 0 to Group.RecursiveProcedureList.Count - 1 do
        begin
          with AddListItem do
            AddSiteMap(Group.RecursiveProcedureList[i].ItemName,
              ItemToFileName(Group.RecursiveProcedureList[i]), 'main');
        end;
    end;

    with Page.Body.AddList do
    begin
      with AddListItem do
        AddSiteMap('Types', ExtractFileName(FTypePage.FileName), 'main');

      List := AddList;
      with List do
        for i := 0 to Group.RecursiveTypeList.Count - 1 do
        begin
          with AddListItem do
            AddSiteMap(Group.RecursiveTypeList[i].ItemName,
              ItemToFileName(Group.RecursiveTypeList[i]), 'main');
        end;
    end;

    Group.RecursiveUnitList.Sort;
    CreateUnitsPage;
    with Page.Body.AddList do
    begin
      with AddListItem do
        AddSiteMap('Units', ExtractFileName(FUnitsPage.FileName), 'main');

      List := AddList;
      with List do
        for i := 0 to Group.RecursiveUnitList.Count - 1 do
        begin
          with AddListItem do
            AddSiteMap( Group.RecursiveUnitList[i].ItemName,
              ItemToFileName(Group.RecursiveUnitList[i]), 'main');
        end;
    end;

    SavePage(Page, False);
  finally
    Page.Free;
  end;
{$IFDEF DEBUG}
  Debugger.LeaveProc('TProjectHTML.CreateTOC');
{$ENDIF}
end;

procedure TProjectHTML.CreateUnitsPage;
begin
{$IFDEF DEBUG}
  Debugger.EnterProc('TProjectHTML.CreateUnitsPage');
{$ENDIF}

  FUnitsPage := NewPage(Group, 'Units');

  AddItemList(FUnitsPage, Group.RecursiveUnitList);

  SavePage(FUnitsPage);
{$IFDEF DEBUG}
  Debugger.LeaveProc('TProjectHTML.CreateUnitsPage');
{$ENDIF}
end;

procedure TProjectHTML.CreateProcPage;
begin
{$IFDEF DEBUG}
  Debugger.EnterProc('TProjectHTML.CreateProcPage');
{$ENDIF}

  FProcsPage := NewPage(Group, 'Procedures');

  AddItemList(FProcsPage, Group.RecursiveProcedureList);

  SavePage(FProcsPage);
{$IFDEF DEBUG}
  Debugger.LeaveProc('TProjectHTML.CreateProcPage');
{$ENDIF}
end;

procedure TProjectHTML.CreateHelp;
var
  Helpfile: TStringList;
  Page: TNLDHtmlPage;
  FileName: string;
  i: Integer;
  RowCount: Integer;
begin
{$IFDEF DEBUG}
  Debugger.EnterProc('TProjectHTML.CreateHelp');
{$ENDIF}

  Helpfile := TStringList.Create;
  try
    Helpfile.Add('[OPTIONS]');
    Helpfile.add('Auto index=Yes');
    Helpfile.add('Compatibility=1.1 or later');
    Helpfile.add(Format('Compiled file=%s.chm', [FTitle]));
    Helpfile.add(Format('Contents file=%stoc.hhc', [FTitle]));
    Helpfile.add(Format('Default topic=%sstart.htm', [FTitle]));
    Helpfile.add(Format('Title=%s', [FTitle]));
    Helpfile.Add('Full-text search=Yes');
    Helpfile.add('Display compile progress=No');
    {   Tijdelijk even geen Index tabblad
        Helpfile.add(Format('Index file=%sIndex.hhk', [FTitle])); }
    Helpfile.add('');
    Helpfile.add('[FILES]');
    Helpfile.Add(FProjPage.FileName);

    FileName := FOutputDir + '\' + FTitle + '.hhp';
    Helpfile.SaveToFile(FileName);
    FPageList.Add(FileName);
  finally
    Helpfile.Free;
  end;

  Page := TNLDHtmlPage.Create(nil);
  try
    Page.Head.AddText(cssFile);

    with Page.Body.AddPar do
    begin
      CssClass := 'logo';
      AddLine('Aangemaakt door BDoc 2.0');
      AddLine('Copyright Bergler ICT');
      AddHyperlink('http://www.Bergler.nl', 'www.Bergler.nl', True);
      AddLine('');
      AddLine('');
      AddLine('Datum en tijd documentatie: ' + FormatDateTime('dd-mm-yy hh:nn', Now));
      AddLine('');
    end;

    RowCount := 0;
    for i := 0 to Group.RecursiveUnitList.Count-1 do
      RowCount := RowCount + Group.RecursiveUnitList[i].RowCount;

    with Page.Body.AddTable do
    begin
      with AddRow do
      begin
        with AddCol do
          AddText('Aantal projecten: ');
        with AddCol do
          AddText(IntToStr(Group.Projects.Count));
      end;
      with AddRow do
      begin
        with AddCol do
          AddText('Aantal units: ');
        with AddCol do
          AddText(IntToStr(Group.RecursiveUnitList.Count));
      end;
      with AddRow do
      begin
        with AddCol do
          AddText('Aantal regels: ');
        with AddCol do
          AddText(IntToStr(RowCount));
      end;
      with AddRow do
      begin
        with AddCol do
          AddText('Aantal classes: ');
        with AddCol do
          AddText(IntToStr(Group.RecursiveClassList.Count));
      end;
      with AddRow do
      begin
        with AddCol do
          AddText('Aantal interfaces: ');
        with AddCol do
          AddText(IntToStr(Group.RecursiveInterfaceList.Count));
      end;
      with AddRow do
      begin
        with AddCol do
          AddText('Aantal procedures: ');
        with AddCol do
          AddText(IntToStr(Group.RecursiveProcedureList.Count));
      end;
      with AddRow do
      begin
        with AddCol do
          AddText('Aantal types: ');
        with AddCol do
          AddText(IntToStr(Group.RecursiveTypeList.Count));
      end;
    end;

    Page.FileName := FOutputDir + '\' + FTitle + 'start' + FileExt;
    SavePage(Page);
  finally
    Page.Free;
  end;
{$IFDEF DEBUG}
  Debugger.LeaveProc('TProjectHTML.CreateHelp');
{$ENDIF}
end;

function TProjectHTML.FileCount: Integer;
var
  i, j: Integer;
begin
  Result := 0;

  for i := 0 to FGroup.RecursiveUnitList.Count - 1 do
    with FGroup.RecursiveUnitList[i] do
    begin
      for j := 0 to Classes.Count - 1 do
      begin
        Inc(Result);
        Inc(Result, Classes[j].Properties.Count);
        Inc(Result, Classes[j].Methods.Count);
      end;

      for j := 0 to Interfaces.Count - 1 do
      begin
        Inc(Result);
        Inc(Result, Interfaces[j].Properties.Count);
        Inc(Result, Interfaces[j].Methods.Count);
      end;

      Inc(Result, Types.Count);
      Inc(Result, Constants.Count);
    end;
end;

end.
