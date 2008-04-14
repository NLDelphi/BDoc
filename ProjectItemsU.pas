unit ProjectItemsU;

interface

uses
  Classes, Contnrs;

{ TODO : ByName routines verwijzen naar ByName standaard }
type
  TprjItemList = class;
  TPrjGroup = class;
  TprjUnitList = class;
  TprjUnit = class;
  TprjFileList = class;
  TprjClassList = class;
  TprjClass = class;
  TprjMethodList = class;
  TprjPropertyList = class;
  TprjProject = class;
  TprjProjectList = class;
  TprjFile = class;
  TprjInterface = class;
  TprjInterfaceList = class;
  TKeywordList = class;
  TprjTypeList = class;
  TprjConstantList = class;
  TprjDummyClass = class;
  TprjCustomClass = class;

  TClassSection = (csPrivate, csProtected,
    csPublic, csPublished, csUndefined);
  TClassSections = set of TClassSection;

  TClassType = (ctClass, ctInterface);

  TLoadFileEvent = procedure(Sender: TObject; Index: Integer) of object;

  { Basisclasse voor projectdocumentatie }
  TprjItem = class(TObject)
  private
    FItemName: string;
    FSourceLine: string;
    FClassSecion: TClassSection;
    FOwner: TprjItem;
    FDescription: TStrings;
    FInterfaceLine: Integer;
    FKeywords: TKeywordList;
    FTag: Longint;
    FRowCount: Integer;
    procedure SetDescription(const Value: TStrings);
    procedure SetSourceLine(const Value: string); virtual;
  protected
    function GetProject: TprjProject; virtual;
    function GetProjectGroup: TPrjGroup; virtual;
    function GetprjFile: TprjFile; virtual;
  public
    { Naam van het onderdeel }
    property ItemName: string read FItemName write FItemName;

    { Bevat de interfaceregel van het item }
    property SourceLine: string read FSourceLine write SetSourceLine;

    { Positie in de classe waar het item zich bevind, zie [TClassSection] }
    property ClassSecion: TClassSection read FClassSecion write FClassSecion;

    { Projectitem waar dit item toe behoort }
    property Owner: TprjItem read FOwner;

    { Omschrijving zoals deze in commentaar is gegeven }
    property Description: TStrings read FDescription write SetDescription;

    { Project waar dit item toe behoort. Er wordt recursief naar het project
      gezocht, dus bijvoorbeeld van een classe naar een unit en vervolgens
      naar een project }
    property Project: TprjProject read GetProject;

    { Projectgroep waar dit item toe behoort. Er wordt recursief naar de groep
      gezocht, dus bijvoorbeeld van een classe naar een unit en vervolgens
      naar een project en een projectgroep }
    property ProjectGroup: TPrjGroup read GetProjectGroup;

    { Unit waarin het item is gedefinieerd }
    property prjFile: TprjFile read GetprjFile;

    { Regelnummer waar item wordt gedefinieerd in de interface }
    property InterfaceLine: Integer read FInterfaceLine write FInterfaceLine;

    { Keywords zoals deze in het commentaar zijn opgenomen tussen rechte haken }
    property Keywords: TKeywordList read FKeywords;

    { Ieder item krijgt hierin zijn eigen nummer }
    property Tag: Longint read FTag;

    { Totaal aantal regels in bestand }
    property RowCount: Integer read FRowCount;

    constructor Create(AOwner: TprjItem); virtual;
    destructor Destroy; override;

    procedure FindDescription; virtual;
    procedure FindKeywords; virtual;
    function FindKeyLink(const Keyword: string): TprjItem; virtual;
  end;

  { Projecttypes }
  TprjType = class(TPrjItem);

  { Algemeen bestandstype }
  TprjFile = class(TprjItem)
  private
    FImplementStart: Integer;
    FConstants: TprjConstantList;
    FInterfaces: TprjInterfaceList;
    FTypes: TprjTypeList;
    FClasses: TprjClassList;
    FClassesDone: Boolean;
    FRecursiveUnitList: TprjUnitList;
    FUsesDone: Boolean;
    FFileName: string;
    FData: TStrings;
    FUnits: TprjUnitList;
    FUsedBy: TprjUnitList;
    FUsedByProjects: TprjUnitList;
    FSearchPath: string;
    FEndComment: string;
    FStartComment: string;
    FDummyClass: TprjDummyClass;
    FUsesLine1: Integer;
    FUsesLine2: Integer;
    FLoadedDone: Boolean;
    procedure ReadUses(Line: Integer);
    function ClassPos(Line: integer): Integer;
    function InterfacePos(Line: integer): Integer;
    function TypePos(Line: integer): Integer;
    procedure ReadType(var Line: Integer);
    function IsConstLine(Line: Integer): boolean;
    procedure ReadConst(var Line: Integer);
    function GetProcedures: TprjMethodList;
    function IsMethodLine(Line: Integer): Boolean;
    procedure ReadMethod(var Line: Integer; ClassItem: TprjCustomClass;
      ClassSection: TClassSection);
    function CreateItemName(const Name: string): string;
  protected
    function FindDescriptionFwd(Lines: TStrings; StartLine: Integer): Boolean;
    function FindDescriptionBwd(Lines: TStrings; EndLine: Integer): Boolean;
    function CreateUnitList: TprjUnitList; virtual;
    function ClassByName(const ClassName: string): TprjClass;
    function InterfaceByName(const ClassName: string): TprjInterface;
    procedure ReadClass(var Line: Integer; ClassType: TClassType);
    function FilePath: string;
    function ExpandUnitName(const UnitName: string): string;
    function CheckLineUses(Line: Integer): Boolean;
    function GetSearchPath: string; virtual;
    function IsCommentLine(var Line: string): Boolean;
    function IsEndCommentLine(const Line: string): Boolean;
    procedure InitCommentSearch;
  public
    { Bestandsnaam, inclusief pad }
    property FileName: string read FFileName write FFileName;

    { Bevat de complete inhoud van het bestand }
    property Data: TStrings read FData;

    { Lijst van units welke worden gebruikt door deze unit }
    property Units: TprjUnitList read FUnits;

    { Lijst van directories, gescheiden door een ; waarin de gebruikte units
      worden gezocht }
    property SearchPath: string read GetSearchPath write FSearchPath;

    { Recursieve lijst van units welke worden gebruikt door deze unit }
    property RecursiveUnitList: TprjUnitList read FRecursiveUnitList;

    { Classes gedefinieerd in dit bestand }
    property Classes: TprjClassList read FClasses;

    { Interfaces gedefinieerd in dit bestand }
    property Interfaces: TprjInterfaceList read FInterfaces;

    { Lijst van units welke deze unit gebruiken }
    property UsedBy: TprjUnitList read FUsedBy;

    { Lijst van projecten welke deze unit gebruiken }
    property UsedByProjects: TprjUnitList read FUsedByProjects;

    { Types gedefinieerd in dit bestand }
    property Types: TprjTypeList read FTypes write FTypes;

    { Constantes gedefinieerd in dit bestand }
    property Constants: TprjConstantList read FConstants;

    { Procedures (en functions) gedefinieerd in dit bestand, niet gekoppeld aan
      een classe }
    property Procedures: TprjMethodList read GetProcedures;

    { Uses line in de interface }
    property UsesLine1: Integer read FUsesLine1;

    { Uses line in de implementation }
    property UsesLine2: Integer read FUsesLine2;

    { Regel waarop de implementation start }
    property ImplementStart: Integer read FImplementStart;

    constructor Create(AOwner: TprjItem); override;
    destructor Destroy; override;

    procedure LoadFromFile; virtual;
    procedure FindUses; virtual;

    procedure FillRecursiveUnitList; virtual;

    function NewUnit(const FileName: string): TprjUnit; virtual;
    procedure FindInterfaces; virtual;
    procedure FindDescription; override;
  end;

  { Unit }
  TprjUnit = class(TprjFile)
  protected
    function GetSearchPath: string; override;
  public
    procedure FindKeywords; override;
    procedure FindDescription; override;
  end;

  { Projectfile }
  TprjProject = class(TprjFile)
  protected
    function GetSearchPath: string; override;
  public
    procedure LoadFromFile; override;
    procedure FindUses; override;

    function NewUnit(const FileName: string): TprjUnit; override;
    procedure FindInterfaces; override;
    procedure FillRecursiveUnitList; override;
  end;

  { Projectgroep }
  TPrjGroup = class(TprjFile)
  private
    FProjects: TprjProjectList;
    FRecursiveClassList: TprjClassList;
    FRecursiveProcedureList: TprjMethodList;
    FRecursiveTypeList: TprjTypeList;
    FRecursiveInterfaceList: TprjInterfaceList;
    FOnLoadFile: TLoadFileEvent;
  protected
    procedure FillRecursiveClassList;
    procedure FillRecursiveProcedureList;
    procedure FillRecursiveTypeList;
    procedure FillRecursiveInterfaceList;
    function CreateUnitList: TprjUnitList; override;
  public
    constructor Create(AOwner: TprjItem); override;
    destructor Destroy; override;

    function NewProject: TprjProject;
    procedure LoadFromFile; override;
    procedure FindUses; override;
    property Projects: TprjProjectList read FProjects;
    property RecursiveClassList: TprjClassList read FRecursiveClassList;
    property RecursiveProcedureList: TprjMethodList
      read FRecursiveProcedureList;
    property RecursiveTypeList: TprjTypeList read FRecursiveTypeList;
    property RecursiveInterfaceList: TprjInterfaceList read
      FRecursiveInterfaceList;
    property OnLoadFile: TLoadFileEvent read FOnLoadFile write FOnLoadFile;
  end;

  { Gegevens van een classe. Omdat een interface (in deze context) ook een
    soort classe is erven TPrjClass en TPrjInterface van deze classe }
  TprjCustomClass = class(TprjItem)
  private
    FParents: string;
    FMethods: TprjMethodList;
    FProperties: TprjPropertyList;
    FParentClass: TprjCustomClass;
    FChildClasses: TprjClassList;
    procedure SetParentClass(const Value: TprjCustomClass);
    function GetParentName: string;
  public
    { Lijst van parents zoals deze in de interface is opgenomen }
    property Parents: string read FParents write FParents;

    { Lijst van methods van deze classe }
    property Methods: TprjMethodList read FMethods;

    { Lijst van properties van deze classe }
    property Properties: TprjPropertyList read FProperties write FProperties;

    { Classe waar deze classe van erft }
    property ParentClass: TprjCustomClass read FParentClass
      write SetParentClass;

    { Classes welke erven van deze classe }
    property ChildClasses: TprjClassList read FChildClasses;

    { Naam van de eerste parentclass zoals deze in de [Parents] staan }
    property ParentName: string read GetParentName;

    procedure FindParent; virtual;
    procedure FindDescription; override;
    function InheritsFrom(const ClassName: string): Boolean;
    function FindKeyLink(const Keyword: string): TprjItem; override;
    procedure FindKeywords; override;

    constructor Create(AOwner: TprjItem); override;
    destructor Destroy; override;
  end;

  TprjDummyClass = class(TprjCustomClass);

  TprjClass = class(TprjCustomClass)
  private
    FInterfaces: TprjInterfaceList;
    procedure AddInterface(prjInterface: TprjInterface);
  public
    property Interfaces: TprjInterfaceList read FInterfaces;

    constructor Create(AOwner: TprjItem); override;
    destructor Destroy; override;

    procedure FindParent; override;
    procedure FindInterfaces; virtual;
    function FindKeyLink(const Keyword: string): TprjItem; override;
  end;

  TprjInterface = class(TprjCustomClass)
  private
    FClasses: TprjClassList;
  public
    property Classes: TprjClassList read FClasses;

    constructor Create(AOwner: TprjItem); override;
    destructor Destroy; override;

    procedure FindParent; override;
    procedure AddClass(prjClass: TprjClass);
  end;

  TPrjClassItem = class(TprjItem)
  private
    function GetprjClass: TprjCustomClass;
  public
    property prjClass: TprjCustomClass read GetprjClass;
    procedure FindDescription; override;
    function FindKeyLink(const Keyword: string): TprjItem; override;
  end;

  TprjMethod = class(TPrjClassItem)
  private
    FIsVirtual: Boolean;
    FImplementLine: Integer;
    procedure SetSourceLine(const Value: string); override;
  public
    property ImplementLine: Integer read FImplementLine write FImplementLine;
    constructor Create(AOwner: TprjItem); override;
    procedure FindDescription; override;
    procedure FindImplement;
    property IsVirtual: Boolean read FIsVirtual write FIsVirtual;
  end;

  TprjProperty = class(TPrjClassItem);

  TprjConstant = class(TPrjItem);

  TprjItemList = class(TObjectList)
  private
    FOwner: TprjItem;
  protected
    function GetItem(Index: Integer): TprjItem;
    function GetProject: TprjProject; virtual;
    function GetProjectGroup: TprjGroup; virtual;
    function GetprjFile: TprjFile; virtual;
  public
    property Items[Index: Integer]: TprjItem read GetItem; default;
    property Owner: TprjItem read FOwner;

    property ProjectGroup: TprjGroup read GetProjectGroup;
    property Project: TprjProject read GetProject;
    property prjFile: TprjFile read GetprjFile;

    constructor Create(AOwner: TprjItem); overload; virtual;
    constructor Create(AOwner: TprjItem; OwnsObjects: boolean); overload;
      virtual;
    function AddNew: TprjItem;
    function Add(AObject: TObject): Integer;
    procedure Sort;
    function ItemByName(const Name: string): TprjItem;
  end;

  TPrjClassItemList = class(TprjItemList)
  private
    function GetprjClass: TprjCustomClass;
  public
    property prjClass: TprjCustomClass read GetprjClass;
  end;

  TprjConstantList = class(TprjItemList)
  protected
    function GetItems(Index: Integer): TprjConstant;
  public
    function AddNew: TprjConstant;

    property Items[Index: Integer]: TprjConstant read GetItems; default;
  end;

  TprjMethodList = class(TPrjClassItemList)
  protected
    function GetItems(Index: Integer): TprjMethod;
  public
    function AddNew: TprjMethod;

    property Items[Index: Integer]: TprjMethod read GetItems; default;
  end;

  TprjPropertyList = class(TPrjClassItemList)
  protected
    function GetItems(Index: Integer): TprjProperty;
  public
    function AddNew: TprjProperty;

    property Items[Index: Integer]: TprjProperty read GetItems; default;
  end;

  TprjTypeList = class(TprjItemList)
  protected
    function GetItems(Index: Integer): TprjType;
  public
    function AddNew: TprjType;

    property Items[Index: Integer]: TprjType read GetItems; default;
  end;

  TprjFileList = class(TprjItemList)
  private
    function GetItems(Index: Integer): TprjFile;
  public
    function FileByName(const FileName: string): TprjFile;
    function AddNew: TprjFile;
    property Items[Index: Integer]: TprjFile read GetItems;
  end;

  TprjUnitList = class(TprjFileList)
  private
    FAutoLoad: Boolean;
  protected
    function GetItems(Index: Integer): TprjUnit;
  public
    constructor Create(AOwner: TprjItem); override;
    constructor Create(AOwner: TprjItem; OwnsObjects: boolean); override;

    function AddNew(const FileName: string): TprjUnit;
    property Items[Index: Integer]: TprjUnit read GetItems; default;

    { Bepaalt of een nieuw toegevoegde unit meteen wordt ingelezen met
      Unit.LoadFromFile }
    property AutoLoad: Boolean read FAutoLoad write FAutoLoad default True;
  end;

  TprjProjectList = class(TprjUnitList)
  protected
    function GetItems(Index: Integer): TprjProject;
  public
    function AddNew: TprjProject;
    property Items[Index: Integer]: TprjProject read GetItems; default;
  end;

  TprjCustomClassList = class(TprjItemList)
  protected
    function GetItems(Index: Integer): TprjCustomClass;
  public
    property Items[Index: Integer]: TprjCustomClass read GetItems; default;
  end;

  TprjClassList = class(TprjCustomClassList)
  protected
    function GetItems(Index: Integer): TprjClass;
  public
    property Items[Index: Integer]: TprjClass read GetItems; default;

    function AddNew: TprjClass;
  end;

  TprjInterfaceList = class(TprjCustomClassList)
  protected
    function GetItems(Index: Integer): TprjInterface;
  public
    property Items[Index: Integer]: TprjInterface read GetItems; default;

    function AddNew: TprjInterface;
  end;

  TKeywordItem = class
  private
    FKeyword: string;
    FItem: TprjItem;
  public
    property Keyword: string read FKeyword;
    property Item: TprjItem read FItem;

    constructor Create(AKeyword: string; AItem: TprjItem);
  end;

  TKeywordList = class(TObjectList)
  protected
    function GetItems(Index: Integer): TKeywordItem;
  public
    property Items[Index: Integer]: TKeywordItem read GetItems; default;

    procedure Add(AKeyword: string; AItem: TprjItem);
  end;

function ClassSectionText(Section: TClassSection): string;
function SortFunction(Item1, Item2: Pointer): Integer;

var
  LowestMethodSection: TClassSection;
  LowestPropertySection: TClassSection;

implementation

uses
{$IFDEF DEBUG} uDBG, {$ENDIF}
  SysUtils, IniFiles, Math,
  JclStrings, JclFileUtils, JvStrToHtml;

var
  TagCounter: Longint;
  FileCache: TStringList;

{ Retourneert de section als een string }
function ClassSectionText(Section: TClassSection): string;
begin
  Result := '';

  case Section of
    csPrivate: Result := 'private';
    csProtected: Result := 'protected';
    csPublic: Result := 'public';
    csPublished: Result := 'published';
  end;

end;

{ TprjItems }

{ TprjItemList }

function TprjItemList.Add(AObject: TObject): Integer;
begin
  if IndexOf(AObject) = -1 then
    Result := inherited Add(AObject)
  else
    Result := -1;
end;

function TprjItemList.AddNew: TprjItem;
begin
  Result := TprjItem.Create(Owner);
  inherited Add(Result);
end;

constructor TprjItemList.Create(AOwner: TprjItem);
begin
  inherited Create;
  FOwner := AOwner;
end;

constructor TprjItemList.Create(AOwner: TprjItem; OwnsObjects: boolean);
begin
  inherited Create(OwnsObjects);
  FOwner := AOwner;
end;

function TprjItemList.GetItem(Index: Integer): TprjItem;
begin
  Result := TprjItem(inherited Items[Index]);
end;

function TprjItemList.GetprjFile: TprjFile;
begin
  if Owner is TprjFile then
    Result := TprjFile(Owner)
  else if Owner <> nil then
    Result := Owner.prjFile
  else
    Result := nil;
end;

function TprjItemList.GetProject: TprjProject;
begin

  if FOwner is TprjProject then
    Result := TprjProject(FOwner)
  else if Owner <> nil then
    Result := Owner.Project
  else
    Result := nil;

end;

function TprjItemList.GetProjectGroup: TprjGroup;
begin

  if Owner is TPrjGroup then
    Result := TPrjGroup(Owner)
  else if Project <> nil then
    Result := Project.ProjectGroup
  else if Owner <> nil then
    Result := Owner.ProjectGroup
  else
    Result := nil;

end;

function SortFunction(Item1, Item2: Pointer): Integer;
begin
  Result := CompareText(TprjItem(Item1).ItemName,
    TprjItem(Item2).ItemName);
end;

procedure TprjItemList.Sort;
begin
  inherited Sort(SortFunction);
end;

function TprjItemList.ItemByName(const Name: string): TprjItem;
var
  i: Integer;
begin

  Result := nil;

  for i := 0 to Count - 1 do
  begin
    if CompareText(Items[i].ItemName, Name) = 0 then
    begin
      Result := Items[i];
      Break;
    end;
  end;

end;

{ TprjClassList }

function TprjClassList.AddNew: TprjClass;
begin

  Result := TprjClass.Create(prjFile);
  inherited Add(Result);

end;

function TprjClassList.GetItems(Index: Integer): TprjClass;
begin

  Result := TprjClass(inherited GetItem(Index));

end;

{ TprjProjectList }

function TprjProjectList.AddNew: TprjProject;
begin

  Result := TprjProject.Create(ProjectGroup);
  inherited Add(Result);

end;

function TprjProjectList.GetItems(Index: Integer): TprjProject;
begin
  Result := TprjProject(inherited GetItems(Index));
end;

{ TprjUnitList }

function TprjUnitList.AddNew(const FileName: string): TprjUnit;
begin
  Result := TprjUnit(ProjectGroup.Units.FileByName(FileName));

  if Result = nil then
  begin
    Result := TprjUnit.Create(Project);
    Result.FileName := FileName;
    ProjectGroup.Units.Add(Result);

    if FAutoLoad then
      Result.LoadFromFile;
  end;

  inherited Add(Result);
end;

constructor TprjUnitList.Create(AOwner: TprjItem);
begin
  inherited;
  FAutoLoad := True;
end;

constructor TprjUnitList.Create(AOwner: TprjItem; OwnsObjects: boolean);
begin
  inherited;
  FAutoLoad := True;
end;

function TprjUnitList.GetItems(Index: Integer): TprjUnit;
begin

  Result := TprjUnit(inherited GetItems(Index));

end;

{ TprjMethodList }

function TprjMethodList.AddNew: TprjMethod;
begin

  Result := TprjMethod.Create(prjClass);
  inherited Add(Result);

end;

function TprjMethodList.GetItems(Index: Integer): TprjMethod;
begin

  Result := TprjMethod(inherited GetItem(Index));

end;

{ TprjFile }

procedure TprjFile.ReadUses(Line: Integer);
var
  UnitName: string;
  TextLine: string;
  InPos: Integer;
  TempLine: string;
  i: Integer;
  List: TStringList;
begin

  TextLine := '';

  { Uses kan over meerdere regels verspreid zitten, plak deze regels achter
    elkaar }
  repeat
    TempLine := FData[Line];

    { Declaratie zoals deze in de projectsource staat. Iedere unit heeft
      een eigen regel in de vorm 'MainFormU in 'MainFormU.pas' '.
      Alles achter 'in' wordt verwijderd en ',' wordt geplakt }
    InPos := Pos(' in ''', TempLine);
    if InPos > 0 then
    begin
      TempLine := Copy(TempLine, InPos + 5, Length(TempLine)) + ',';
      TempLine := Copy(TempLine, 1, Pos('''', TempLine) - 1) + ',';
    end
    else if
      CompareText(Copy(TempLine, 1, 4), 'uses') = 0 then
      TempLine := Copy(TempLine, 5, Length(TempLine));

    { Als regel begint met 'contains' dit verwijderen }
    if CompareText(Copy(TempLine, 1, 8), 'contains') = 0 then
      TempLine := Copy(TempLine, 9, Length(TempLine));

    TextLine := TextLine + Trim(TempLine);

    { Laatste regel? Dan stoppen }
    if (Pos(';', FData[Line - 1]) <> 0) then
      Break;

    Inc(Line);
  until (Line > FData.Count - 1);

  { TextLine bevat nu de units gescheiden door , }
  { TODO : testsen ivm JCL }
  List := TStringList.Create;
  try
    StrTokenToStrings(TextLine, ',', List);

    for i := 0 to List.Count-1 do
    begin
      UnitName := List[i];
      UnitName := StrRemoveChars(UnitName, [' ']);
      UnitName := StrRemoveChars(UnitName, [';']);
      UnitName := StrRemoveChars(UnitName, [',']);

      if ExtractFileExt(UnitName) = '' then
        UnitName := UnitName + '.pas';

      UnitName := ExpandUnitName(UnitName);

      if UnitName <> '' then
        NewUnit(UnitName);
    end;
  finally
    List.Free;
  end;
end;

function TprjFile.CheckLineUses(Line: Integer): Boolean;
begin

  Result := (CompareText(Copy(FData[Line], 1, 4), 'uses') = 0) or
    (CompareText(Copy(FData[Line], 1, 8), 'contains') = 0);

  if Result then
  begin
    if FUsesLine1 = -1 then
      FUsesLine1 := Line
    else
      FUsesLine2 := Line;

    ReadUses(Line);
  end;

end;

constructor TprjFile.Create;
begin

  inherited;
  FData := TStringList.Create;
  FUnits := CreateUnitList;
  FRecursiveUnitList := TprjUnitList.Create(Self, False);
  FClasses := TprjClassList.Create(Self);
  FInterfaces := TprjInterfaceList.Create(Self);
  FUsedBy := TprjUnitList.Create(Self, False);
  FUsedByProjects := TprjUnitList.Create(Self, False);
  FTypes := TprjTypeList.Create(Self);
  FConstants := TprjConstantList.Create(Self);
  FDummyClass := TprjDummyClass.Create(Self);
  FDummyClass.ItemName := 'NoClass';

end;

destructor TprjFile.Destroy;
begin

  FDummyClass.Free;
  FConstants.Free;
  FTypes.Free;
  FUsedByProjects.Free;
  FUsedBy.Free;
  FInterfaces.Free;
  FClasses.Free;
  FRecursiveUnitList.Free;
  FData.Free;
  FUnits.Free;

  inherited;

end;

{ Zet een bestandsnaam om naar een bestandsnaam met path. Omdat een FileExists
  relatief lang duurt worden de bestanden in een stringlist bijgehouden. }
function TprjFile.ExpandUnitName(const UnitName: string): string;
var
  i: Integer;
  List: TStringList;
  CheckFile: string;
  Pos: Integer;
begin
  Result := '';
  Pos := FileCache.IndexOfName(UnitName);

  if Pos > -1 then
    Result := FileCache.Values[UnitName]
  else
  begin
    CheckFile := ExpandFileName(UnitName);
    if FileExists(CheckFile) then
      Result := CheckFile
    else
    begin
      CheckFile := FilePath + '\' + UnitName;
      if FileExists(CheckFile) then
          Result := CheckFile
      else
      begin
        List := TStringList.Create;
        try
          StrTokenToStrings(SearchPath, ';', List);

          for i := 0 to List.Count-1 do
          begin
            CheckFile := List[i] + '\' + UnitName;

            if FileExists(CheckFile) then
            begin
              Result := CheckFile;
              Break;
            end;
          end;
        finally
          List.Free;
        end;

        if (Result = '') and (Project <> nil) then
          for i := 0 to Project.Units.Count - 1 do
          begin
            if SameText(ExtractFileName(Project.Units[i].FileName), UnitName) then
            begin
              Result := Project.Units[i].FileName;
              Exit;
            end;
          end;
      end;
    end;

    FileCache.Add(UnitName + '=' + Result);
  end;
end;

{ Retourneerd het path van het bestand, zonder laatste '\' }

function TprjFile.FilePath: string;
begin

  Result := ExtractFilePath(FFileName);

  Result := Copy(Result, 1, Length(Result) - 1);

end;

procedure TprjFile.FindUses;
var
  I: Integer;
begin

  FRecursiveUnitList.Clear;

  if FUsesDone then
    Exit;

  FUsesLine1 := -1;
  FUsesLine2 := -1;

  FUsesDone := True;

  for I := 0 to FData.Count - 1 do
  begin
    CheckLineUses(I);

    if CompareText(FData[i], 'implementation') = 0 then
      FImplementStart := I;
  end;
end;

function TprjFile.GetSearchPath: string;
begin

  Result := FSearchPath;

end;

procedure TprjFile.LoadFromFile;
var
  Stream: TStream;
begin
  if FLoadedDone then
    Exit;

  FLoadedDone := True;

  if FileName = '' then
    raise Exception.Create('No FileName assigned, unable to load');

  if not FileExists(FileName) then
    raise Exception.Create('File not found: ' + FileName);

  ItemName := Extractfilename(ChangeFileExt(FileName, ''));

  if ItemName = '' then
    raise Exception.Create('Invalid filename: ' + FileName);

  Stream := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyNone);
  try
    FData.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;

  FRowCount := FData.Count;

  FindUses;
  FindInterfaces;
  FindDescription;
  FData.Clear;
end;

function TprjFile.NewUnit(const FileName: string): TprjUnit;
begin
  Result := FUnits.AddNew(FileName);
  Result.UsedBy.Add(self);
end;

procedure TprjFile.FillRecursiveUnitList;

  procedure AddUnits(ProjectFile: TprjFile);
  var
    i: Integer;
  begin

    for i := 0 to ProjectFile.Units.Count - 1 do
    begin
      if FRecursiveUnitList.FileByName(ProjectFile.Units[i].FileName) = nil then
      begin
        FRecursiveUnitList.Add(ProjectFile.Units[i]);
        AddUnits(ProjectFile.Units[i]);
      end;
    end;
  end;

begin
  FRecursiveUnitList.Clear;

  AddUnits(Self);
end;

function TprjFile.ClassPos(Line: integer): Integer;
var
  TempLine: string;
begin
  TempLine := LowerCase(FData[Line]);

  Result := Pos(' class(', TempLine);

  if Result = 0 then
    Result := Pos(' class;', TempLine);

  if (Result = 0) and (Copy(TempLine, Length(TempLine) - 4, 5) = 'class') then
    Result := Pos(' class', TempLine);

  if Result = 0 then
    Result := Pos('=class(', TempLine);

  if Result = 0 then
    Result := Pos(' class (', TempLine);

  { Voor onze good old Pascallers }
  if Result = 0 then
    Result := Pos(' = object', TempLine);
end;

function TprjFile.TypePos(Line: integer): Integer;
begin
  Result := 0;

  if ClassPos(Line) > 0 then
    Exit;

  Result := Pos('=', FData[Line]);
end;

function TprjFile.IsConstLine(Line: Integer): Boolean;
begin
  Result := (Copy(Trim(LowerCase(FData[Line])), 1, 5) = 'const') or
    (Copy(Trim(LowerCase(FData[Line])), 1, 14) = 'resourcestring');
end;

function TprjFile.InterfacePos(Line: integer): Integer;
var
  TempLine: string;
begin

  TempLine := LowerCase(FData[Line]);

  Result := Pos(' interface(', TempLine);

  if Result = 0 then
    Result := Pos(' interface;', TempLine);

  if Result = 0 then
    if Copy(TempLine, Length(TempLine)-8, 9) = 'interface' then
      Result := Length(TempLine)-9;
end;

procedure TprjFile.FindInterfaces;
var
  Line: string;
  I: Integer;
begin
  if FClassesDone then
    Exit;

  FClassesDone := True;

  InitCommentSearch;

  I := 0;

  while I < FData.Count - 1 do
  begin
    Line := Fdata[I];

    if not IsCommentLine(Line) then
    begin

      if ClassPos(I) > 0 then
        ReadClass(I, ctClass)
      else if InterfacePos(I) > 0 then
        ReadClass(I, ctInterface)

      else if IsConstLine(I) then
        ReadConst(I)

      else if IsMethodLine(I) then
        ReadMethod(I, FDummyClass, csPublic)
      else if TypePos(I) > 0 then
        ReadType(I)
      else if (CompareText(Copy(FData[i], 1, 14), 'implementation') = 0) or
        (I >= FImplementStart) then
        Break;
    end;

    Inc(I);
  end;
end;

function TprjFile.CreateItemName(const Name: string): string;
begin

  Result := Trim(Name);

  Result := Copy(Result, Pos(' ', Result) + 1, Length(Result));

  if Pos('[', Result) > 0 then
    Result := Copy(Result, 1, Pos('[', Result) - 1);

  if Pos('(', Result) > 0 then
    Result := Copy(Result, 1, Pos('(', Result) - 1);

  if Pos(':', Result) > 0 then
    Result := Copy(Result, 1, Pos(':', Result) - 1);

  if Pos(';', Result) > 0 then
    Result := Copy(Result, 1, Pos(';', Result) - 1);

  Result := Trim(Result);

end;

procedure TprjFile.ReadClass(var Line: Integer; ClassType: TClassType);
var
  TempLine: string;
  iPos: Integer;
  ClassName: string;
  ClassLine: string;
  NewClass: TprjCustomClass;
  ClassSection: TClassSection;
  NewItem: TprjClassItem;
begin

  if ClassType = ctClass then
    iPos := ClassPos(Line)
  else
    iPos := InterfacePos(Line);

  ClassLine := FData[Line];

  ClassName := Copy(ClassLine, 1, Pos('=', ClassLine));
  ClassName := StrRemoveChars(ClassName, [' ']);
  ClassName := StrRemoveChars(ClassName, ['=']);

  if ClassType = ctClass then
  begin
    NewClass := ClassByName(ClassName);

    if NewClass = nil then
      NewClass := FClasses.AddNew;

    NewClass.Parents := Copy(ClassLine, iPos + 7, Length(ClassLine));
  end
  else
  begin
    NewClass := InterfaceByName(ClassName);

    if NewClass = nil then
      NewClass := FInterfaces.AddNew;

    NewClass.Parents := Copy(ClassLine, iPos + 11, Length(ClassLine));
  end;

  NewClass.InterfaceLine := Line;
  NewClass.ItemName := ClassName;
  NewClass.SourceLine := ClassLine;

  NewClass.Parents := StrRemoveChars(NewClass.Parents, [' ']);
  NewClass.Parents := StrRemoveChars(NewClass.Parents, [')']);
  NewClass.Parents := StrRemoveChars(NewClass.Parents, ['(']);
  NewClass.Parents := StrRemoveChars(NewClass.Parents, [';']);

  if NewClass.Parents = '' then
    NewClass.Parents := 'TObject';

  { Alleen declaratie }
  if Copy(ClassLine, Length(ClassLine), 1) = ';' then
    Exit;

  Inc(Line);

  ClassSection := csPublic;

  while Line < FData.Count - 1 do
  begin
    TempLine := Trim(LowerCase(FData[Line]));

    if TempLine = 'end;' then
      Break
    else if TempLine = 'private' then
      ClassSection := csPrivate
    else if TempLine = 'protected' then
      ClassSection := csProtected
    else if TempLine = 'public' then
      ClassSection := csPublic
    else if TempLine = 'published' then
      ClassSection := csPublished
    else if IsMethodLine(Line) then
    begin
      if ClassSection >= LowestMethodSection then
        ReadMethod(Line, NewClass, ClassSection)
    end
    else if (Copy(TempLine, 1, 8) = 'property') then
    begin
      if ClassSection >= LowestPropertySection then
      begin
        NewItem := NewClass.Properties.AddNew;
        NewItem.InterfaceLine := Line;
        NewItem.ClassSecion := ClassSection;
        NewItem.SourceLine := FData[Line];
        NewItem.ItemName := CreateItemName(FData[Line]);
      end;
    end;

    Inc(Line);
  end;

  Dec(Line);

end;

function TprjFile.IsMethodLine(Line: Integer): Boolean;
var
  TempLine: string;
begin

  TempLine := Trim(LowerCase(FData[Line]));
  Result := (Copy(TempLine, 1, 10) = 'procedure ') or
    (Copy(TempLine, 1, 9) = 'function ') or
    (Copy(TempLine, 1, 12) = 'constructor ') or
    (Copy(TempLine, 1, 11) = 'destructor ');

end;

procedure TprjFile.ReadMethod(var Line: Integer; ClassItem: TprjCustomClass;
  ClassSection: TClassSection);
var
  NewMethod: TprjMethod;
begin

  NewMethod := ClassItem.Methods.AddNew;
  NewMethod.InterfaceLine := Line;
  NewMethod.ClassSecion := ClassSection;
  NewMethod.SourceLine := FData[Line];
  NewMethod.ItemName := CreateItemName(FData[Line]);

  while (Pos('(' ,NewMethod.SourceLine) > 0) and
    (Pos(')' ,NewMethod.SourceLine) = 0) do
  begin
    Inc(Line);
    NewMethod.SourceLine := NewMethod.SourceLine + #13#10 +
      FData[Line];
  end;

end;

procedure TprjFile.ReadConst(var Line: Integer);
var
  ConstLine: string;
  CurrentLine: string;
  NewConst: TprjConstant;
  DelimPos: Integer;
  TempPos: Integer;
begin

  { Alleen 'const'? }
  if (Copy(Trim(LowerCase(FData[Line])), 1, 5) = 'const') or
     (Copy(Trim(LowerCase(FData[Line])), 1, 14) = 'resourcestring') then
    Inc(Line);

  repeat
    CurrentLine := FData[Line];

    if (ConstLine = '') and (CurrentLine <> '') and (Pos('=', CurrentLine) = 0)
      then
    begin
      Dec(Line);
      Break;
    end;

    if CurrentLine <> '' then
      ConstLine := ConstLine + CurrentLine + #13#10;

    if Copy(CurrentLine, Length(CurrentLine), 1) = ';' then
    begin
      NewConst := FConstants.AddNew;

      DelimPos := Pos('=', ConstLine);

      TempPos := Pos(':', ConstLine);
      if (TempPos > 0) and (TempPos < DelimPos) then
        DelimPos := TempPos;

      NewConst.ItemName := Trim(Copy(ConstLine, 1, DelimPos - 1));
      NewConst.SourceLine := StringToHtml(ConstLine);

      ConstLine := '';
    end;

    Inc(Line);
  until Line >= FData.Count - 1;

end;

procedure TprjFile.ReadType(var Line: Integer);
var
  NewType: TprjType;
  DelimPos: Integer;
  NewPos: Integer;
begin

  NewType := FTypes.AddNew;

  DelimPos := TypePos(Line);

  NewPos := Pos(':', FData[Line]);

  if (NewPos <> 0) and (NewPos < DelimPos) then
    DelimPos := NewPos;

  NewType.ItemName := Trim(Copy(FData[Line], 1, DelimPos - 1));
  NewType.SourceLine := FData[Line];

end;

function TprjFile.ClassByName(const ClassName: string): TprjClass;
var
  i: Integer;
begin

  Result := nil;

  for i := 0 to FClasses.Count - 1 do
  begin
    if CompareText(FClasses[i].ItemName, ClassName) = 0 then
    begin
      Result := FClasses[i];
      Break;
    end;
  end;

end;

function TprjFile.InterfaceByName(const ClassName: string): TprjInterface;
var
  i: Integer;
begin

  Result := nil;

  for i := 0 to FInterfaces.Count - 1 do
  begin
    if CompareText(FInterfaces[i].ItemName, ClassName) = 0 then
    begin
      Result := FInterfaces[i];
      Break;
    end;
  end;

end;

function TprjFile.CreateUnitList: TprjUnitList;
begin

  Result := TprjUnitList.Create(Self, False);

end;

procedure TprjFile.FindDescription;
var
  i: Integer;
begin

  inherited;

  for i := 0 to 3 do
    if FindDescriptionFwd(FDescription, i) then
      Break;
end;

function TprjFile.FindDescriptionFwd(Lines: TStrings; StartLine: Integer):
  Boolean;
var
  i: Integer;
  Line: string;
begin
  Result := False;

  if StartLine >= FData.Count then
    Exit;

  InitCommentSearch;

  Line := FData[StartLine];
  Result := IsCommentLine(Line);

  if not Result then
    Exit;

  InitCommentSearch;

  i := StartLine;
  while i <= FData.Count - 1 do
  begin
    Line := FData[i];

    if IsCommentLine(Line) then
      Lines.Add(Line)
    else
      Break;

    Inc(i);
  end;

end;

function TprjFile.FindDescriptionBwd(Lines: TStrings; EndLine: Integer):
  Boolean;
var
  Line: string;
  StartLine: Integer;
begin
  InitCommentSearch;

  StartLine := -1;

  Line := FData[EndLine];

  if IsCommentLine(Line) then
  begin
    Result := True;

    { Commentaar met begin en eindhaken op 1 regel }
    if IsEndCommentLine(Line) then
    begin
      Lines.Add(Line);
      Exit;
    end
    else
    begin
      { Commentaar met slashes, zoek (achteruit) naar eerste
        niet-commentaar regel }
      StartLine := EndLine - 1;

      while (StartLine > 0) do
      begin
        Line := FData[StartLine];

        if not IsCommentLine(Line) then
        begin
          { Eentje te ver doorgeschoten }
          Inc(StartLine);
          Break;
        end;

        Dec(StartLine);
      end;
    end;
  end
  else if IsEndCommentLine(Line) then
  begin
    { Einde commentaar gevonden, zoek naar startcomment }
    StartLine := EndLine - 1;

    while (StartLine > 0) do
    begin
      Line := FData[StartLine];

      if IsCommentLine(Line) then
        Break;

      Dec(StartLine);
    end;
  end;

  if StartLine = -1 then
    Result := False
  else
    Result := FindDescriptionFwd(Lines, StartLine);
end;

procedure TprjFile.InitCommentSearch;
begin

  FStartComment := '';

end;

function TprjFile.IsCommentLine(var Line: string): Boolean;
begin

  Result := False;

  if FStartComment = '' then
  begin
    if Copy(Trim(Line), 1, 2) = '//' then
    begin
      Result := True;
      FStartComment := '//';
    end
    else if Copy(Trim(Line), 1, 2) = '(*' then
    begin
      Result := True;
      FStartComment := '(*'
    end
    else if (Copy(Trim(Line), 1, 1) = '{') and not
      (Copy(Trim(Line), 2, 1) = '$') then
    begin
      Result := True;
      FStartComment := '{'
    end
    else if (Length(Trim(Line)) > 1) and (Copy(Line, Length(Line), 1) = '{')
      then
    begin
      Result := False;
      FStartComment := '{'
    end;

    if Result and (not (FStartComment = '//')) then
      Line := StringReplace(Line, FStartComment, ' ', []);
  end;

  if FStartComment = '//' then
  begin
    Result := Copy(Trim(Line), 1, 2) = '//';

    if Result then
      Line := StringReplace(Line, '//', '', []);

  end
  else if FStartComment <> '' then
  begin
    Result := True;
    if IsEndCommentLine(Line) then
    begin
      FStartComment := '';
      Line := StringReplace(Line, FEndComment, '', []);
    end;
  end;

end;

function TprjFile.IsEndCommentLine(const Line: string): Boolean;
begin

  Result := False;

  if (FStartComment = '(*') or (FStartComment = '') then
  begin
    Result := Copy(Line, Length(Line) - 1, 2) = '*)';

    if Result then
      FEndComment := '*)';

  end;

  if not Result then
  begin
    Result := Copy(Line, Length(Line), 1) = '}';

    if Result then
      FEndComment := '}';
  end;

end;

function TprjFile.GetProcedures: TprjMethodList;
begin
  Result := FDummyClass.Methods;
end;

{ TprjFileList }

function TprjFileList.AddNew: TprjFile;
begin
  if Project <> nil then
    Result := TprjFile.Create(Project)
  else
    Result := TprjFile.Create(ProjectGroup);

  inherited Add(Result);
end;

function TprjFileList.GetItems(Index: Integer): TprjFile;
begin

  Result := TprjFile(inherited GetItem(Index));

end;

function TprjFileList.FileByName(const FileName: string): TprjFile;
var
  i: Integer;
begin

  Result := nil;

  for i := 0 to Count - 1 do
  begin
    if CompareText(Items[i].FileName, FileName) = 0 then
    begin
      Result := Items[i];
      Break;
    end;
  end;

end;

{ TprjProject }

procedure TprjProject.FillRecursiveUnitList;
var
  i: Integer;
begin
  inherited;

  for i := 0 to FRecursiveUnitList.Count-1 do
    FRecursiveUnitList[i].UsedByProjects.Add(Self)
end;

procedure TprjProject.FindInterfaces;
var
  ClassIndex: Integer;
  UnitIndex: Integer;
  prjUnit: TprjUnit;
begin
  for UnitIndex := 0 to RecursiveUnitList.Count - 1 do
  begin
    prjUnit := RecursiveUnitList[UnitIndex];

    for ClassIndex := 0 to prjUnit.Classes.Count - 1 do
      prjUnit.Classes[ClassIndex].FindParent;

    for ClassIndex := 0 to prjUnit.Classes.Count - 1 do
      prjUnit.Classes[ClassIndex].FindInterfaces;

    for ClassIndex := 0 to prjUnit.Interfaces.Count - 1 do
      prjUnit.Interfaces[ClassIndex].FindParent;
  end;
end;

procedure TprjProject.FindUses;
var
  CurrentDir: string;
  i: Integer;
begin

  CurrentDir := GetCurrentDir;
  SetCurrentDir(ExtractFilePath(FileName));

  FUnits.AutoLoad := False;

  try
    inherited;

    FUnits.AutoLoad := True;
    { Bij een projectfile moeten eerst alle uses worden ingelezen en dan pas
      worden verwerkt zodat de uses naar de bestanden kunnen verwijzen die niet
      in het path staan, maar wel in de projctfile }
    for i := 0 to FUnits.Count-1 do
      FUnits[i].LoadFromFile;
  finally
    SetCurrentDir(CurrentDir);
  end;

  FillRecursiveUnitList;
end;

function TprjProject.GetSearchPath: string;
begin

  Result := inherited GetSearchPath;

  if ProjectGroup <> nil then
    Result := Result + ProjectGroup.SearchPath;

end;

procedure TprjProject.LoadFromFile;
var
  DOFFile: string;
begin
{$IFDEF DEBUG}
  Debugger.EnterProc('TprjProject.LoadFromFile');
{$ENDIF}

  // mhemmes: inherited call moved to end of procedure
  // inherited;

  DOFFile := ChangeFileExt(FileName, '.dof');

  if FileExists(DOFFile) then
  begin
    with TIniFile.Create(DOFFile) do
    try
      FSearchPath := ReadString('Directories', 'SearchPath', '');
    finally
      Free;
    end;
  end;

  // mhemmes: inherited call moved from begin of procedure
  inherited;

{$IFDEF DEBUG}
  Debugger.LeaveProc('TprjProject.LoadFromFile');
{$ENDIF}
end;

{ TPrjGroup }

constructor TPrjGroup.Create;
begin
  inherited;

  FProjects := TprjProjectList.Create(self);
  FRecursiveClassList := TprjClassList.Create(Self, False);
  FRecursiveProcedureList := TprjMethodList.Create(Self, False);
  FRecursiveTypeList := TprjTypeList.Create(Self, False);
  FRecursiveInterfaceList := TprjInterfaceList.Create(Self, False);
end;

function TPrjGroup.CreateUnitList: TprjUnitList;
begin
  Result := TprjUnitList.Create(Self, True);
end;

destructor TPrjGroup.Destroy;
begin
  FRecursiveInterfaceList.Free;
  FRecursiveClassList.Free;
  FRecursiveProcedureList.Free;
  FRecursiveTypeList.Free;
  FProjects.Free;

  inherited;
end;

procedure TPrjGroup.FillRecursiveClassList;
var
  ClassIndex: Integer;
  UnitIndex: Integer;
begin
  FRecursiveClassList.Clear;

  for UnitIndex := 0 to RecursiveUnitList.Count - 1 do
    for ClassIndex := 0 to RecursiveUnitList[UnitIndex].Classes.Count - 1 do
      FRecursiveClassList.Add(
        RecursiveUnitList[UnitIndex].Classes[ClassIndex]);
end;

procedure TPrjGroup.FillRecursiveProcedureList;
var
  ProcedureIndex: Integer;
  UnitIndex: Integer;
begin
  FRecursiveProcedureList.Clear;

  for UnitIndex := 0 to RecursiveUnitList.Count - 1 do
    for ProcedureIndex := 0 to RecursiveUnitList[UnitIndex].Procedures.Count - 1 do
      FRecursiveProcedureList.Add(
        RecursiveUnitList[UnitIndex].Procedures[ProcedureIndex]);
end;

procedure TPrjGroup.FillRecursiveTypeList;
var
  TypeIndex: Integer;
  UnitIndex: Integer;
begin
  FRecursiveTypeList.Clear;

  for UnitIndex := 0 to RecursiveUnitList.Count - 1 do
    for TypeIndex := 0 to RecursiveUnitList[UnitIndex].Types.Count - 1 do
      FRecursiveTypeList.Add(
        RecursiveUnitList[UnitIndex].Types[TypeIndex]);
end;

procedure TPrjGroup.FillRecursiveInterfaceList;
var
  InterfaceIndex: Integer;
  UnitIndex: Integer;
begin
  FRecursiveInterfaceList.Clear;

  for UnitIndex := 0 to RecursiveUnitList.Count - 1 do
    for InterfaceIndex := 0 to RecursiveUnitList[UnitIndex].Interfaces.Count - 1
      do
      FRecursiveInterfaceList.Add(
        RecursiveUnitList[UnitIndex].Interfaces[InterfaceIndex]);
end;

procedure TPrjGroup.FindUses;
  procedure FindProject(const ProjectName: string);
  var
    ProjectFile: string;
    i: Integer;
  begin
    for i := 0 to FData.Count - 1 do
    begin
      if Copy(FData[i], 1, Length(ProjectName) + 1) = ProjectName + ':' then
      begin
        ProjectFile := Copy(FData[i], Pos(':', FData[i]) + 1, Length(FData[i]));
        ProjectFile := Trim(ProjectFile);

        ProjectFile := ExpandFileName(ProjectFile);

        with NewProject do
          FileName := ProjectFile;
      end;
    end;
  end;

var
  i: Integer;
  ProjectsLine: string;
  CurrentDir: string;
  List: TStringList;
begin
{$IFDEF DEBUG}
  Debugger.EnterProc('TPrjGroup.FindUses');
{$ENDIF}

  inherited;

  i := 0;

  while i < FData.Count - 1 do
  begin
    if CompareText(Copy(FData[i], 1, 8), 'PROJECTS') = 0 then
    begin
      ProjectsLine := '';

      repeat
        ProjectsLine := ProjectsLine + FData[i];

        if Copy(FData[i], Length(FData[i]), 1) <> '\' then
          Break;

        Inc(i);
      until i = FData.Count - 1;

      Break;
    end;

    Inc(i);
  end;

  if ProjectsLine <> '' then
  begin
    CurrentDir := GetCurrentDir;
    SetCurrentDir(ExtractFilePath(FileName));
    try
      List := TStringList.Create;
      try
        StrTokenToStrings(ProjectsLine, ' ', List);
        { TODO : Testen ivm met JCL, waarom beginnen bij 3? }
        for i := 2 to List.Count-1 do
          FindProject(List[i]);
      finally
        List.Free;
      end;
    finally
      SetCurrentDir(CurrentDir);
    end;
  end;

{$IFDEF DEBUG}
  Debugger.LeaveProc('TPrjGroup.FindUses');
{$ENDIF}
end;

procedure TPrjGroup.LoadFromFile;
var
  i: Integer;
begin
  if FData.Count > 0 then
    Exit;

{$IFDEF DEBUG}
  Debugger.EnterProc('TPrjGroup.LoadFromFile');
{$ENDIF}

  if FileName <> '' then
    inherited;

  for i := 0 to FProjects.Count - 1 do
  begin
    if Assigned(FOnLoadFile) then
      FOnLoadFile(Self, i);

    FProjects[i].LoadFromFile;
  end;

  FillRecursiveUnitList;
  FillRecursiveProcedureList;
  FillRecursiveTypeList;
  FillRecursiveInterfaceList;
  FillRecursiveClassList;

  for i := 0 to RecursiveUnitList.Count -1 do
    RecursiveUnitList[i].FindKeywords;

{$IFDEF DEBUG}
  Debugger.LeaveProc('TPrjGroup.LoadFromFile');
{$ENDIF}
end;

function TPrjGroup.NewProject: TprjProject;
begin
  Result := FProjects.AddNew;
end;

{ TprjUnit }

procedure TprjUnit.FindDescription;
var
  i: Integer;
begin
  inherited;

  for i := 0 to Classes.Count - 1 do
    Classes[i].FindDescription;
end;

procedure TprjUnit.FindKeywords;
var
  i: Integer;
begin
  inherited;

  for i := 0 to Classes.Count - 1 do
    Classes[i].FindKeywords;
end;

function TprjUnit.GetSearchPath: string;
begin
  Result := inherited GetSearchPath;

  if Project <> nil then
    Result := Result + Project.SearchPath;
end;

function TprjProject.NewUnit(const FileName: string): TprjUnit;
begin
  Result := inherited NewUnit(FileName);
end;

{ TprjClass }

constructor TprjClass.Create(AOwner: TprjItem);
begin
  inherited;
  FInterfaces := TprjInterfaceList.Create(Self, False);
end;

destructor TprjClass.Destroy;
begin
  FInterfaces.Free;
  inherited;
end;

procedure TprjClass.AddInterface(prjInterface: TprjInterface);
begin
  FInterfaces.Add(prjInterface);
  prjInterface.AddClass(self);
end;

procedure TprjClass.FindInterfaces;
var
  SuppInterface: TprjInterface;
  i: Integer;
  InterfaceName: string;
  ParentIndex: Integer;
  List: TStringList;
begin

  if prjFile <> nil then
  begin
    List := TStringList.Create;
    try
      StrTokenToStrings(Parents, ',', List);

      { TODO : Testen ivm met JCL }
      for ParentIndex := 1 to List.Count-1 do
      begin
        InterfaceName := List[ParentIndex];

        SuppInterface := prjFile.InterfaceByName(InterfaceName);

        if SuppInterface = nil then
          { Interface staat niet in deze unit, zoeken in used units }
          for I := 0 to prjFile.Units.Count - 1 do
          begin
            SuppInterface := prjFile.Units[i].InterfaceByName(InterfaceName);

            if SuppInterface <> nil then
              Break;
          end;

        if SuppInterface <> nil then
        begin
          AddInterface(SuppInterface);
          FKeywords.Add(InterfaceName, SuppInterface);
        end;
      end;
    finally
      List.Free;
    end;
  end;
end;

procedure TprjClass.FindParent;
var
  i: Integer;
begin

  inherited;

  if prjFile <> nil then
  begin
    ParentClass := prjFile.ClassByName(ParentName);

    if ParentClass = nil then
      for I := 0 to prjFile.Units.Count - 1 do
      begin
        ParentClass := prjFile.Units[i].ClassByName(ParentName);

        if ParentClass <> nil then
          Break;
      end;
  end;

end;

function TprjClass.FindKeyLink(const Keyword: string): TprjItem;
begin
  Result := FInterfaces.ItemByName(Keyword);

  if Result = nil then
    Result := inherited FindKeyLink(Keyword);
end;

{ TprjPropertyList }

function TprjPropertyList.AddNew: TprjProperty;
begin

  Result := TprjProperty.Create(prjClass);
  inherited Add(Result);

end;

function TprjPropertyList.GetItems(Index: Integer): TprjProperty;
begin

  Result := TprjProperty(inherited GetItem(Index));

end;

{ TprjItem }

constructor TprjItem.Create(AOwner: TprjItem);
begin
  inherited Create;
  FOwner := AOwner;
  FDescription := TStringList.Create;
  FKeywords := TKeywordList.Create;
  FTag := TagCounter;
  Inc(TagCounter);
end;

destructor TprjItem.Destroy;
begin

  FKeywords.Free;
  FDescription.Free;

  inherited;

end;

procedure TprjItem.FindDescription;
begin
  FDescription.Clear;
end;

function TprjItem.FindKeyLink(const Keyword: string): TprjItem;
begin

  Result := nil;

  if ProjectGroup <> nil then
  begin
    Result := ProjectGroup.RecursiveClassList.ItemByName(Keyword);

    if Result = nil then
      Result := ProjectGroup.RecursiveInterfaceList.ItemByName(Keyword);

    if Result = nil then
      Result := ProjectGroup.RecursiveUnitList.ItemByName(Keyword);

    if Result = nil then
      Result := ProjectGroup.RecursiveTypeList.ItemByName(Keyword);

  end;
end;

procedure TprjItem.FindKeywords;
var
  Keyword: string;
  KeywordPos: Integer;
  EndPos: Integer;
  Text: string;
  i: Integer;
  KeyLink: TprjItem;
  List: TStringList;
  Buffer: string;

const
  Delimiters = [':', ';', ')', ' '];
begin
  Text := FDescription.Text;

  repeat
    KeywordPos := Pos('[', Text);

    if KeywordPos > 0 then
    begin
      EndPos := Pos(']', Text);

      if EndPos > KeywordPos then
      begin
        Keyword := Copy(Text, KeywordPos + 1, EndPos - KeywordPos - 1);
        FKeywords.Add(Keyword, FindKeyLink(Keyword));
        Delete(Text, KeywordPos, EndPos - KeywordPos + 1);
      end
      else
        Delete(Text, EndPos, 1);
    end;
  until KeywordPos = 0;

  Buffer := SourceLine;
  List := TStringList.Create;
  try
    Buffer := StrReplaceChar(Buffer, ';', ' ');
    Buffer := StrReplaceChar(Buffer, ':', ' ');
    Buffer := StrReplaceChar(Buffer, ')', ' ');
    StrToStrings(Buffer, ' ', List);

    for i := 0 to List.Count-1 do
    begin
      Keyword := List[i];

      if Keyword <> ItemName then
      begin
        KeyLink := FindKeyLink(Keyword);

        if KeyLink <> nil then
          FKeywords.Add(Keyword, KeyLink);
      end
    end;
  finally
    List.Free;
  end;

end;

function TprjItem.GetprjFile: TprjFile;
begin

  if Owner is TprjFile then
    Result := TprjFile(Owner)
  else if Owner <> nil then
    Result := Owner.prjFile
  else
    Result := nil;

end;

function TprjItem.GetProject: TprjProject;
begin

  if Owner is TprjProject then
    Result := TprjProject(Owner)
  else if prjFile <> nil then
    Result := prjFile.Project
  else if Owner <> nil then
    Result := Owner.Project
  else
    Result := nil;

end;

function TprjItem.GetProjectGroup: TPrjGroup;
begin

  if Owner is TPrjGroup then
    Result := TPrjGroup(Owner)
  else if (Project <> nil) and (Project.ProjectGroup <> nil) then
    Result := Project.ProjectGroup
  else if prjFile <> nil then
    Result := prjFile.ProjectGroup
  else if Owner <> nil then
    Result := Owner.ProjectGroup
  else
    Result := nil;

end;

procedure TprjItem.SetDescription(const Value: TStrings);
begin

  FDescription.Assign(Value);

end;

procedure TprjItem.SetSourceLine(const Value: string);
begin
  FSourceLine := Value;
end;

{ TPrjClassItem }

procedure TPrjClassItem.FindDescription;
begin
  inherited;

  if not prjFile.FindDescriptionBwd(FDescription, FInterfaceLine - 1) then
    prjFile.FindDescriptionBwd(FDescription, FInterfaceLine - 2);
end;

function TPrjClassItem.FindKeyLink(const Keyword: string): TprjItem;
begin
  Result := prjClass.FindKeyLink(Keyword);

  if Result = nil then
    Result := inherited FindKeyLink(Keyword);
end;

function TPrjClassItem.GetprjClass: TprjCustomClass;
begin

  if Owner is TprjCustomClass then
    Result := TprjCustomClass(Owner)
  else
    Result := nil;

end;

{ TPrjClassItemList }

function TPrjClassItemList.GetprjClass: TprjCustomClass;
begin

  if Owner is TprjCustomClass then
    Result := TprjCustomClass(Owner)
  else
    Result := nil;

end;

{ TprjMethod }

constructor TprjMethod.Create(AOwner: TprjItem);
begin
  inherited;
  FImplementLine := -1;
end;

{ Zoekt de omschrijving in de implementatie van de unit }
procedure TprjMethod.FindDescription;
begin
  if FImplementLine = -1 then
    FindImplement;

  if not ((FImplementLine > -1) and
    (prjFile.FindDescriptionBwd(FDescription, FImplementLine-1) or
    prjFile.FindDescriptionBwd(FDescription, FImplementLine - 2))) then
    inherited;
end;

procedure TprjMethod.FindImplement;
var
  FindLine: string;
  i: Integer;
  ParentFile: TprjFile;
begin
  ParentFile := prjFile;

  FindLine := StrBefore(' ', Trim(SourceLine)) + ' ';

  if not (prjClass is TprjDummyClass) then
    FindLine := FindLine + prjClass.ItemName + '.';

  FindLine := FindLine + ItemName;

  FImplementLine := -1;

  for i := ParentFile.ImplementStart to ParentFile.Data.Count - 1 do
    if CompareText(Copy(ParentFile.Data[i], 1, Length(FindLine)), FindLine) = 0
      then
    begin
      FImplementLine := i;
      Break;
    end;
end;

procedure TprjMethod.SetSourceLine(const Value: string);
var
  Line: string;
begin
  inherited;

  Line := LowerCase(Value);
  FIsVirtual := (Pos('virtual;', Line) > 0) or (Pos('dynamic;', Line) > 0)
end;

{ TprjInterfaceList }

function TprjInterfaceList.AddNew: TprjInterface;
begin
  Result := TprjInterface.Create(prjFile);
  inherited Add(Result);
end;

function TprjInterfaceList.GetItems(Index: Integer): TprjInterface;
begin
  Result := TprjInterface(inherited GetItem(Index));
end;

{ TprjInterface }

procedure TprjInterface.AddClass(prjClass: TprjClass);
begin
  FClasses.Add(prjClass);
end;

constructor TprjInterface.Create(AOwner: TprjItem);
begin
  inherited;

  FClasses := TprjClassList.Create(Self, False);
end;

destructor TprjInterface.Destroy;
begin

  FClasses.Free;

  inherited;

end;

procedure TprjInterface.FindParent;
var
  I: Integer;
begin

  inherited;

  if prjFile <> nil then
  begin
    ParentClass := prjFile.InterfaceByName(ParentName);

    if ParentClass = nil then
      for I := 0 to prjFile.Units.Count - 1 do
      begin
        ParentClass := prjFile.Units[i].InterfaceByName(ParentName);

        if ParentClass <> nil then
          Break;
      end;
  end;

end;

{ TprjCustomClass }

constructor TprjCustomClass.Create(AOwner: TprjItem);
begin

  inherited;
  FMethods := TprjMethodList.Create(self);
  FProperties := TprjPropertyList.Create(self);
  FChildClasses := TprjClassList.Create(Owner, False);

end;

destructor TprjCustomClass.Destroy;
begin
  FChildClasses.Free;
  FProperties.Free;
  FMethods.Free;

  inherited;
end;

procedure TprjCustomClass.FindDescription;
var
  i: Integer;
begin
  inherited;

  for i := FInterfaceLine - 1 downto FInterfaceLine - 3 do
    if prjFile.FindDescriptionBwd(FDescription, i) then
      Break;

  for i := 0 to Methods.Count - 1 do
    Methods[i].FindDescription;

  for i := 0 to Properties.Count - 1 do
    Properties[i].FindDescription;
end;

function TprjCustomClass.FindKeyLink(const Keyword: string): TprjItem;
begin
  Result := FMethods.ItemByName(Keyword);

  if Result = nil then
    Result := FProperties.ItemByName(Keyword);

  if Result = nil then
    Result := inherited FindKeyLink(Keyword);
end;

procedure TprjCustomClass.FindKeywords;

  procedure DoFind(Items: TprjItemList);
  var
    i: Integer;
  begin
    for i := 0 to Items.Count - 1 do
      Items[i].FindKeywords;
  end;
begin
  inherited;

  DoFind(Methods);
  DoFind(Properties);

  if ParentClass <> nil then
    FKeywords.Add(ParentClass.ItemName, ParentClass);
end;

procedure TprjCustomClass.FindParent;
begin
end;

function TprjCustomClass.GetParentName: string;
begin
  Result := StrBefore(',', Trim(Parents));
end;

function TprjCustomClass.InheritsFrom(const ClassName: string): Boolean;
begin
  Result := CompareText(ClassName, ParentName) = 0;
end;

procedure TprjCustomClass.SetParentClass(const Value: TprjCustomClass);
begin

  if FParentClass <> nil then
    FParentClass.ChildClasses.Remove(Self);

  FParentClass := Value;

  if Value <> nil then
    FParentClass.ChildClasses.Add(self);

end;

{ TprjCustomClassList }

function TprjCustomClassList.GetItems(Index: Integer): TprjCustomClass;
begin

  Result := TprjCustomClass(inherited GetItem(Index));

end;

{ TKeywordItem }

constructor TKeywordItem.Create(AKeyword: string; AItem: TprjItem);
begin
  inherited Create;
  FKeyword := AKeyword;
  FItem := AItem;
end;

{ TKeywordList }

procedure TKeywordList.Add(AKeyword: string; AItem: TprjItem);
begin
  if (AKeyword = '') or (Pos('(', AKeyword) > 0) or (Pos('//', AKeyword) > 0) then
    Exit;

  AKeyword := StringReplace(AKeyword, ',', '', [rfReplaceAll]);

  inherited Add(TKeywordItem.Create(AKeyword, AItem));
end;

function TKeywordList.GetItems(Index: Integer): TKeywordItem;
begin
  Result := TKeywordItem(inherited GetItem(Index));
end;

{ TprjTypeList }

function TprjTypeList.AddNew: TprjType;
begin

  Result := TprjType(inherited AddNew);

end;

function TprjTypeList.GetItems(Index: Integer): TprjType;
begin
  Result := TprjType(inherited GetItem(Index));
end;

{ TprjConstantList }

function TprjConstantList.AddNew: TprjConstant;
begin

  Result := TprjConstant(inherited AddNew);

end;

function TprjConstantList.GetItems(Index: Integer): TprjConstant;
begin

  Result := TprjConstant(inherited GetItem(Index));

end;

initialization
  FileCache := TStringList.Create;

finalization
  FileCache.Free;

end.

