unit EditorComponentUnit;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls, deType, ddType, ddPlugin_TLB, FMX.Layouts, FMX.Objects,
  FMX.Grid;

type
  TEditorComponent = class;

  TMsgType = (mtError, mtWarnign, mtInfo);

  TMsgInfoType = (mtAll, mtEnter, mtExit, mtValidace, mtShow, mtButton, mtEnterUser, mtExitUser, mtValidaceUser,
    mtShowUser, mtButtonUser);

  TMessage = class(TLabel)
  private
    FMsgType: TMsgType;
    FMsg: String;
    FDoAction: String;
    FMsgId: Integer;
    FMsgDruh: TMsgInfoType;
    procedure SetMsgType(const Value: TMsgType);
    procedure SetMsg(const Value: String);
    procedure SetDoAction(const Value: String);
    procedure SetMsgId(const Value: Integer);
    procedure SetMsgDruh(const Value: TMsgInfoType);
  protected
  published
    property MsgDruh: TMsgInfoType read FMsgDruh write SetMsgDruh;
    property MsgId: Integer read FMsgId write SetMsgId;
    property MsgType: TMsgType read FMsgType write SetMsgType;
    property Msg: String read FMsg write SetMsg;
    property DoAction: String read FDoAction write SetDoAction;
  end;

  THlaskaComponent = class(TLayout)
  private
    fEdit: TEditorComponent;
    fMessages: TStringList;
    fImage: TImage;
    fListMsg: TCalloutPanel;
    fGrid: TGrid;
    fAutoRemove: Boolean;
    fImgWarnig, fImgError, fImgInfo, fImgDismis, fImgDo: TImage;
    fDismisList: TStringList;
  protected
    procedure doMessage;
    procedure doClick(Sender: TObject);
    procedure Grid1GetValue(Sender: TObject; const Col, Row: Integer; var Value: Variant);
    procedure CreateListMsg;
  published
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure addMessage(aType: TMsgInfoType; aMsgId: Integer; aMsg: String; aMsgType: TMsgType; aDoAction: String);
    procedure AddHlaska(aType: TMsgInfoType; aQuery: IHeQuery);
    procedure removeMessages(aType: TMsgInfoType);
    function Hlasky: Boolean;
  end;

  TEditorComponent = class(TLabel)
  private
    { Private declarations }
    fonShow: string;
    fonExit: string;
    fEditType: TTypDefEditAtr;
    fHelios: IHelios;
    fonValidate: string;
    fonEnter: string;
    fColumnName: string;
    fEdit: TStyledControl; // odkaz na edit komponentu
    fHlaska: THlaskaComponent; // odkaz na komponentu s hlsenim chyb a warningu
    fonButton: string;
    fSpaces: single;
    fTempTableName: string;
    fModified: Boolean;
    fBrowse: TBrowse;
    fGUID: TGUID;
    fonExitUser: string;
    fonButtonUser: string;
    fonValidateUser: string;
    fonEnterUser: string;
    fonShowUser: string;

    procedure createEditor;
    procedure SetOnButton(const Value: string);
    function getValue: Variant;
    procedure setValue(const Value: Variant);
    procedure setSpaces(const Value: single);
    procedure SetOnButtonUser(const Value: string);
  protected
    { Protected declarations }
    procedure Resize; override;

    // detekce editace
    procedure EditChange(Sender: TObject);
    procedure EditEnter(Sender: TObject);
    procedure EditExit(Sender: TObject);
  public
    { Public declarations }
    procedure reloadFromTmp;
    function doPrepareSql(aSQL: String): String;
  published
    { Published declarations }
    constructor CreateEdit(AOwner: TComponent; aColumnName, aTempTableName: string; aEditType: TTypDefEditAtr;
      aHelios: IHelios; aBrowse: TBrowse = bZadny);

    // editacni pole
    property onShow: string read fonShow write fonShow;
    property onEnter: string read fonEnter write fonEnter;
    property onExit: string read fonExit write fonExit;
    property onValidate: string read fonValidate write fonValidate;
    property onButton: string read fonButton write SetOnButton;
    property onShowUser: string read fonShowUser write fonShowUser;
    property onEnterUser: string read fonEnterUser write fonEnterUser;
    property onExitUser: string read fonExitUser write fonExitUser;
    property onValidateUser: string read fonValidateUser write fonValidateUser;
    property onButtonUser: string read fonButtonUser write SetOnButtonUser;
    property GUID: TGUID read fGUID write fGUID;
    property EditType: TTypDefEditAtr read fEditType;
    property Helios: IHelios read fHelios;
    property ColumnName: string read fColumnName;
    property TempTableName: string read fTempTableName;
    property Value: Variant read getValue write setValue;
    property Modified: Boolean read fModified default false;
    property Edit: TStyledControl read fEdit;
    property Hlaska: THlaskaComponent read fHlaska;

    function Save: Boolean;
    function Validate: Boolean;
    function Hlasky: Boolean;

    // design values
    property spaces: single read fSpaces write setSpaces;
  end;

procedure Register;

implementation

uses FMX.Edit, FMX.ExtCtrls, Variants, sqString, HeGrid, FMX.Platform, FMX.Effects, System.Types;

procedure Register;
begin
  RegisterComponents('HacunaMatata', [TEditorComponent, THlaskaComponent]);
end;

{ TEditorComponent }

constructor TEditorComponent.CreateEdit(AOwner: TComponent; aColumnName, aTempTableName: string;
  aEditType: TTypDefEditAtr; aHelios: IHelios; aBrowse: TBrowse = bZadny);
begin
  fColumnName := aColumnName;
  fEditType := aEditType;
  fHelios := aHelios;
  fTempTableName := aTempTableName;
  fBrowse := aBrowse;
  inherited Create(AOwner);

  Text := fColumnName;
  Top := 8;
  Left := 16;
  Width := 170;
  VertTextAlign := TTextAlign.taLeading;
  fSpaces := 6;

  Margins.Right := -5;

  if fEditType = taBrowse then
  begin
    Align := TAlignLayout.alClient;
    Padding.Left := 8;
    Padding.Top := 4;
    Padding.Right := Padding.Right + 8;
    Padding.Bottom := 4;
  end;

  fEdit := nil;
  createEditor;

  fHlaska := THlaskaComponent.Create(self);
  fHlaska.Parent := self;
  InsertComponent(fHlaska);
end;

procedure TEditorComponent.SetOnButton(const Value: string);
begin
  fonButton := Value;
  // TODO -ojan.damek -cfunkcni: pri naplneni onButton vytvorit tlacitko
end;

procedure TEditorComponent.SetOnButtonUser(const Value: string);
begin
  fonButtonUser := Value;
  // TODO -ojan.damek -cfunkcni: pri naplneni onButton vytvorit tlacitko
end;

procedure TEditorComponent.setSpaces(const Value: single);
begin
  fSpaces := Value;
  // recalulate all component
  // TODO -ojan.damek -cfunkcni: nastaveni mezer mezi labelem a komponentou
end;

procedure TEditorComponent.setValue(const Value: Variant);
var
  DT: TDateTime;
begin
  if Assigned(fEdit) then
    case fEditType of
      TTypDefEditAtr.taInteger, TTypDefEditAtr.taFloat, TTypDefEditAtr.taMoney:
        if not VarIsNull(Value) then
          TNumberBox(fEdit).Value := Value;
      taString:
        if not VarIsNull(Value) then
          TClearingEdit(fEdit).Text := VarToStr(Value);
      taDatum, taCas, taDatumCas:
        if not VarIsNull(Value) then
          if VarIsStr(Value) then
            if TryStrToDate(VarToStr(Value), DT) then
              TCalendarEdit(fEdit).Date := DT
            else
              TCalendarEdit(fEdit).Text := VarToStr(Value)
          else
            TCalendarEdit(fEdit).Date := VarToDateTime(Value);
      taCombo:
        begin
          if not VarIsNull(Value) then
            TComboEdit(fEdit).Text := VarToStr(Value);
        end;
      taList:
        begin
          if not VarIsNull(Value) then
            TComboEditListBox(fEdit).ItemIndex := TComboEditListBox(fEdit).Items.IndexOf(VarToStr(Value));
        end;
      taBrowse:
        begin
          // TODO -ojan.damek -cfunkcni: nastaveni hodnot pro grid
          // fEdit := TGrid.Create(self);
        end;
      taLabel:
        begin
          if not VarIsNull(Value) then
            TLabel(fEdit).Text := VarToStr(Value);
        end
    else
      TLabel(fEdit).Text := VarToStr(Value);
    end;
end;

function TEditorComponent.Validate: Boolean;
begin
  if onValidate <> '' then
    fHlaska.AddHlaska(mtValidace, fHelios.OpenSQL(doPrepareSql(onValidate)));
  Result := true;
end;

function TEditorComponent.getValue: Variant;
begin
  if Assigned(fEdit) then
    case fEditType of
      taNeni:
        Result := '';
      taInteger, TTypDefEditAtr.taFloat, taMoney:
        Result := TNumberBox(fEdit).Value;
      taString:
        Result := TClearingEdit(fEdit).Text;
      taDatum, taCas, taDatumCas:
        Result := TCalendarEdit(fEdit).Date;
      taCombo:
        begin
          Result := TComboEdit(fEdit).Text;
        end;
      taList:
        begin
          if (TComboEditListBox(fEdit).ItemIndex > -1) and (TComboEditListBox(fEdit).Count > 0) and
            (TComboEditListBox(fEdit).ItemIndex < TComboEditListBox(fEdit).Count) then
            Result := TComboEditListBox(fEdit).ListItems[TComboEditListBox(fEdit).ItemIndex].Text
          else
            Result := '';
        end;
      taBrowse:
        begin
          // TODO -ojan.damek -cfunkcni: vraceni hodnoty z gridu
          // with THeGrid(fEdit) do
          // begin
          // if ID <> '' then
          // Result := VirtualTable.FieldByName(ID).AsVariant
          // else
          Result := '';
          // end;
        end;
      taLabel:
        begin
          Result := TLabel(fEdit).Text;
        end
    else
      Result := TLabel(fEdit).Text;
    end;
end;

function TEditorComponent.Hlasky: Boolean;
begin
  Result := fHlaska.Hlasky;
end;

procedure TEditorComponent.reloadFromTmp;
var
  SQL: String;
  fQuery: IHeQuery;
begin
  SQL := 'SELECT ' + fColumnName + ' FROM ' + fTempTableName;
  fQuery := fHelios.OpenSQL(SQL);
  if fQuery.RecordCount = 1 then
  begin
    if Value <> fQuery.FieldValues(0) then
    begin
      fModified := true;
      Value := fQuery.FieldValues(0);
    end;
  end;
end;

function TEditorComponent.Save: Boolean;
var
  SQL: String;
  Val: Variant;
begin
  Result := false;
  begin
    Val := getValue;
    if VarIsNull(Val) then
      SQL := 'UPDATE ' + fTempTableName + ' SET ' + fColumnName + ' = NULL'
    else if VarIsEmpty(Val) then
      SQL := 'UPDATE ' + fTempTableName + ' SET ' + fColumnName + ' = N""'
    else
      SQL := 'UPDATE ' + fTempTableName + ' SET ' + fColumnName + ' = ' + NQuotedStr(VarToStr(Val));
    fHelios.ExecSQL(SQL);
  end;
  Result := true;
end;

procedure TEditorComponent.Resize;
begin
  inherited;
  if fEditType = taBrowse then
    if Assigned(fEdit) then
      with THeGrid(fEdit) do
      begin
        Height := self.Height - self.Font.Size - fSpaces;
        Width := self.Width;
      end;
end;

procedure TEditorComponent.createEditor;
begin
  case fEditType of
    taInteger:
      begin
        fEdit := TNumberBox.Create(self);
        with TNumberBox(fEdit) do
        begin
          ValueType := TNumValueType.vtInteger;
          DecimalDigits := 0;
          Min := Low(Integer);
          Max := High(Integer);
          HorzIncrement := 1;
          OnChange := EditChange;
        end;
      end;
    TTypDefEditAtr.taFloat:
      begin
        fEdit := TNumberBox.Create(self);
        with TNumberBox(fEdit) do
        begin
          ValueType := TNumValueType.vtFloat;
          DecimalDigits := 9;
          HorzIncrement := 1;
          Min := 1E-19;
          Max := 1E19;
          OnChange := EditChange;
        end;
      end;
    taMoney:
      begin
        fEdit := TNumberBox.Create(self);
        with TNumberBox(fEdit) do
        begin
          ValueType := TNumValueType.vtFloat;
          DecimalDigits := 2;
          HorzIncrement := 0.50;
          Min := 1E-19;
          Max := 1E19;
          OnChange := EditChange;
        end;
      end;
    taString:
      begin
        fEdit := TClearingEdit.Create(self);
        with TClearingEdit(fEdit) do
        begin
          OnChange := EditChange;
        end;
      end;
    taDatum:
      begin
        fEdit := TCalendarEdit.Create(self);
        with TCalendarEdit(fEdit) do
        begin
          // TODO -ojan.damek -cfunkcni: nastaveni kalendare jenom na datum
          // Calendar.
          OnChange := EditChange;
        end;
      end;
    taCas:
      begin
        // TODO -ojan.damek -cfunkcni: nastaveni editace casu
        fEdit := TCalendarEdit.Create(self);
        with TCalendarEdit(fEdit) do
        begin
          OnChange := EditChange;
        end;
      end;
    taDatumCas:
      begin
        fEdit := TCalendarEdit.Create(self);
        with TCalendarEdit(fEdit) do
        begin
          OnChange := EditChange;
        end;
      end;
    taCombo:
      begin
        fEdit := TComboEdit.Create(self);
        with TComboEdit(fEdit) do
        begin
          OnChange := EditChange;
        end;
      end;
    taList:
      begin
        fEdit := TComboEditListBox.Create(self);
      end;
    taBrowse:
      begin
        // TODO -ojan.damek -cfunkcni: Definice vlastniho Gridu, pro vytvoreni gridu, ikon, hledani a vychozich tlacitek
        fEdit := THeGrid.CreateGrid(self, fBrowse, fHelios);
      end;
    taLabel:
      begin
        fEdit := TLabel.Create(self);
      end
  else
    fEdit := TLabel.Create(self);
    TLabel(fEdit).Text := 'bez definice typu';
  end;
  fEdit.Parent := self;
  fEdit.Width := self.Width - 28;
  fEdit.Position.Y := Font.Size + fSpaces;
  fEdit.onEnter := EditEnter;
  fEdit.onExit := EditExit;

  InsertComponent(fEdit);
  if fEdit.Parent.ChildrenCount > 2 then
    fEdit.SendToBack;

  self.Height := self.Height + 8 + fEdit.Height + 4;
end;

function TEditorComponent.doPrepareSql(aSQL: String): String;
var
  r: String;
begin
  r := StringReplace(aSQL, '__table', fTempTableName, [rfReplaceAll, rfIgnoreCase]);
  r := StringReplace(r, '__field', fColumnName, [rfReplaceAll, rfIgnoreCase]);
  Result := r;
end;

procedure TEditorComponent.EditChange(Sender: TObject);
begin
  if not fModified then
    fModified := true;
end;

procedure TEditorComponent.EditEnter(Sender: TObject);
begin
  // event pro vstup na polozku
  if Save then
  begin
    if onEnter <> '' then
      fHlaska.AddHlaska(mtEnter, fHelios.OpenSQL(doPrepareSql(onEnter)));
    if onEnterUser <> '' then
      fHlaska.AddHlaska(mtEnterUser, fHelios.OpenSQL(doPrepareSql(onEnterUser)));

    reloadFromTmp;
  end;
end;

procedure TEditorComponent.EditExit(Sender: TObject);
begin
  // event pro opusteni polozky
  if Save then
  begin

    // proces po opusteni editacniho pole
    if onExit <> '' then
      fHlaska.AddHlaska(mtExit, fHelios.OpenSQL(doPrepareSql(onExit)));
    if onExitUser <> '' then
      fHlaska.AddHlaska(mtExitUser, fHelios.OpenSQL(doPrepareSql(onExitUser)));

    // proces validace opusteneho pole
    if onValidate <> '' then
      fHlaska.AddHlaska(mtValidace, fHelios.OpenSQL(doPrepareSql(onValidate)));
    if onValidateUser <> '' then
      fHlaska.AddHlaska(mtValidaceUser, fHelios.OpenSQL(doPrepareSql(onValidateUser)));

    reloadFromTmp;
  end;
end;

{ THlaskaComponent }

procedure THlaskaComponent.addMessage(aType: TMsgInfoType; aMsgId: Integer; aMsg: String; aMsgType: TMsgType;
  aDoAction: String);
var
  Msg: TMessage;
  i: Integer;
  s: String;
begin
  s := IntToStr(Ord(aType)) + '-' + IntToStr(aMsgId);
  if fMessages.Find(s, i) then
  begin
    if i > 0 then
      fMessages.Move(i, 0);
  end
  else if not fDismisList.Find(s, i) then
  begin
    Msg := TMessage.Create(self);
    Msg.MsgDruh := aType;
    Msg.MsgType := aMsgType;
    Msg.MsgId := aMsgId;
    Msg.Msg := aMsg;
    Msg.DoAction := aDoAction;
    Msg.Text := aMsg;
    Msg.OnClick := doClick;

    fMessages.InsertObject(0, s, Msg);
  end;

  doMessage;
end;

procedure THlaskaComponent.AddHlaska(aType: TMsgInfoType; aQuery: IHeQuery);
var
  MsgType: TMsgType;
  MsgId: Integer;
  Msg: String;
  MsgDo: string;
begin
  removeMessages(aType);

  if Assigned(aQuery) and (aQuery.RecordCount > 0) then
  begin
    aQuery.First;
    while not aQuery.EOF do
    begin
      MsgType := mtError;
      MsgId := -1;
      if aQuery.FieldCount >= 3 then
      begin
        try
          MsgType := TMsgType(aQuery.Fields(0).Value);
        except
          MsgType := mtError;
        end;
        try
          MsgId := aQuery.Fields(1).Value;
        except
          MsgId := -1;
        end;
        try
          Msg := aQuery.Fields(2).Value;
        except
          Msg := '';
        end;
      end;
      if aQuery.FieldCount = 4 then
        try
          MsgDo := aQuery.Fields(3).Value;
        except
          MsgDo := '';
        end;

      if aQuery.FieldCount >= 3 then
        addMessage(aType, MsgId, Msg, MsgType, MsgDo);
      // TODO -ojan.damek -cfunkcni: Osetreni hlasky ze vracena zprava neni korektni

      aQuery.Next;
    end;
  end else
  begin
    //TODO -ojan.damek -cfunkcni: Vychozi hlaseni chyby ve scriptu, ktery nevratil zadny vysledek
  end;
end;

constructor THlaskaComponent.Create(AOwner: TComponent);
var
  i: TInnerGlowEffect;
  ts: TResourceStream;
begin
  inherited;
  fAutoRemove := false;
  Align := TAlignLayout.alRight;

  if AOwner is TEditorComponent then
    fEdit := TEditorComponent(AOwner)
  else
    fEdit := nil;
  fMessages := TStringList.Create;

  Width := 28;
  Visible := true;
  Margins.Right := 5;

  fImage := TImage.Create(self);
  fImage.Parent := self;
  fImage.Align := TAlignLayout.alCenter;
  fImage.Width := 24;
  fImage.Height := 24;
  fImage.HitTest := true;
  fImage.OnClick := doClick;
  i := TInnerGlowEffect.Create(fImage);
  i.Enabled := true;
  i.Trigger := 'IsMouseOver=true';

  fImage.InsertComponent(i);
  fImage.BringToFront;

  InsertComponent(fImage);
  fListMsg := nil;
  fGrid := nil;

  fDismisList := TStringList.Create;

  ts := TResourceStream.Create(HInstance, 'Warning', RT_RCDATA);
  fImgWarnig := TImage.Create(self);
  fImgWarnig.Bitmap.LoadFromStream(ts);
  ts.Free;

  ts := TResourceStream.Create(HInstance, 'Error', RT_RCDATA);
  fImgError := TImage.Create(self);
  fImgError.Bitmap.LoadFromStream(ts);
  ts.Free;

  ts := TResourceStream.Create(HInstance, 'Info', RT_RCDATA);
  fImgInfo := TImage.Create(self);
  fImgInfo.Bitmap.LoadFromStream(ts);
  ts.Free;

  ts := TResourceStream.Create(HInstance, 'Dismiss', RT_RCDATA);
  fImgDismis := TImage.Create(self);
  fImgDismis.Bitmap.LoadFromStream(ts);
  ts.Free;

  ts := TResourceStream.Create(HInstance, 'Doit', RT_RCDATA);
  fImgDo := TImage.Create(self);
  fImgDo.Bitmap.LoadFromStream(ts);
  ts.Free;
end;

procedure THlaskaComponent.CreateListMsg;
var
  fColumn: TColumn;
  fsb: TStatusBar;
begin
  if self.Parent.Parent is TPanel then
  begin
    fListMsg := TCalloutPanel.Create(self.Parent.Parent);
    fListMsg.Parent := self.Parent.Parent;
    self.Parent.Parent.InsertComponent(fListMsg);
  end
  else
  begin
    fListMsg := TCalloutPanel.Create(self.Parent);
    fListMsg.Parent := self.Parent;
    self.Parent.InsertComponent(fListMsg);
  end;

  fListMsg.Width := 393;
  fListMsg.Height := 137;
  fListMsg.Margins.Rect := TRectF.Create(2, 13, 2, 2);
  fListMsg.TabOrder := 1;
  fListMsg.CalloutWidth := 23;
  fListMsg.CalloutLength := 11;
  fListMsg.CalloutOffset := 10;

  if Assigned(fEdit) then
  begin
    fListMsg.Position.X := fEdit.Position.X + fEdit.Width - 24;
    fListMsg.Position.Y := fEdit.Position.Y + fEdit.Height - 4;
  end
  else
  begin
    fListMsg.Position.X := (TStyledControl(Owner).Width / 2) - (fListMsg.Width/2);
    fListMsg.Position.Y := ((TStyledControl(Owner).Height / 2) - (fListMsg.Height/2))*-1;
    if abs(fListMsg.Position.Y)<abs(fListMsg.Height) then
      fListMsg.Position.Y := (fListMsg.Height + 20)*-1;
    fListMsg.CalloutLength:=0;
    fListMsg.CalloutOffset:=0;
  end;

  fsb := TStatusBar.Create(fListMsg);
  fsb.Parent := fListMsg;
  fsb.Position.Point := TPointF.Create(2, 123);
  fsb.Width := 389;
  fsb.Height := 12;
  fsb.TabOrder := 2;
  fsb.ShowSizeGrip := false;
  fListMsg.InsertComponent(fsb);

  fGrid := TGrid.Create(fListMsg);
  fGrid.Parent := fListMsg;
  fGrid.Align := TAlignLayout.alClient;
  fGrid.Position.Point := TPointF.Create(2, 13);
  fGrid.Width := 389;
  fGrid.Height := 110;
  fGrid.TabOrder := 1;
  fGrid.UseSmallScrollBars := true;
  fGrid.RowHeight := 21;
  fGrid.ShowSelectedCell := false;
  fGrid.ShowVertLines := false;
  fGrid.ShowHorzLines := false;
  fGrid.ShowHeader := false;
  fGrid.OnGetValue := Grid1GetValue;
  fGrid.OnClick := doClick;

  fColumn := TImageColumn.Create(fGrid);
  fColumn.Parent := fGrid;
  fColumn.ReadOnly := true;
  fColumn.Width := 30;
  fColumn.Height := 108;
  fColumn.TabOrder := 0;
  fGrid.InsertComponent(fColumn);
  fColumn := TStringColumn.Create(fGrid);
  fColumn.Parent := fGrid;
  fColumn.ReadOnly := true;
  fColumn.Position.Point := TPointF.Create(30, 0);
  fColumn.Width := 295;
  fColumn.Height := 108;
  fColumn.TabOrder := 1;
  fGrid.InsertComponent(fColumn);
  fColumn := TImageColumn.Create(fGrid);
  fColumn.Parent := fGrid;
  fColumn.ReadOnly := true;
  fColumn.Position.Point := TPointF.Create(325, 0);
  fColumn.Width := 50;
  fColumn.Height := 108;
  fColumn.TabOrder := 2;
  fColumn.OnClick := doClick;
  fGrid.InsertComponent(fColumn);
  fGrid.RowCount := fMessages.Count;
  fListMsg.InsertComponent(fGrid);

  fListMsg.Visible := false;
end;

destructor THlaskaComponent.Destroy;
begin
  fMessages.Free;
  fImage.Free;

  fImgWarnig.Free;
  fImgError.Free;
  fImgInfo.Free;
  fImgDismis.Free;
  fImgDo.Free;

  fDismisList.Free;

  inherited;
end;

procedure THlaskaComponent.doClick(Sender: TObject);
var
  Msg: TMessage;
  s: String;
begin
  // akce po kliku na zpravu
  if Sender is TImage then
  begin
    if not Assigned(fListMsg) then
      CreateListMsg;
    fListMsg.Visible := not fListMsg.Visible;
    if fListMsg.Visible then
      fListMsg.BringToFront;
    fAutoRemove := false;
  end
  else if (Sender is TGrid) and (fGrid.Selected > -1) and (fGrid.Selected < fMessages.Count) then
  begin
    Msg := TMessage(fMessages.Objects[fGrid.Selected]);
    if Msg.MsgType <> mtError then
    begin
      s := IntToStr(Ord(Msg.MsgDruh)) + '-' + IntToStr(Msg.MsgId);
      fDismisList.Add(s);
      fMessages.Delete(fGrid.Selected);
      doMessage;
    end
    else
    begin
      if (Msg.DoAction <> '') and Assigned(fEdit) then
      begin
        fEdit.Hlaska.AddHlaska(mtValidace, fEdit.Helios.OpenSQL(fEdit.doPrepareSql(Msg.DoAction)));
        fEdit.reloadFromTmp;
        fEdit.EditExit(nil);
      end;
    end;
  end;
end;

procedure THlaskaComponent.doMessage;
var
  mt: TMsgType;
  i: Integer;
  Msg: TMessage;
begin
  // zobrazeni a skryti ikony dle poctu zprav
  if fMessages.Count > 0 then
  begin
    mt := high(TMsgType);
    for i := 0 to fMessages.Count - 1 do
    begin
      Msg := TMessage(fMessages.Objects[i]);
      if mt > Msg.MsgType then
        mt := Msg.MsgType;
    end;

    case mt of
      mtError:
        fImage.Bitmap.Assign(fImgError.Bitmap);

      mtWarnign:
        fImage.Bitmap.Assign(fImgWarnig.Bitmap);

      mtInfo:
        fImage.Bitmap.Assign(fImgInfo.Bitmap);

    end;

    if fAutoRemove and Assigned(fListMsg) then
    begin
      fListMsg.Visible := true;
      fAutoRemove := false;
    end;
  end
  else if Assigned(fImage.Bitmap) then
    fImage.Bitmap.Clear(0);

  if Assigned(fGrid) then
  begin
    fGrid.RowCount := fMessages.Count;
    fGrid.Repaint;
    if fGrid.RowCount = 0 then
    begin
      fAutoRemove := true;
      fListMsg.Visible := false;
    end;
  end;
end;

procedure THlaskaComponent.Grid1GetValue(Sender: TObject; const Col, Row: Integer; var Value: Variant);
var
  Msg: TMessage;
begin
  Msg := TMessage(fMessages.Objects[Row]);

  case Col of
    0:
      begin
        case Msg.MsgType of
          mtError:
            Value := ObjectToVariant(fImgError.Bitmap);

          mtWarnign:
            Value := ObjectToVariant(fImgWarnig.Bitmap);

          mtInfo:
            Value := ObjectToVariant(fImgInfo.Bitmap);

        end;

      end;
    1:
      Value := Msg.Msg;
    2:
      begin
        if Msg.DoAction <> '' then
          Value := ObjectToVariant(fImgDo.Bitmap)
        else if (Msg.MsgType <> mtError) then
          Value := ObjectToVariant(fImgDismis.Bitmap);

      end;
  end;
end;

function THlaskaComponent.Hlasky: Boolean;
begin
  Result := fMessages.Count > 0;
end;

procedure THlaskaComponent.removeMessages(aType: TMsgInfoType);
var
  i: Integer;
begin
  for i := fMessages.Count - 1 downto 0 do
    if (TMessage(fMessages.Objects[i]).MsgDruh = aType) then
    begin
      TMessage(fMessages.Objects[i]).Free;
      fMessages.Delete(i);
    end;

  doMessage;
end;

{ TMessage }

procedure TMessage.SetDoAction(const Value: String);
begin
  FDoAction := Value;
end;

procedure TMessage.SetMsg(const Value: String);
begin
  FMsg := Value;
end;

procedure TMessage.SetMsgDruh(const Value: TMsgInfoType);
begin
  FMsgDruh := Value;
end;

procedure TMessage.SetMsgId(const Value: Integer);
begin
  FMsgId := Value;
end;

procedure TMessage.SetMsgType(const Value: TMsgType);
begin
  FMsgType := Value;
end;

end.
