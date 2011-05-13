unit frm_codegen;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_form, fpg_button, fpg_label,
  fpg_tab, fpg_popupcalendar, fpg_checkbox, fpg_panel, fpg_edit, onguard;

type

  TCodeGenerationForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: CodeGenerationForm}
    btnOK: TfpgButton;
    btnCancel: TfpgButton;
    pgCodes: TfpgPageControl;
    tsDate: TfpgTabSheet;
    tsDays: TfpgTabSheet;
    tsReg: TfpgTabSheet;
    tsSerialNo: TfpgTabSheet;
    tsUsage: TfpgTabSheet;
    tsNetwork: TfpgTabSheet;
    tsSpecial: TfpgTabSheet;
    lblDateStart: TfpgLabel;
    calDateStart: TfpgCalendarCombo;
    lblDateEnd: TfpgLabel;
    calDateEnd: TfpgCalendarCombo;
    lblDaysCount: TfpgLabel;
    EditInteger1: TfpgEditInteger;
    lblDaysExpire: TfpgLabel;
    EditInteger2: TfpgEditInteger;
    GroupBox1: TfpgGroupBox;
    cbxNoModifier: TfpgCheckBox;
    cbxMachineModifier: TfpgCheckBox;
    cbxUniqueModifier: TfpgCheckBox;
    cbxDateModifier: TfpgCheckBox;
    calModifier: TfpgCalendarCombo;
    cbxStringModifier: TfpgCheckBox;
    Edit1: TfpgEdit;
    Label1: TfpgLabel;
    edtModifier: TfpgEdit;
    Label2: TfpgLabel;
    edtBlockKey: TfpgEdit;
    btnGenKey: TfpgButton;
    GroupBox2: TfpgGroupBox;
    btnGenerate: TfpgButton;
    Edit4: TfpgEdit;
    Button2: TfpgButton;
    {@VFD_HEAD_END: CodeGenerationForm}
    FCode: TCode;
    FCodeType: TCodeType;
    FKey: TKey;
    FKeyType: TKeyType;
    FKeyFileName: string;
    procedure SetCodeType(const AValue: TCodeType);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure InfoChanged(Sender: TObject);
    procedure btnGenerateKeyClicked(Sender: TObject);
    procedure OGMCheck;
    procedure OGMQuit;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AfterCreate; override;
    procedure SetKey(Value: TKey);
    procedure GetKey(var Value: TKey);
    property  Code: TCode read FCode;
    property  CodeType: TCodeType read FCodeType write SetCodeType;
    property  KeyFileName: string read FKeyFileName write FKeyFileName;
    property  KeyType: TKeyType read FKeyType write FKeyType;
  end;

{@VFD_NEWFORM_DECL}

implementation

uses
  ogutil,
  frm_keymaint;

{@VFD_NEWFORM_IMPL}

procedure TCodeGenerationForm.SetCodeType(const AValue: TCodeType);
begin
  if Value <> TCodeType(pgCodes.ActivePageIndex) then
  begin
    FCodeType := Value;
    pgCodes.ActivePageIndex := Ord(FCodeType);
  end;
end;

procedure TCodeGenerationForm.FormCreate(Sender: TObject);
var
  D: TDateTime;
begin
  cbxNoModifier.Checked := True;
  pgCodes.ActivePageIndex := Ord(FCodeType);
  edtBlockKey.Text := BufferToHex(FKey, SizeOf(FKey));
  if HexStringIsZero(edtBlockKey.Text)then
    edtBlockKey.Text := '';

  {initialize date edits}
  calDateStart.DateValue := Date;
  calDateEnd.DateValue := Date;
  calModifier.DateValue := Date;

  D := EncodeDate(9999,12,31);
  //UsageExpiresEd.Text := OgFormatDate(D);
  //SpecialExpiresEd.Text := OgFormatDate(D);
  //SerialExpiresEd.Text := OgFormatDate(D);
  //RegExpiresEd.Text := OgFormatDate(D);
  //DaysExpiresEd.Text := OgFormatDate(D);

  InfoChanged(self);
end;

procedure TCodeGenerationForm.FormShow(Sender: TObject);
begin
  OGMCheck;
end;

procedure TCodeGenerationForm.InfoChanged(Sender: TObject);
begin
 // GenerateBtn.Enabled := HexToBuffer(BlockKeyEd.Text, FKey, SizeOf(FKey));
 // OKBtn.Enabled := Length(RegCodeEd.Text) > 0;
end;

procedure TCodeGenerationForm.btnGenerateKeyClicked(Sender: TObject);
var
  F: TKeyMaintForm;
begin
  F := TKeyMaintForm.Create(Self);
  try
    F.SetKey(FKey);
    F.KeyType := FKeyType;
    F.KeyFileName := FKeyFileName;
    F.ShowHint := ShowHint;
    if F.ShowModal = mrOK then
    begin
      F.GetKey(FKey);
      BlockKeyEd.Text := BufferToHex(FKey, SizeOf(FKey));
      if HexStringIsZero(BlockKeyEd.Text)then
        BlockKeyEd.Text := '';
      FKeyType := F.KeyType;
      FKeyFileName := F.KeyFileName;
      InfoChanged(Self);
    end;
  finally
    F.Free;
  end;
end;

procedure TCodeGenerationForm.OGMCheck;
var
  F: TKeyMaintForm;
begin
  if not HexToBuffer(edtBlockKey.Text, FKey, SizeOf(FKey)) then
  begin
    {get a key}
    F := TKeyMaintForm.Create(Self);
    try
      F.SetKey(FKey);
      F.KeyType := ktRandom;
      F.KeyFileName := FKeyFileName;
      F.ShowHint := ShowHint;
      if F.ShowModal = mrOK then
      begin
        F.GetKey(FKey);
        BlockKeyEd.Text := BufferToHex(FKey, SizeOf(FKey));
        if HexStringIsZero(BlockKeyEd.Text) then
          BlockKeyEd.Text := '';
        FKeyFileName := F.KeyFileName;
        InfoChanged(Self);
      end
      else
        OGMQuit;
    finally
      F.Free;
    end;
  end;
end;

procedure TCodeGenerationForm.OGMQuit;
begin
  ModalResult := mrCancel;
end;

constructor TCodeGenerationForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnCreate := @FormCreate;
  OnShow := @FormShow;
end;

procedure TCodeGenerationForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: CodeGenerationForm}
  Name := 'CodeGenerationForm';
  SetPosition(714, 208, 490, 478);
  WindowTitle := 'Code Generation';
  Hint := '';

  btnOK := TfpgButton.Create(self);
  with btnOK do
  begin
    Name := 'btnOK';
    SetPosition(316, 444, 80, 24);
    Anchors := [anRight,anBottom];
    Text := 'OK';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 1;
  end;

  btnCancel := TfpgButton.Create(self);
  with btnCancel do
  begin
    Name := 'btnCancel';
    SetPosition(400, 444, 80, 24);
    Anchors := [anRight,anBottom];
    Text := 'Cancel';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 2;
  end;

  pgCodes := TfpgPageControl.Create(self);
  with pgCodes do
  begin
    Name := 'pgCodes';
    SetPosition(4, 4, 482, 108);
    Anchors := [anLeft,anRight,anTop];
    Hint := '';
    TabOrder := 3;
  end;

  tsDate := TfpgTabSheet.Create(pgCodes);
  with tsDate do
  begin
    Name := 'tsDate';
    SetPosition(3, 24, 476, 81);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Text := 'Date';
  end;

  tsDays := TfpgTabSheet.Create(pgCodes);
  with tsDays do
  begin
    Name := 'tsDays';
    SetPosition(3, 24, 476, 81);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Text := 'Days';
  end;

  tsReg := TfpgTabSheet.Create(pgCodes);
  with tsReg do
  begin
    Name := 'tsReg';
    SetPosition(3, 24, 476, 81);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Text := 'Reg';
  end;

  tsSerialNo := TfpgTabSheet.Create(pgCodes);
  with tsSerialNo do
  begin
    Name := 'tsSerialNo';
    SetPosition(3, 24, 476, 81);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Text := 'S/N';
  end;

  tsUsage := TfpgTabSheet.Create(pgCodes);
  with tsUsage do
  begin
    Name := 'tsUsage';
    SetPosition(3, 24, 476, 81);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Text := 'Usage';
  end;

  tsNetwork := TfpgTabSheet.Create(pgCodes);
  with tsNetwork do
  begin
    Name := 'tsNetwork';
    SetPosition(3, 24, 476, 81);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Text := 'Network';
  end;

  tsSpecial := TfpgTabSheet.Create(pgCodes);
  with tsSpecial do
  begin
    Name := 'tsSpecial';
    SetPosition(3, 24, 476, 81);
    Anchors := [anLeft,anRight,anTop,anBottom];
    Text := 'tsSpecial';
  end;

  lblDateStart := TfpgLabel.Create(tsDate);
  with lblDateStart do
  begin
    Name := 'lblDateStart';
    SetPosition(8, 8, 104, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Start date:';
  end;

  calDateStart := TfpgCalendarCombo.Create(tsDate);
  with calDateStart do
  begin
    Name := 'calDateStart';
    SetPosition(120, 4, 120, 24);
    BackgroundColor := TfpgColor($80000002);
    DateFormat := 'yyyy-mm-dd';
    DayColor := TfpgColor($000000);
    FontDesc := '#List';
    Hint := '';
    HolidayColor := TfpgColor($000000);
    SelectedColor := TfpgColor($000000);
    TabOrder := 2;
  end;

  lblDateEnd := TfpgLabel.Create(tsDate);
  with lblDateEnd do
  begin
    Name := 'lblDateEnd';
    SetPosition(8, 36, 104, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'End date:';
  end;

  calDateEnd := TfpgCalendarCombo.Create(tsDate);
  with calDateEnd do
  begin
    Name := 'calDateEnd';
    SetPosition(120, 32, 120, 24);
    BackgroundColor := TfpgColor($80000002);
    DateFormat := 'yyyy-mm-dd';
    DayColor := TfpgColor($000000);
    FontDesc := '#List';
    Hint := '';
    HolidayColor := TfpgColor($000000);
    SelectedColor := TfpgColor($000000);
    TabOrder := 4;
  end;

  lblDaysCount := TfpgLabel.Create(tsDays);
  with lblDaysCount do
  begin
    Name := 'lblDaysCount';
    SetPosition(8, 8, 104, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Day count:';
  end;

  EditInteger1 := TfpgEditInteger.Create(tsDays);
  with EditInteger1 do
  begin
    Name := 'EditInteger1';
    SetPosition(120, 4, 120, 24);
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 2;
    Value := 0;
  end;

  lblDaysExpire := TfpgLabel.Create(tsDays);
  with lblDaysExpire do
  begin
    Name := 'lblDaysExpire';
    SetPosition(8, 36, 104, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Expires:';
  end;

  EditInteger2 := TfpgEditInteger.Create(tsDays);
  with EditInteger2 do
  begin
    Name := 'EditInteger2';
    SetPosition(120, 32, 120, 24);
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 4;
    Value := 0;
  end;

  GroupBox1 := TfpgGroupBox.Create(self);
  with GroupBox1 do
  begin
    Name := 'GroupBox1';
    SetPosition(4, 120, 482, 216);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Key used to encode';
  end;

  cbxNoModifier := TfpgCheckBox.Create(GroupBox1);
  with cbxNoModifier do
  begin
    Name := 'cbxNoModifier';
    SetPosition(8, 20, 120, 20);
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 1;
    Text := 'No modifier';
  end;

  cbxMachineModifier := TfpgCheckBox.Create(GroupBox1);
  with cbxMachineModifier do
  begin
    Name := 'cbxMachineModifier';
    SetPosition(184, 20, 120, 20);
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 2;
    Text := 'Machine modifier';
  end;

  cbxUniqueModifier := TfpgCheckBox.Create(GroupBox1);
  with cbxUniqueModifier do
  begin
    Name := 'cbxUniqueModifier';
    SetPosition(344, 20, 120, 20);
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 3;
    Text := 'Unique modifier';
  end;

  cbxDateModifier := TfpgCheckBox.Create(GroupBox1);
  with cbxDateModifier do
  begin
    Name := 'cbxDateModifier';
    SetPosition(8, 48, 120, 20);
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 4;
    Text := 'Date modifier';
  end;

  calModifier := TfpgCalendarCombo.Create(GroupBox1);
  with calModifier do
  begin
    Name := 'calModifier';
    SetPosition(140, 44, 120, 24);
    BackgroundColor := TfpgColor($80000002);
    DateFormat := 'yyyy-mm-dd';
    DayColor := TfpgColor($000000);
    FontDesc := '#List';
    Hint := '';
    HolidayColor := TfpgColor($000000);
    SelectedColor := TfpgColor($000000);
    TabOrder := 5;
  end;

  cbxStringModifier := TfpgCheckBox.Create(GroupBox1);
  with cbxStringModifier do
  begin
    Name := 'cbxStringModifier';
    SetPosition(8, 76, 120, 20);
    FontDesc := '#Label1';
    Hint := '';
    TabOrder := 6;
    Text := 'String Modifier';
  end;

  Edit1 := TfpgEdit.Create(GroupBox1);
  with Edit1 do
  begin
    Name := 'Edit1';
    SetPosition(140, 72, 332, 24);
    ExtraHint := '';
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 7;
    Text := '';
  end;

  Label1 := TfpgLabel.Create(GroupBox1);
  with Label1 do
  begin
    Name := 'Label1';
    SetPosition(8, 112, 80, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Modifier:';
  end;

  edtModifier := TfpgEdit.Create(GroupBox1);
  with edtModifier do
  begin
    Name := 'edtModifier';
    SetPosition(8, 128, 252, 24);
    ExtraHint := '';
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 9;
    Text := '';
  end;

  Label2 := TfpgLabel.Create(GroupBox1);
  with Label2 do
  begin
    Name := 'Label2';
    SetPosition(8, 160, 80, 16);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Key';
  end;

  edtBlockKey := TfpgEdit.Create(GroupBox1);
  with edtBlockKey do
  begin
    Name := 'edtBlockKey';
    SetPosition(8, 176, 428, 24);
    ExtraHint := '';
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 11;
    Text := '';
  end;

  btnGenKey := TfpgButton.Create(GroupBox1);
  with btnGenKey do
  begin
    Name := 'btnGenKey';
    SetPosition(444, 176, 24, 24);
    Text := 'K';
    Flat := True;
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 12;
  end;

  GroupBox2 := TfpgGroupBox.Create(self);
  with GroupBox2 do
  begin
    Name := 'GroupBox2';
    SetPosition(4, 344, 482, 72);
    FontDesc := '#Label1';
    Hint := '';
    Text := 'Generate Code';
  end;

  btnGenerate := TfpgButton.Create(GroupBox2);
  with btnGenerate do
  begin
    Name := 'btnGenerate';
    SetPosition(8, 28, 80, 24);
    Text := 'Generate';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 1;
  end;

  Edit4 := TfpgEdit.Create(GroupBox2);
  with Edit4 do
  begin
    Name := 'Edit4';
    SetPosition(96, 28, 340, 24);
    ExtraHint := '';
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 2;
    Text := '';
  end;

  Button2 := TfpgButton.Create(GroupBox2);
  with Button2 do
  begin
    Name := 'Button2';
    SetPosition(444, 28, 24, 24);
    Text := 'C';
    Flat := True;
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 3;
  end;

  {@VFD_BODY_END: CodeGenerationForm}
  {%endregion}
end;

procedure TCodeGenerationForm.SetKey(Value: TKey);
begin
  FKey := Value;
  edtBlockKey.Text := BufferToHex(FKey, SizeOf(FKey));
  if HexStringIsZero(edtBlockKey.Text) then
    edtBlockKey.Text := '';
end;

procedure TCodeGenerationForm.GetKey(var Value: TKey);
begin
  Value := FKey;
end;


end.
