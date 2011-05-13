unit frm_keymaint;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fpg_base, fpg_main, fpg_form, fpg_label, fpg_button,
  fpg_listbox, fpg_editbtn;

type

  TKeyMaintForm = class(TfpgForm)
  private
    {@VFD_HEAD_BEGIN: KeyMaintForm}
    Label1: TfpgLabel;
    edtFilename: TfpgFileNameEdit;
    Label2: TfpgLabel;
    btnAddApp: TfpgButton;
    btnEditApp: TfpgButton;
    btnDelApp: TfpgButton;
    lbApplications: TfpgListBox;
    Label3: TfpgLabel;
    edtBlockKey: TfpgEdit;
    edtBytesBlockKey: TfpgEdit;
    btnOK: TfpgButton;
    btnCancel: TfpgButton;
    btnLoadFile: TfpgButton;
    {@VFD_HEAD_END: KeyMaintForm}
    FKey: TKey;
    FKeyType: TKeyType;
    function    GetKeyFileName: string;
    procedure   SetKeyFileName(const AValue: string);
    procedure   FormCreate(Sender: TObject);
    procedure   InfoChanged(Sender: TObject);
    procedure   btnLoadClicked(Sender: TObject);
    procedure   edtFilenameHasBeenSet(Sender: TObject; const AOldValue, ANewValue: TfpgString);
    procedure   btnAddAppClicked(Sender: TObject);
    procedure   btnEditAppClicked(Sender: TObject);
    procedure   btnDelAppClicked(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure   AfterCreate; override;
    procedure   SetKey(Value: TKey);
    procedure   GetKey(var AValue: TKey);
    property    KeyFileName: string read GetKeyFileName write SetKeyFileName;
    property    KeyType: TKeyType read FKeyType write FKeyType;
  end;

{@VFD_NEWFORM_DECL}

implementation

uses
  ogconst,
  IniFiles,
  fpg_dialogs,
  frm_productmaint;

{@VFD_NEWFORM_IMPL}

function TKeyMaintForm.GetKeyFileName: string;
begin
  Result := edtFileName.FileName;
end;

procedure TKeyMaintForm.SetKeyFileName(const AValue: string);
begin
  edtFileNameEd.Filename := AValue;
  InfoChanged(Self);
end;

procedure TKeyMaintForm.FormCreate(Sender: TObject);
begin
  KeyFileName := '';
end;

procedure TKeyMaintForm.InfoChanged(Sender: TObject);
var
  I: Integer;
  IniFile: TIniFile;
begin
  try
    FillChar(FKey, SizeOf(FKey), 0);
    if Length(KeyFileName) > 0 then begin
      IniFile := TIniFile.Create(KeyFileName);
      try
        I := lbApplications.FocusItem;
        lbApplications.BeginUpdate;
        try
          lbApplications.Items.Clear;
          IniFile.ReadSection(OgKeySection, lbApplications.Items);
          if I < lbApplications.Items.Count then
            lbApplications.FocusIndex := I
          else
          begin
            lbApplications.FocusIndex := lbApplications.Items.Count-1;
            I := lbApplications.FocusIndex;
          end;

          if (I > -1) then
          begin
            //btnEditApp.Enabled := True;
            //btnDelApp.Enabled := True;
            edtBlockKey.Text := IniFile.ReadString(OgKeySection, lbApplications.Items[I], '');
            HexToBuffer(edtBlockKey.Text, FKey, SizeOf(FKey));
            edtBlockKey.Text := BufferToHex(FKey, SizeOf(FKey));
            edtBytesKey.Text := BufferToHexBytes(FKey, SizeOf(FKey));
            if HexStringIsZero(edtBlockKey.Text)then
              edtBlockKey.Text := '';
            if HexStringIsZero(edtBytesKey.Text)then
              edtBytesKey.Text := '';
          end
          else
          begin
            //btnEditApp.Enabled := False;
            //btnDelApp.Enabled := False;
          end;
        finally
          lbApplications.EndUpdate;
        end;
      finally
        IniFile.Free;
      end;
    end
    else
      lbApplications.Items.Clear;

    //btnOK.Enabled := HexToBuffer(edtBlockKey.Text, FKey, SizeOf(FKey));
  except
    on E:Exception do
      ShowMessage(E.Message);
  end;
end;

procedure TKeyMaintForm.btnLoadClicked(Sender: TObject);
begin
  InfoChanged(self);
end;

procedure TKeyMaintForm.edtFilenameHasBeenSet(Sender: TObject; const AOldValue, ANewValue: TfpgString);
begin
  KeyFileName := ANewValue;
end;

procedure TKeyMaintForm.btnAddAppClicked(Sender: TObject);
var
  F: TProductMaintForm;
  IniFile: TIniFile;
begin
  F := TProductMaintForm.Create(Self);
  try
    F.SetKey(FKey);
    F.KeyType := FKeyType;
    F.ShowHint := ShowHint;
    if F.ShowModal = mrOK then begin
      IniFile := TIniFile.Create(KeyFileName);
      try
        IniFile.WriteString(OgKeySection, F.ProductEd.Text, F.KeyEd.Text);
      finally
        IniFile.Free;
      end;
      F.GetKey(FKey);
      FKeyType := F.KeyType;
    end;
  finally
    F.Free;
  end;

  InfoChanged(Self);
end;

procedure TKeyMaintForm.btnEditAppClicked(Sender: TObject);
begin

end;

procedure TKeyMaintForm.btnDelAppClicked(Sender: TObject);
begin

end;

constructor TKeyMaintForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnCreate := @FormCreate;
end;

procedure TKeyMaintForm.AfterCreate;
begin
  {%region 'Auto-generated GUI code' -fold}
  {@VFD_BODY_BEGIN: KeyMaintForm}
  Name := 'KeyMaintForm';
  SetPosition(673, 170, 422, 402);
  WindowTitle := 'Key Maintenance';
  Hint := '';
  ShowHint := True;
  MinWidth := 380;
  MinHeight := 400;

  Label1 := TfpgLabel.Create(self);
  with Label1 do
  begin
    Name := 'Label1';
    SetPosition(8, 8, 256, 16);
    FontDesc := '#Label2';
    Hint := '';
    Text := 'Key file';
  end;

  edtFilename := TfpgFileNameEdit.Create(self);
  with edtFilename do
  begin
    Name := 'edtFilename';
    SetPosition(24, 28, 304, 24);
    Anchors := [anLeft,anRight,anTop];
    ExtraHint := '';
    FileName := 'onguard.ini';
    Filter := 'Key File (*.ini)|*.ini';
    InitialDir := '';
    TabOrder := 2;
    OnFilenameSet  := @edtFilenameHasBeenSet;
  end;

  Label2 := TfpgLabel.Create(self);
  with Label2 do
  begin
    Name := 'Label2';
    SetPosition(8, 60, 272, 16);
    FontDesc := '#Label2';
    Hint := '';
    Text := 'Applications';
  end;

  btnAddApp := TfpgButton.Create(self);
  with btnAddApp do
  begin
    Name := 'btnAddApp';
    SetPosition(24, 80, 24, 24);
    Text := '';
    Flat := True;
    FontDesc := '#Label1';
    Hint := 'Add';
    ImageMargin := 0;
    ImageName := 'stdimg.add';
    TabOrder := 4;
  end;

  btnEditApp := TfpgButton.Create(self);
  with btnEditApp do
  begin
    Name := 'btnEditApp';
    SetPosition(49, 80, 24, 24);
    Text := '';
    Flat := True;
    FontDesc := '#Label1';
    Hint := 'Edit';
    ImageMargin := 0;
    ImageName := 'stdimg.edit';
    TabOrder := 5;
  end;

  btnDelApp := TfpgButton.Create(self);
  with btnDelApp do
  begin
    Name := 'btnDelApp';
    SetPosition(74, 80, 24, 24);
    Text := '';
    Flat := True;
    FontDesc := '#Label1';
    Hint := 'Delete';
    ImageMargin := 0;
    ImageName := 'stdimg.delete';
    TabOrder := 6;
  end;

  lbApplications := TfpgListBox.Create(self);
  with lbApplications do
  begin
    Name := 'lbApplications';
    SetPosition(24, 104, 388, 156);
    Anchors := [anLeft,anRight,anTop];
    FontDesc := '#List';
    Hint := '';
    TabOrder := 7;
  end;

  Label3 := TfpgLabel.Create(self);
  with Label3 do
  begin
    Name := 'Label3';
    SetPosition(8, 272, 220, 16);
    FontDesc := '#Label2';
    Hint := '';
    Text := 'Key';
  end;

  edtBlockKey := TfpgEdit.Create(self);
  with edtBlockKey do
  begin
    Name := 'edtBlockKey';
    SetPosition(24, 292, 388, 24);
    Anchors := [anLeft,anRight,anTop];
    ExtraHint := '';
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 9;
    Text := '';
  end;

  edtBytesBlockKey := TfpgEdit.Create(self);
  with edtBytesBlockKey do
  begin
    Name := 'edtBytesBlockKey';
    SetPosition(24, 320, 388, 24);
    Anchors := [anLeft,anRight,anTop];
    ExtraHint := '';
    FontDesc := '#Edit1';
    Hint := '';
    TabOrder := 10;
    Text := '';
  end;

  btnOK := TfpgButton.Create(self);
  with btnOK do
  begin
    Name := 'btnOK';
    SetPosition(248, 368, 80, 24);
    Anchors := [anRight,anBottom];
    Text := 'OK';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    ModalResult := mrOK;
    TabOrder := 11;
  end;

  btnCancel := TfpgButton.Create(self);
  with btnCancel do
  begin
    Name := 'btnCancel';
    SetPosition(332, 368, 80, 24);
    Anchors := [anRight,anBottom];
    Text := 'Cancel';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    ModalResult := mrCancel;
    TabOrder := 12;
  end;

  btnLoadFile := TfpgButton.Create(self);
  with btnLoadFile do
  begin
    Name := 'btnLoadFile';
    SetPosition(332, 28, 80, 24);
    Text := 'Load';
    FontDesc := '#Label1';
    Hint := '';
    ImageName := '';
    TabOrder := 13;
    OnClick := @btnLoadClicked;
  end;

  {@VFD_BODY_END: KeyMaintForm}
  {%endregion}
end;

procedure TKeyMaintForm.SetKey(Value: TKey);
begin

end;

procedure TKeyMaintForm.GetKey(var AValue: TKey);
begin

end;


end.
