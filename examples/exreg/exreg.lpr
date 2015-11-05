program ExReg;

{$mode objfpc}{$H+}

uses
  Interfaces,
  Forms,
  exreg1;


begin
  Application.Initialize;
  Application.CreateForm(TfrmExRegCode, frmExRegCode);
  Application.Run;
end.
