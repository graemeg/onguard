program Exdmod;
{$mode objfpc}{$H+}

uses
  Interfaces,
  Forms,
  Exdmodu1 in 'EXDMODU1.PAS' {Form1},
  Exdmodu2 in 'EXDMODU2.PAS' {SNEntryDlg};

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
