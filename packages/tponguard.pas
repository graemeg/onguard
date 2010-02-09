{ This file was automatically created by Lazarus. do not edit ! 
  This source is only used to compile and install the package.
 }

unit tponguard; 

interface

uses
    ogfile, ognetwrk, ogproexe, ogreg, ogutil, qogabout0, qonguard1, qonguard2, 
  qonguard3, qonguard4, qonguard5, qonguard6, qonguard7, ogconst, onguard, 
  LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('ogreg', @ogreg.Register); 
end; 

initialization
  RegisterPackage('tponguard', @Register); 
end.
