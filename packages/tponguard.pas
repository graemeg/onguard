{ This file was automatically created by Lazarus. do not edit ! 
  This source is only used to compile and install the package.
 }

unit tponguard; 

interface

uses
  ogfile, ognetwrk, ogproexe, ogutil, ogconst, onguard, LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('tponguard', @Register); 
end.
