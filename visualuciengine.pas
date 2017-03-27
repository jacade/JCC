unit VisualUCIEngine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, UCI;

type
  TVisualUCIEngine = class(TUCIEngine)
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I uci_icon.lrs}
  RegisterComponents('Chess', [TVisualUCIEngine]);
end;

end.

