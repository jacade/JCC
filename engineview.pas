{ JCC (Jan's Chess Componenents) - This file contains a visual listview to display output by chess engines
  Copyright (C) 2016-2017  Jan Dette

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
}

unit EngineView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics,
  Dialogs, ComCtrls, uci;

type

  { TEngineView }

  TEngineView = class(TListView)
  private
    { Private declarations }
    FUCIEngine: TUCIEngine;
    procedure FUCIEngineInfo(Sender: TObject; Info: TInfo);
    procedure SetUCIEngine(AValue: TUCIEngine);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  published
    property UCIEngine: TUCIEngine read FUCIEngine write SetUCIEngine;
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I engineview_icon.lrs}
  RegisterComponents('Chess', [TEngineView]);
end;

{ TEngineView }

procedure TEngineView.SetUCIEngine(AValue: TUCIEngine);
begin
  if FUCIEngine = AValue then
    Exit;
  if Assigned(AValue) then
  begin
    FUCIEngine := AValue;
    FUCIEngine.OnInfo := @FUCIEngineInfo;
  end;
end;

constructor TEngineView.Create(AOwner: TComponent);
var
  i: integer;
  Col: TListColumn;
begin
  inherited Create(AOwner);
  for i := 1 to 5 do
  begin
    Col := Columns.Add;
    Col.MinWidth := 50;
    Col.AutoSize := True;
  end;
  Columns[0].Caption := 'Depth';
  Columns[1].Caption := 'Score';
  Columns[2].Caption := 'Time';
  Columns[3].Caption := 'Nodes';
  Columns[4].Caption := 'Variation';
  Self.ViewStyle := vsReport;
end;

procedure TEngineView.FUCIEngineInfo(Sender: TObject; Info: TInfo);
var
  ListItem: TListItem;
begin
  if Info.Depth > 0 then
  begin
    ListItem := Self.Items.Add;
    ListItem.Caption := IntToStr(Info.Depth);
    ListItem.SubItems.Add(FloatToStr(Info.Score.CP / 100));
    ListItem.SubItems.Add(FloatToStr(Info.Time / 1000));
    ListItem.SubItems.Add(IntToStr(Info.Nodes));
    ListItem.SubItems.Add('');
  end;
  // Zur Zeit keine weitere Verwendung
  FreeAndNil(Info.CurrLine);
  FreeAndNil(Info.PV);
  FreeAndNil(Info.Refutation);
end;

end.
