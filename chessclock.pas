{ JCC (Jan's Chess Componenents) - This file contains a visual digital chess clock
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

unit ChessClock;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs;

type

  EInvalidTimeLimit = class(Exception);

  { TChessClock }

  TChessClock = class(TCustomControl)
  private
    FShowLeadingZero: boolean;
    FShowSecondsLimit: word;
    FTimeLeft: longword;
    procedure SetShowSecondsLimit(AValue: word);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetTime(Hours, Minutes, Seconds: word);
  published
    // Determines wether the first digit should be displayed if it's zero.
    property ShowLeadingZero: boolean read FShowLeadingZero
      write FShowLeadingZero default True;
    // This limit determines, when the clock should switch from HH:MM to MM:SS display.
    // It is given in minutes.
    // Note that it must be under 60, otherwise there will be an exception raised.
    property ShowSecondsLimit: word read FShowSecondsLimit
      write SetShowSecondsLimit default 20;
    // Time in deciseconds, which is left on the clock
    property TimeLeft: longword read FTimeLeft write FTimeLeft default 0;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I chessclock_icon.lrs}
  RegisterComponents('Chess', [TChessClock]);
end;

{ TChessClock }

procedure TChessClock.SetShowSecondsLimit(AValue: word);
begin
  if FShowSecondsLimit = AValue then
    Exit;
  if AValue >= 60 then
    raise EInvalidTimeLimit.Create('The ShowSecondsLimit must be under 60. Instead + ' +
      IntToStr(AValue) + ' was given.');
  FShowSecondsLimit := AValue;
end;

procedure TChessClock.Paint;
var
  Digits: array[1..4] of integer;
  Hours, Minutes, Seconds: integer;
  X, Y, i, t: integer;
begin
  inherited Paint;
  // Just for now
  Canvas.Pen.Color := clBlack;
  Canvas.Brush.Color := clBlack;
  Canvas.Rectangle(0, 0, Width, Height);
  Hours := FTimeLeft div 36000;
  Minutes := (FTimeLeft - (36000 * Hours)) div 600;
  Seconds := (FTimeLeft div 10) mod 60;
  if Minutes < FShowSecondsLimit then
  begin
    Digits[1] := Minutes div 10;
    Digits[2] := Minutes mod 10;
    Digits[3] := Seconds div 10;
    Digits[4] := Seconds mod 10;
  end
  else
  begin
    Digits[1] := Hours div 10;
    Digits[2] := Hours mod 10;
    Digits[3] := Minutes div 10;
    Digits[4] := Minutes mod 10;
  end;
  Canvas.Brush.Color := $3bfbfd;
  Canvas.Pen.Color := $35c7f5;
  Y := 5;
  X := 21;
  Canvas.Ellipse(50, Y + 4, 53, Y + 8);
  Canvas.Ellipse(50, Y + 13, 53, Y + 17);
  if ShowLeadingZero then
    t := 1
  else
    t := 2;
  for i := t to 4 do
  begin
    if Digits[i] in [4, 5, 6, 8, 9, 0] then
      Canvas.Ellipse(X, Y + 3, X + 3, Y + 9);
    if Digits[i] in [2, 3, 5, 6, 7, 8, 9, 0] then
      Canvas.Ellipse(X + 3, Y, X + 9, Y + 3);
    if Digits[i] in [1, 2, 3, 4, 7, 8, 9, 0] then
      Canvas.Ellipse(X + 9, Y + 3, X + 12, Y + 9);
    if Digits[i] in [2, 3, 4, 5, 6, 8, 9] then
      Canvas.Ellipse(X + 3, Y + 9, X + 9, Y + 12);
    if Digits[i] in [1, 3, 4, 5, 6, 7, 8, 9, 0] then
      Canvas.Ellipse(X + 9, Y + 12, X + 12, Y + 18);
    if Digits[i] in [2, 3, 5, 6, 8, 9, 0] then
      Canvas.Ellipse(X + 3, Y + 18, X + 9, Y + 21);
    if Digits[i] in [2, 6, 8, 0] then
      Canvas.Ellipse(X, Y + 12, X + 3, Y + 18);
    if i = 2 then
      Inc(X, 21)
    else
      Inc(X, 14);
  end;
end;

constructor TChessClock.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FShowLeadingZero := True;
  FShowSecondsLimit := 20;
  FTimeLeft := 0;
end;

procedure TChessClock.SetTime(Hours, Minutes, Seconds: word);
begin
  FTimeLeft := (Hours * 3600 + Minutes * 60 + Seconds) * 10;
end;

end.
