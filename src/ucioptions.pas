{ JCC (Jan's Chess Componenents) - This file contains classes to handle options given by chess engines
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

unit UCIOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrTools, fgl;

type

  TType = (tpCheck, tpSpin, tpCombo, tpButton, tpString);

  { TUCIOption }

  TUCIOption = class
  protected
    FName: string;
    function GetValue: string; virtual; abstract;
    procedure SetValue(const AValue: string); virtual; abstract;
  public
    function Typ: TType; virtual; abstract;
    property Name: string read FName;
    property Value: string read GetValue write SetValue;
  end;

  { TUCIOptionList }

  TUCIOptionList = class(specialize TFPGObjectList<TUCIOption>)
  public
    function GetUCIOptionByName(const UCIOptionName: string): TUCIOption;
  end;

  { TUCIButtonOption }

  TUCIButtonOption = class(TUCIOption)
  protected
    function GetValue: string; override;
    procedure SetValue(const AValue: string); override;
  public
    function Typ: TType; override;
    constructor Create(const AName: string);
  end;

  { TUCICheckOption }

  TUCICheckOption = class(TUCIOption)
  private
    FDefault, FValue: boolean;
  protected
    function GetValue: string; override;
    procedure SetValue(const AValue: string); override;
  public
    constructor Create(const AName, AValue: string);
    function Typ: TType; override;
    property DefaultValue: boolean read FDefault;
    property ValueBool: Boolean read FValue write FValue;
  end;

  { TUCIComboOption }

  TUCIComboOption = class(TUCIOption)
  private
    FDefault, FValue: string;
    FVariables: TStringList;
  protected
    function GetValue: string; override;
    procedure SetValue(const AValue: string); override;
  public
    constructor Create(const AName, AValue, Vars: string);
    destructor Destroy; override;
    function Typ: TType; override;
    property DefaultValue: string read FDefault;
    property Variables: TStringList read FVariables;
  end;

  { TUCISpinOption }

  TUCISpinOption = class(TUCIOption)
  private
    FDefault, FMax, FMin, FValue: integer;
  protected
    function GetValue: string; override;
    procedure SetValue(const AValue: string); override;
  public
    constructor Create(const AName, AValue, AMin, AMax: string);
    function Typ: TType; override;
    property DefaultValue: integer read FDefault;
    property Maximum: integer read FMax;
    property Minimum: integer read FMin;
    property ValueInt: Integer read FValue write FValue;
  end;

  { TUCIStringOption }

  TUCIStringOption = class(TUCIOption)
  private
    FDefault, FValue: string;
  protected
    function GetValue: string; override;
    procedure SetValue(const AValue: string); override;
  public
    constructor Create(const AName, AValue: string);
    function Typ: TType; override;
    property DefaultValue: string read FDefault;
  end;

implementation

{ TUCIOptionList }

function TUCIOptionList.GetUCIOptionByName(const UCIOptionName: string): TUCIOption;
begin
  for Result in Self do
    if Result.Name = UCIOptionName then
      Exit;
  Result := nil;
end;

{ TUCIStringOption }

function TUCIStringOption.GetValue: string;
begin
  Result := FValue;
end;

procedure TUCIStringOption.SetValue(const AValue: string);
begin
  if AValue = '' then
    FValue := '<empty>'
  else
    FValue := AValue;
end;

constructor TUCIStringOption.Create(const AName, AValue: string);
begin
  FName := AName;
  if Length(AValue) = 0 then
    FDefault := '<empty>'
  else
    FDefault := AValue;
  FVAlue := FDefault;
end;

function TUCIStringOption.Typ: TType;
begin
  Result := tpString;
end;

{ TUCISpinOption }

function TUCISpinOption.GetValue: string;
begin
  Result := IntToStr(FValue);
end;

procedure TUCISpinOption.SetValue(const AValue: string);
begin
  FValue := StrToInt(AValue);
end;

constructor TUCISpinOption.Create(const AName, AValue, AMin, AMax: string);
begin
  FName := AName;
  FDefault := StrToInt(AValue);
  FValue := FDefault;
  FMax := StrToInt(AMax);
  FMin := StrToInt(AMin);
end;

function TUCISpinOption.Typ: TType;
begin
  Result := tpSpin;
end;

{ TUCIComboOption }

function TUCIComboOption.GetValue: string;
begin
  Result := FValue;
end;

procedure TUCIComboOption.SetValue(const AValue: string);
begin
  FValue := AValue;
end;

constructor TUCIComboOption.Create(const AName, AValue, Vars: string);
begin
  FName := AName;
  FVariables := Split(Vars, ';');

  FDefault := AValue;
  FValue := FDefault;
end;

destructor TUCIComboOption.Destroy;
begin
  FVariables.Free;
  inherited Destroy;
end;

function TUCIComboOption.Typ: TType;
begin
  Result := tpCombo;
end;

{ TUCICheckOption }

function TUCICheckOption.GetValue: string;
begin
  Result := BoolToStr(FValue, 'true', 'false');
end;

procedure TUCICheckOption.SetValue(const AValue: string);
begin
  FValue := StrToBool(AValue);
end;

constructor TUCICheckOption.Create(const AName, AValue: string);
begin
  FName := AName;
  FDefault := AValue = 'true';
  FValue := FDefault;
end;

function TUCICheckOption.Typ: TType;
begin
  Result := tpCheck;
end;

{ TUCIButtonOption }

function TUCIButtonOption.GetValue: string;
begin
  Result := '';
end;

procedure TUCIButtonOption.SetValue(const AValue: string);
begin

end;

function TUCIButtonOption.Typ: TType;
begin
  Result := tpButton;
end;

constructor TUCIButtonOption.Create(const AName: string);
begin
  FName := AName;
end;

end.
