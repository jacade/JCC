{ JCC (Jan's Chess Componenents) - This file contains classes to handle options given by chess engines
  Copyright (C) 2016  Jan Dette

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
}unit UCIOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrTools;

type

  TType = (tpCheck, tpSpin, tpCombo, tpButton, tpString);

  { TOption }

  TOption = class
  protected
    FName: string;
  public
    function Typ: TType; virtual; abstract;
    property Name: string read FName;
  end;

  { TButtonOption }

  TButtonOption = class(TOption)
  public
    function Typ: TType; override;
    constructor Create(AName: string);
  end;

  { TCheckOption }

  TCheckOption = class(TOption)
  private
    FDefault, FValue: boolean;
  public
    constructor Create(AName, AValue: string);
    function Typ: TType; override;
    property DefaultValue: boolean read FDefault;
    property Value: boolean read FValue write FValue;
  end;

  { TComboOption }

  TComboOption = class(TOption)
  private
    FDefault, FValue: string;
    FVariables: TStringList;
  public
    constructor Create(AName, AValue, Vars: string);
    destructor Destroy; override;
    function Typ: TType; override;
    property DefaultValue: string read FDefault;
    property Variables: TStringList read FVariables;
    property Value: string read FValue write FValue;
  end;

  { TSpinOption }

  TSpinOption = class(TOption)
  private
    FDefault, FMax, FMin, FValue: integer;
  public
    constructor Create(AName, AValue, AMin, AMax: string);
    function Typ: TType; override;
    property DefaultValue: integer read FDefault;
    property Maximum: integer read FMax;
    property Minimum: integer read FMin;
    property Value: integer read FValue write FValue;
  end;

  { TStringOption }

  TStringOption = class(TOption)
  private
    FDefault, FValue: string;
  public
    constructor Create(AName, AValue: string);
    function Typ: TType; override;
    property DefaultValue: string read FDefault;
    property Value: string read FValue write FValue;
  end;

implementation

{ TStringOption }

constructor TStringOption.Create(AName, AValue: string);
begin
  FName := AName;
  if Length(AValue) = 0 then
    FDefault:='<empty>'
  else
    FDefault := AValue;
  FVAlue := FDefault;
end;

function TStringOption.Typ: TType;
begin
  Result := tpString;
end;

{ TSpinOption }

constructor TSpinOption.Create(AName, AValue, AMin, AMax: string);
begin
  FName := AName;
  FDefault := StrToInt(AValue);
  FValue := FDefault;
  FMax := StrToInt(AMax);
  FMin := StrToInt(AMin);
end;

function TSpinOption.Typ: TType;
begin
  Result := tpSpin;
end;

{ TComboOption }

constructor TComboOption.Create(AName, AValue, Vars: string);
begin
  FName := AName;
  FVariables:= Split(Vars, ' ');
  FDefault := AValue;
  FValue := FDefault;
end;

destructor TComboOption.Destroy;
begin
  FVariables.Free;
  inherited Destroy;
end;

function TComboOption.Typ: TType;
begin
  Result := tpCombo;
end;

{ TCheckOption }

constructor TCheckOption.Create(AName, AValue: string);
begin
  FName := AName;
  FDefault := AValue = 'true';
  FValue := FDefault;
end;

function TCheckOption.Typ: TType;
begin
  Result := tpCheck;
end;

{ TButtonOption }

function TButtonOption.Typ: TType;
begin
  Result := tpButton;
end;

constructor TButtonOption.Create(AName: string);
begin
  FName := AName;
end;

end.
