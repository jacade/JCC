{ JCC (Jan's Chess Componenents) - This file has no use and may be deleted in the future
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
}

unit Notation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mMoveList, Pieces, contnrs;

type

  { TComment }

  TComment = class
  private
    FText: string;
    FNAG: array[1..3] of byte; // Numeric Annotation Glyph
    function GetNAG(Index: byte): byte;
    procedure SetNAG(Index: byte; AValue: byte);
  public
    property Text: string read FText write FText;
    // Index should be 1 for move annotation, 2 for position annotation and 3 for time pressure description
    // All other values throw an OutOfBounds-Exception
    property NAG[Index: byte]: byte read GetNAG write SetNAG;
  end;

  { TLine }

  TLine = class
  private
    FComments: TFPObjectList;
    FLines: TFPObjectList;
    FMoveList: TMoveList;
    FPlieNumber: integer;
    function GetComments(Index: integer): TComment;
    function GetSubLines(Index: integer): TLine;
    procedure SetComments(Index: integer; AValue: TComment);
    procedure SetSubLines(Index: integer; AValue: TLine);
  public
    constructor Create(StartPlieNumber: integer);
    destructor Destroy; override;
    property Comments[Index: integer]: TComment read GetComments write SetComments;
    property MoveList: TMoveList read FMoveList;
    property PlieNumber: integer read FPlieNumber;
    property SubLines[Index: integer]: TLine read GetSubLines write SetSubLines;
  end;

implementation

{ TComment }

function TComment.GetNAG(Index: byte): byte;
begin
  Result := FNAG[Index];
end;

procedure TComment.SetNAG(Index: byte; AValue: byte);
begin
  FNAG[Index] := AValue;
end;

{ TLine }

function TLine.GetComments(Index: integer): TComment;
begin
  Result := FComments[Index] as TComment;
end;

function TLine.GetSubLines(Index: integer): TLine;
begin
  Result := FLines[Index] as TLine;
end;

procedure TLine.SetComments(Index: integer; AValue: TComment);
begin
  FComments.Items[Index] := AValue;
end;

procedure TLine.SetSubLines(Index: integer; AValue: TLine);
begin
  FLines.Items[Index] := AValue;
end;

constructor TLine.Create(StartPlieNumber: integer);
begin
  FPlieNumber := StartPlieNumber;
  FComments := TFPObjectList.Create(True);
  FLines := TFPObjectList.Create(True);
end;

destructor TLine.Destroy;
begin
  FComments.Free;
  FLines.Free;
end;

end.

