{ JCC (Jan's Chess Componenents) - This file contains classes to store moves
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

unit MoveList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, Pieces;

type

  // ----Algebraic Notation-----
  // | a8 b8 c8 d8 e8 f8 g8 h8 |
  // | a7 b7 c7 d7 e7 f7 g7 h7 |
  // | a6 b6 c6 d6 e6 f6 g6 h6 |
  // | a5 b5 c5 d5 e5 f5 g5 h5 |
  // | a4 b4 c4 d4 e4 f4 g4 h4 |
  // | a3 b3 c3 d3 e3 f3 g3 h3 |
  // | a2 b2 c2 d2 e2 f2 g2 h2 |
  // | a1 b1 c1 d1 e1 f1 g1 h1 |
  // ---------------------------
  TAlgebraicFile = 'a'..'z';
  TAlgebraicRank = '1'..'8';

  // Only used internally
  // -------------8x8 Board-------------
  // | 1,8 2,8 3,8 4,8 5,8 6,8 7,8 8,8 |
  // | 1,7 2,7 3,7 4,7 5,7 6,7 7,7 8,7 |
  // | 1,6 2,6 3,6 4,6 5,6 6,6 7,6 8,6 |
  // | 1,5 2,5 3,5 4,5 5,5 6,5 7,5 8,5 |
  // | 1,4 2,4 3,4 4,4 5,4 6,4 7,4 8,4 |
  // | 1,3 2,3 3,3 4,3 5,3 6,3 7,3 8,3 |
  // | 1,2 2,2 3,2 4,2 5,2 6,2 7,2 8,2 |
  // | 1,1 2,1 3,1 4,1 5,1 6,1 7,1 8,1 |
  // -----------------------------------
  TFile8x8 = 1..8;
  TRank8x8 = 1..8;

  // -------------------10x12 Board----------------------------
  // |   0      1    2    3    4    5    6    7    8      9  |
  // |  10     11   12   13   14   15   16   17   18     19  |
  // ----------------------------------------------------------
  // |  20  |  21   22   23   24   25   26   27   28  |  29  |
  // |  30  |  31   32   33   34   35   36   37   38  |  39  |
  // |  40  |  41   42   43   44   45   46   47   48  |  49  |
  // |  50  |  51   52   53   54   55   56   57   58  |  59  |
  // |  60  |  61   62   63   64   65   66   67   68  |  69  |
  // |  70  |  71   72   73   74   75   76   77   78  |  79  |
  // |  80  |  81   82   83   84   85   86   87   88  |  89  |
  // |  90  |  91   92   93   94   95   96   97   98  |  99  |
  // ----------------------------------------------------------
  // | 100    101  102  103  104  105  106  107  108    109  |
  // | 110    111  112  113  114  115  116  117  118    119  |
  // ----------------------------------------------------------
  TSquare10x12 = 0..119;

  TAlgebraicSquare = record
    RFile: TAlgebraicFile;
    RRank: TAlgebraicRank;
  end;

  TSquare8x8 = record
    RFile: TFile8x8;
    RRank: TRank8x8;
  end;

  { TMove }

  TMove = class
  private
    FStart, FDest: TSquare10x12;
    FPromotionPiece: TPieceType;
    function GetDest: TSquare8x8;
    function GetStart: TSquare8x8;
  public
    constructor Create(AStart, ADest: TSquare8x8; APromotionPiece: TPieceType = ptEmpty);
    function IsEqual(AMove: TMove): boolean;
    function Copy: TMove;
    property Dest: TSquare8x8 read GetDest;
    property PromotionPiece: TPieceType read FPromotionPiece;
    property Start: TSquare8x8 read GetStart;
  end;

  { TMoveList }

  TMoveList = specialize TFPGObjectList<TMove>;

  { TMoveListHelper }

  TMoveListHelper = class helper for TMoveList
    // Note: Elements are fully copied
    procedure AddList(AnotherMoveList: TMoveList);
    function ToStringList: TStringList;
  end;


function SquareToString(AAlgebraicSquare: TAlgebraicSquare): string;
function AlgebraicMoveToString(AAlgebraicMove: TMove): string;

operator = (ASquare1, ASquare2: TSquare8x8): boolean;

operator := (AAlgebraicSquare: TAlgebraicSquare): TSquare10x12;
operator := (ASquare10x12: TSquare10x12): TSquare8x8;
operator := (ASquare8x8: TSquare8x8): TAlgebraicSquare;
operator := (AAlgebraicSquare: TAlgebraicSquare): TSquare8x8;
operator := (ASquare10x12: TSquare10x12): TAlgebraicSquare;
operator := (ASquare8x8: TSquare8x8): TSquare10x12;

operator in (AMove: TMove; AMoveList: TMoveList): Boolean;

function AlgebraicSquare(AAlgebraicFile: TAlgebraicFile;
  AAlgebraicRank: TAlgebraicRank): TAlgebraicSquare;

const
  OffSquares = [0..20, 29, 30, 39, 40, 49, 50, 59, 60, 69, 70, 79,
    80, 89, 90, 99..119];
  ValidSquares = [21..28, 31..38, 41..48, 51..58, 61..68, 71..78, 81..88, 91..98];

  // 10x12 board constants
  Rank1 = [91, 92, 93, 94, 95, 96, 97, 98];
  Rank2 = [81, 82, 83, 84, 85, 86, 87, 88];
  Rank3 = [71, 72, 73, 74, 75, 76, 77, 78];
  Rank4 = [61, 62, 63, 64, 65, 66, 67, 68];
  Rank5 = [51, 52, 53, 54, 55, 56, 57, 58];
  Rank6 = [41, 42, 43, 44, 45, 46, 47, 48];
  Rank7 = [31, 32, 33, 34, 35, 36, 37, 38];
  Rank8 = [21, 22, 23, 24, 25, 26, 27, 28];

  FileA = [21, 31, 41, 51, 61, 71, 81, 91];
  FileB = [22, 32, 42, 52, 62, 72, 82, 92];
  FileC = [23, 33, 43, 53, 63, 73, 83, 93];
  FileD = [24, 34, 44, 54, 64, 74, 84, 94];
  FileE = [25, 35, 45, 55, 65, 75, 85, 95];
  FileF = [26, 36, 46, 56, 66, 76, 86, 96];
  FileG = [27, 37, 47, 57, 67, 77, 87, 97];
  FileH = [28, 38, 48, 58, 68, 78, 88, 98];


implementation

function SquareToString(AAlgebraicSquare: TAlgebraicSquare): string;
begin
  Result := AAlgebraicSquare.RFile + AAlgebraicSquare.RRank;
end;

function AlgebraicMoveToString(AAlgebraicMove: TMove): string;
begin
  Result := SquareToString(AAlgebraicMove.Start) +
    SquareToString(AAlgebraicMove.Dest);
end;

operator = (ASquare1, ASquare2: TSquare8x8): boolean;
begin
  Result := (ASquare1.RFile = ASquare2.RFile) and (ASquare1.RRank = ASquare2.RRank);
end;

operator := (AAlgebraicSquare: TAlgebraicSquare): TSquare10x12;
begin
  Result := TSquare8x8(AAlgebraicSquare);
end;

operator := (ASquare10x12: TSquare10x12): TSquare8x8;
begin
  if ASquare10x12 in OffSquares then
    raise ERangeError.Create('The given square ' + IntToStr(ASquare10x12) +
      ' is not on the board!');
  Result.RFile := ASquare10x12 mod 10;
  Result.RRank := 10 - (ASquare10x12 div 10);
end;

operator := (ASquare8x8: TSquare8x8): TAlgebraicSquare;
begin
  Result.RFile := Chr(ASquare8x8.RFile + 96);
  Result.RRank := Chr(ASquare8x8.RRank + 48);
end;

operator := (AAlgebraicSquare: TAlgebraicSquare): TSquare8x8;
begin
  Result.RFile := Ord(AAlgebraicSquare.RFile) - 96;
  Result.RRank := Ord(AAlgebraicSquare.RRank) - 48;
end;

operator := (ASquare10x12: TSquare10x12): TAlgebraicSquare;
begin
  Result := TSquare8x8(ASquare10x12);
end;

operator := (ASquare8x8: TSquare8x8): TSquare10x12;
begin
  Result := ASquare8x8.RFile + (10 - ASquare8x8.RRank) * 10;
end;

operator in(AMove: TMove; AMoveList: TMoveList): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to AMoveList.Count - 1 do
    Result := Result or AMoveList.Items[i].IsEqual(AMove);
end;

function AlgebraicSquare(AAlgebraicFile: TAlgebraicFile;
  AAlgebraicRank: TAlgebraicRank): TAlgebraicSquare;
begin
  Result.RFile := AAlgebraicFile;
  Result.RRank := AAlgebraicRank;
end;

{ TMoveListHelper }

procedure TMoveListHelper.AddList(AnotherMoveList: TMoveList);
var
  Move: TMove;
begin
  for Move in AnotherMoveList do
    Add(Move.Copy);
end;

function TMoveListHelper.ToStringList: TStringList;
var
  i: integer;
begin
  Result := TStringList.Create;
  for i := 0 to Count - 1 do
    Result.Add(AlgebraicMoveToString(Items[i]));
end;

{ TMove }

function TMove.GetDest: TSquare8x8;
begin
  Result := FDest;
end;

function TMove.GetStart: TSquare8x8;
begin
  Result := FStart;
end;

constructor TMove.Create(AStart, ADest: TSquare8x8; APromotionPiece: TPieceType);
begin
  FStart := AStart;
  fDest := ADest;
  FPromotionPiece := APromotionPiece;
end;

function TMove.IsEqual(AMove: TMove): boolean;
begin
  Result := (FStart = AMove.Start) and (FDest = AMove.Dest) and
    (FPromotionPiece = AMove.PromotionPiece);
end;

function TMove.Copy: TMove;
begin
  Result := TMove.Create(FStart, FDest, FPromotionPiece);
end;

end.
