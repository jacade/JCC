{ JCC (Jan's Chess Componenents) - This file contains classes to store moves
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
      FData: Word;
    function GetDest: Byte;
    function GetDest8x8: TSquare8x8;
    function GetPromotionPiece: TPieceType;
    function GetStart: Byte;
    function GetStart8x8: TSquare8x8;
    procedure SetDest(AValue: Byte);
    procedure SetDest8x8(AValue: TSquare8x8);
    procedure SetPromotionPiece(AValue: TPieceType);
    procedure SetStart(AValue: Byte);
    procedure SetStart8x8(AValue: TSquare8x8);
  public
    function Copy: TMove; virtual;
    constructor Create(AStart, ADest: Byte; APromotionPiece: TPieceType = ptEmpty);
    function Equals(Obj: TObject): boolean; override;
  public
    property Dest: Byte read GetDest write SetDest;
    property Dest8x8: TSquare8x8 read GetDest8x8 write SetDest8x8;
    property Start: Byte read GetStart write SetStart;
    property Start8x8: TSquare8x8 read GetStart8x8 write SetStart8x8;
    property PromotionPiece: TPieceType read GetPromotionPiece write SetPromotionPiece;
  end;

  { TMoveList }

  TMoveList = class(specialize TFPGObjectList<TMove>)
  public
    // Note: Elements are fully copied
    procedure AddList(AnotherMoveList: TMoveList);
    function Filter(Start: Byte; Dest: Byte = 255; PromoPiece: TPieceType = ptEmpty): TMoveList;
    function ToStringList: TStringList;
  end;


function SquareToString(AAlgebraicSquare: TAlgebraicSquare): string;
function AlgebraicMoveToString(AAlgebraicMove: TMove): string;

operator = (ASquare1, ASquare2: TSquare8x8): boolean;

operator := (ASquare8x8: TSquare8x8): TAlgebraicSquare;
operator := (AAlgebraicSquare: TAlgebraicSquare): TSquare8x8;

operator in (AMove: TMove; AMoveList: TMoveList): Boolean;

function AlgebraicSquare(AAlgebraicFile: TAlgebraicFile;
  AAlgebraicRank: TAlgebraicRank): TAlgebraicSquare;

function CreateMove(AStart, ADest: TSquare8x8; APromotionPiece: TPieceType=ptEmpty):TMove;

// Board representation for the following method
// 0  1  2  3  4  5  6  7
// 8  9  10 11 12 13 14 15
// 16 17 18 19 20 21 22 23
// 24 25 26 27 28 29 30 31
// 32 33 34 35 36 37 38 39
// 40 41 42 43 44 45 46 47
// 48 49 50 51 52 53 54 55
// 56 57 58 59 60 61 62 63

function CreateMoveFromInt(AStart, ADest: Integer; APromotionPiece: TPieceType=ptEmpty): TMove;

const
  // 0..63 board constants
  Rank1 = [56, 57, 58, 59, 60, 61, 62, 63];
  Rank2 = [48, 49, 50, 51, 52, 53, 54, 55];
  Rank3 = [40, 41, 42, 43, 44, 45, 46, 47];
  Rank4 = [32, 33, 34, 35, 36, 37, 38, 39];
  Rank5 = [24, 25, 26, 27, 28, 29, 30, 31];
  Rank6 = [16, 17, 18, 19, 20, 21, 22, 23];
  Rank7 = [8, 9, 10, 11, 12, 13, 14, 15];
  Rank8 = [0, 1, 2, 3, 4, 5, 6, 7];

  FileA = [0, 8, 16, 24, 32, 40, 48, 56];
  FileB = [1, 9, 17, 25, 33, 41, 49, 57];
  FileC = [2, 10, 18, 26, 34, 42, 50, 58];
  FileD = [3, 11, 19, 27, 35, 43, 51, 59];
  FileE = [4, 12, 20, 28, 36, 44, 52, 60];
  FileF = [5, 13, 21, 29, 37, 45, 53, 61];
  FileG = [6, 14, 22, 30, 38, 46, 54, 62];
  FileH = [7, 15, 23, 31, 39, 47, 55, 63];


implementation

function SquareToString(AAlgebraicSquare: TAlgebraicSquare): string;
begin
  Result := AAlgebraicSquare.RFile + AAlgebraicSquare.RRank;
end;

function AlgebraicMoveToString(AAlgebraicMove: TMove): string;
begin
  Result := SquareToString(AAlgebraicMove.Start8x8) +
    SquareToString(AAlgebraicMove.Dest8x8);
end;

operator = (ASquare1, ASquare2: TSquare8x8): boolean;
begin
  Result := (ASquare1.RFile = ASquare2.RFile) and (ASquare1.RRank = ASquare2.RRank);
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

operator in(AMove: TMove; AMoveList: TMoveList): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to AMoveList.Count - 1 do
    Result := Result or (AMoveList.Items[i] = AMove);
end;

function AlgebraicSquare(AAlgebraicFile: TAlgebraicFile;
  AAlgebraicRank: TAlgebraicRank): TAlgebraicSquare;
begin
  Result.RFile := AAlgebraicFile;
  Result.RRank := AAlgebraicRank;
end;

function CreateMove(AStart, ADest: TSquare8x8; APromotionPiece: TPieceType
  ): TMove;
begin
  Result := TMove.Create(0,0);
  Result.Start8x8 := AStart;
  Result.Dest8x8 := ADest;
  Result.PromotionPiece := APromotionPiece;
end;

function CreateMoveFromInt(AStart, ADest: Integer; APromotionPiece: TPieceType
  ): TMove;
begin
  Result := TMove.Create(AStart, ADest, APromotionPiece);
end;

{ TMove }

function TMove.GetDest: Byte;
begin
  Result := (FData and 4032) shr 6;
end;

function TMove.GetDest8x8: TSquare8x8;
var
  Temp: Byte;
begin
  Temp := GetDest;
  Result.RFile := Temp mod 8 + 1;
  Result.RRank := 8 - Temp div 8 ;
end;

function TMove.GetPromotionPiece: TPieceType;
var
  Temp: Word;
begin
  Temp := (FData and 61440) shr 12;
  case Temp of
    0: Result := ptEmpty;
    1: Result := ptWPawn;
    2: Result := ptWKnight;
    3: Result := ptWBishop;
    4: Result := ptWRook;
    5: Result := ptWQueen;
    6: Result := ptWKing;
    7: Result := ptBPawn;
    8: Result := ptBKnight;
    9: Result := ptBBishop;
    10: Result := ptBRook;
    11: Result := ptBQueen;
    12: Result := ptBKing;
    15: Result := ptOff;
  end;
end;

function TMove.GetStart: Byte;
begin
  Result := FData and 63;
end;

function TMove.GetStart8x8: TSquare8x8;
var
  Temp: Byte;
begin
  Temp := GetStart;
  Result.RFile := Temp mod 8 + 1;
  Result.RRank := 8 - Temp div 8 ;
end;

procedure TMove.SetDest(AValue: Byte);
begin
  FData := (FData and not Word(4032)) + AValue shl 6;
end;

procedure TMove.SetDest8x8(AValue: TSquare8x8);
begin
  SetDest((8 - AValue.RRank) * 8 + AValue.RFile - 1);
end;

procedure TMove.SetPromotionPiece(AValue: TPieceType);
var
  Temp: Word;
begin
  case AValue of
    ptEmpty: Temp := 0;
    ptWPawn: Temp := 1;
    ptWKnight: Temp := 2;
    ptWBishop: Temp := 3;
    ptWRook: Temp := 4;
    ptWQueen: Temp := 5;
    ptWKing: Temp := 6;
    ptBPawn: Temp := 7;
    ptBKnight: Temp := 8;
    ptBBishop: Temp := 9;
    ptBRook: Temp := 10;
    ptBQueen: Temp := 11;
    ptBKing: Temp := 12;
    ptOff: Temp := 15;
  end;
  FData := (FData and not 61440) + Temp shl 12;
end;

procedure TMove.SetStart(AValue: Byte);
begin
  FData := (FData and not Word(63)) + AValue;
end;

procedure TMove.SetStart8x8(AValue: TSquare8x8);
begin
  SetStart((8 - AValue.RRank) * 8 + AValue.RFile - 1);
end;

function TMove.Copy: TMove;
begin
  Result := TMove.Create(FData and 63, (FData and 4032) shr 6 , PromotionPiece);
end;

constructor TMove.Create(AStart, ADest: Byte; APromotionPiece: TPieceType);
var
  Temp: Word;
begin
  case APromotionPiece of
    ptEmpty: Temp := 0;
    ptWPawn: Temp := 1;
    ptWKnight: Temp := 2;
    ptWBishop: Temp := 3;
    ptWRook: Temp := 4;
    ptWQueen: Temp := 5;
    ptWKing: Temp := 6;
    ptBPawn: Temp := 7;
    ptBKnight: Temp := 8;
    ptBBishop: Temp := 9;
    ptBRook: Temp := 10;
    ptBQueen: Temp := 11;
    ptBKing: Temp := 12;
    ptOff: Temp := 15;
  end;
  FData := AStart + Word(ADest) shl 6 + Temp shl 12;
end;

function TMove.Equals(Obj: TObject): boolean;
begin
  Result := inherited Equals(Obj);
  Result := Result or ((Obj is TMove) and ((Obj as TMove).FData = Self.FData));
end;

{ TMoveList }

procedure TMoveList.AddList(AnotherMoveList: TMoveList);
var
  AMove: TMove;
begin
  for AMove in AnotherMoveList do
    Add(AMove);
end;

function TMoveList.Filter(Start: Byte; Dest: Byte; PromoPiece: TPieceType
  ): TMoveList;
var
  NoFilterDest, NoFilterPromoPiece: Boolean;
  AMove: TMove;
begin
  NoFilterDest := Dest = 255;
  NoFilterPromoPiece:= PromoPiece = ptEmpty;
  Result := TMoveList.Create(False);
  for AMove in Self do
    if (Start = AMove.Start) and (NoFilterDest or (Dest = AMove.Dest)) and
      (NoFilterPromoPiece or (PromoPiece = AMove.PromotionPiece)) then
        Result.Add(AMove);
end;

function TMoveList.ToStringList: TStringList;
var
  i: integer;
begin
  Result := TStringList.Create;
  for i := 0 to Count - 1 do
    Result.Add(AlgebraicMoveToString(Items[i]));
end;

end.
