{ JCC (Jan's Chess Componenents) - This file contains chess piece representation
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

unit Pieces;


{$mode objfpc}{$H+}

interface

uses SysUtils;

type
  {$PACKENUM 1}
  TBasicPieceType = (bptPawn, bptKnight, bptBishop, bptRook, bptQueen, bptKing);

  {$PACKENUM 1}
  TPieceType = (ptEmpty = 0, // empty Square
    // White Pieces
    ptWPawn = 1, ptWKnight = 2, ptWBishop = 3, ptWRook = 4, ptWQueen = 5, ptWKing = 6,
    // Black Pieces
    ptBPawn = 129, ptBKnight = 130, ptBBishop = 131, ptBRook = 132, ptBQueen = 133,
    ptBKing = 134,
    // Used to mark that we are off the board
    ptOff = 255);

  TPieceTypes = set of TPieceType;


const
  WhitePieces = [ptWPawn, ptWKnight, ptWBishop, ptWRook, ptWQueen, ptWKing];
  BlackPieces = [ptBPawn, ptBKnight, ptBBishop, ptBRook, ptBQueen, ptBKing];

function BasicPieceType(APiece: TPieceType): TBasicPieceType;
function IsWhite(Piece: TPieceType): boolean;
function PieceType(ABasicPieceType: TBasicPieceType; WhiteColor: boolean): TPieceType;
function SameColor(Piece1, Piece2: TPieceType): boolean;

implementation

function BasicPieceType(APiece: TPieceType): TBasicPieceType;
begin
  case APiece of
    ptEmpty, ptOff: raise Exception.Create('There exist no basictype for these!');
    ptWPawn, ptBPawn: Result := bptPawn;
    ptWKnight, ptBKnight: Result := bptKnight;
    ptWBishop, ptBBishop: Result := bptBishop;
    ptWRook, ptBRook:Result :=  bptRook;
    ptWQueen, ptBQueen: Result := bptQueen;
    ptWKing, ptBKing:Result :=  bptKing;
  end;
end;

function IsWhite(Piece: TPieceType): boolean;
begin
  Result := (Ord(Piece) > 0) and (Ord(Piece) < 7);
end;

function PieceType(ABasicPieceType: TBasicPieceType; WhiteColor: boolean): TPieceType;
begin
  case ABasicPieceType of
    bptPawn: if WhiteColor then
        Result := ptWPawn
      else
        Result := ptBPawn;
    bptKnight: if WhiteColor then
        Result := ptWKnight
      else
        Result := ptBKnight;
    bptBishop: if WhiteColor then
        Result := ptWBishop
      else
        Result := ptBBishop;
    bptRook: if WhiteColor then
        Result := ptWRook
      else
        Result := ptBRook;
    bptQueen: if WhiteColor then
        Result := ptWQueen
      else
        Result := ptBQueen;
    bptKing: if WhiteColor then
        Result := ptWKing
      else
        Result := ptBKing;
  end;
end;

function SameColor(Piece1, Piece2: TPieceType): boolean;
begin
  Result := (Ord(Piece1) and 128) = (Ord(Piece2) and 128);
end;

end.
