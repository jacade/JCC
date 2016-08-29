{ JCC (Jan's Chess Componenents) - This file contains chess piece representation
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

unit Pieces;

{$mode objfpc}{$H+}

interface

type
  {$PACKENUM 1}
  TPieceType = (ptEmpty = 0, // empty Square
    // White Pieces
    ptWPawn = 1, ptWKnight = 2, ptWBishop = 3, ptWRook = 4, ptWQueen = 5, ptWKing = 6,
    // Black Pieces
    ptBPawn = 129, ptBKnight = 130, ptBBishop = 131, ptBRook = 132, ptBQueen = 133,
    ptBKing = 134,
    // Used to mark that we are off the board
    ptOff = 255);


const
  WhitePieces = [ptWPawn, ptWKnight, ptWBishop, ptWRook, ptWQueen, ptWKing];
  BlackPieces = [ptBPawn, ptBKnight, ptBBishop, ptBRook, ptBQueen, ptBKing];

function IsWhite(Piece: TPieceType): boolean;
function SameColor(Piece1, Piece2: TPieceType): boolean;

implementation

function IsWhite(Piece: TPieceType): boolean;
begin
  Result := (Ord(Piece) > 0) and (Ord(Piece) < 7);
end;

function SameColor(Piece1, Piece2: TPieceType): boolean;
begin
  Result := (Ord(Piece1) and 128) = (Ord(Piece2) and 128);
end;

end.
