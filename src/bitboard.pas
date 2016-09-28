{ JCC (Jan's Chess Componenents) - This file contains constants and functions to handle bitboards
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
unit BitBoard;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils;

// We map a 8x8 board from the upper left corner (a8) to the lower right corner (h1)
// by going left to right and then up to down.
// So a8 would be 2^0 and h1 is 2^63

type
  TBitBoard = QWord;

const
  // From 1 to 8
  Ranks: array[1..8] of TBitBoard =
    (18374686479671623680, 71776119061217280, 280375465082880,
    1095216660480, 4278190080, 16711680, 65280, 255);

  // From A to H
  Files: array[1..8] of TBitBoard =
    (72340172838076673, 144680345676153346, 289360691352306692, 578721382704613384,
    1157442765409226768, 2314885530818453536, 4629771061636907072, 9259542123273814144);

  // Diagonals from top left to bottom right
  Diagonals: array[1..15] of TBitBoard =
    (1, 258, 66052, 16909320, 4328785936, 1108169199648,
    283691315109952, 72624976668147840, 145249953336295424,
    290499906672525312, 580999813328273408, 1161999622361579520,
    2323998145211531264, 4647714815446351872, 9223372036854775808);

  // Anti-Diagonals from top right to bottom left
  AntiDiagonals: array[1..15] of TBitBoard =
    (128, 32832, 8405024, 2151686160, 550831656968,
    141012904183812, 36099303471055874, 9241421688590303745, 4620710844295151872,
    2310355422147575808, 1155177711073755136, 577588855528488960,
    288794425616760832, 144396663052566528, 72057594037927936);

  // Pre-calculated knight moves from A8 to H1
  KnightMoves: array[0..63] of TBitBoard =
    (132096, 329728, 659712, 1319424, 2638848, 5277696, 10489856,
    4202496, 33816580, 84410376, 168886289, 337772578, 675545156,
    1351090312, 2685403152, 1075839008, 8657044482, 21609056261,
    43234889994, 86469779988, 172939559976, 345879119952, 687463207072,
    275414786112, 2216203387392, 5531918402816, 11068131838464,
    22136263676928, 44272527353856, 88545054707712, 175990581010432,
    70506185244672, 567348067172352, 1416171111120896, 2833441750646784,
    5666883501293568, 11333767002587136, 22667534005174272,
    45053588738670592, 18049583422636032, 145241105196122112,
    362539804446949376, 725361088165576704, 1450722176331153408,
    2901444352662306816, 5802888705324613632, 11533718717099671552,
    4620693356194824192, 288234782788157440, 576469569871282176, 1224997833292120064,
    2449995666584240128, 4899991333168480256, 9799982666336960512,
    1152939783987658752, 2305878468463689728, 1128098930098176, 2257297371824128,
    4796069720358912, 9592139440717824, 19184278881435648, 38368557762871296,
    4679521487814656, 9077567998918656);

  // Pre-calculated king moves from A8 to H1
  KingMoves: array[0..63] of TBitBoard =
    (770, 1797, 3594, 7188, 14376, 28752, 57504, 49216, 197123, 460039,
    920078, 1840156, 3680312, 7360624, 14721248, 12599488, 50463488,
    117769984, 235539968, 471079936, 942159872, 1884319744, 3768639488,
    3225468928, 12918652928, 30149115904, 60298231808, 120596463616,
    241192927232, 482385854464, 964771708928, 825720045568, 3307175149568,
    7718173671424, 15436347342848, 30872694685696, 61745389371392,
    123490778742784, 246981557485568, 211384331665408, 846636838289408,
    1975852459884544, 3951704919769088, 7903409839538176, 15806819679076352,
    31613639358152704, 63227278716305408, 54114388906344448, 216739030602088448,
    505818229730443264, 1011636459460886528, 2023272918921773056,
    4046545837843546112, 8093091675687092224, 16186183351374184448,
    13853283560024178688, 144959613005987840, 362258295026614272,
    724516590053228544, 1449033180106457088, 2898066360212914176,
    5796132720425828352, 11592265440851656704, 4665729213955833856);

  // Empty castling squares e. g. f1, g1 for white kingside, b1, c1, d1 for queenside
  // White: Kingside, Queenside, Black: Kingside, Queenside
  CastlingSquares: array[1..4] of TBitBoard =
    (6917529027641081856, 1008806316530991104, 96, 14);

  // Black and white squares on board
  BlackSquares: TBitBoard = 6172840429334713770;
  WhiteSquares: TBitBoard = 12273903644374837845;

function BitBoardToStr(ABitBoard: TBitBoard): string;
function IsBitSet(ABitBoard: TBitBoard; Index: byte): boolean;
function NumberOfLeadingZeroes(const ABitBoard: TBitBoard): integer;
function NumberOfTrailingZeroes(const ABitBoard: TBitBoard): integer;
function ReverseBitBoard(ABitBoard: TBitBoard): TBitBoard;

implementation

function BitBoardToStr(ABitBoard: TBitBoard): string;
var
  j: integer;
begin
  Result := '';
  for j := 1 to 64 do
  begin
    if (ABitBoard and 1) = 1 then
      Result := Result + '1'
    else
      Result := Result + '0';
    ABitBoard := ABitBoard shr 1;
  end;
  for j := 8 downto 1 do
    Insert(LineEnding, Result, j * 8 + 1);
end;

function IsBitSet(ABitBoard: TBitBoard; Index: byte): boolean;
begin
  Result := ((ABitBoard shr Index) and 1) = 1;
end;

function NumberOfLeadingZeroes(const ABitBoard: TBitBoard): integer;
begin
  Result := 63 - BsrQWord(ABitBoard);
end;

function NumberOfTrailingZeroes(const ABitBoard: TBitBoard): integer;
begin
  Result := BsfQWord(ABitBoard);
end;

function ReverseBitBoard(ABitBoard: TBitBoard): TBitBoard;
begin
  // based on http://graphics.stanford.edu/~seander/bithacks.html#ReverseParallel
  // swap odd and even bits
  ABitBoard := ((ABitBoard shr 1) and $5555555555555555) or
    ((ABitBoard and $5555555555555555) shl 1);
  // swap consecutive pairs
  ABitBoard := ((ABitBoard shr 2) and $3333333333333333) or
    ((ABitBoard and $3333333333333333) shl 2);
  // swap nibbles ...
  ABitBoard := ((ABitBoard shr 4) and $0F0F0F0F0F0F0F0F) or
    ((ABitBoard and $0F0F0F0F0F0F0F0F) shl 4);
  // swap bytes
  ABitBoard := ((ABitBoard shr 8) and $00FF00FF00FF00FF) or
    ((ABitBoard and $00FF00FF00FF00FF) shl 8);
  // swap 2-byte long pairs
  ABitBoard := ((ABitBoard shr 16) and $0000FFFF0000FFFF) or
    ((ABitBoard and $0000FFFF0000FFFF) shl 16);
  // swap 4-byte long pairs
  Result := (ABitBoard shr 32) or (ABitBoard shl 32);
end;

end.
