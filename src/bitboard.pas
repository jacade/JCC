unit BitBoard;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils;

// We map a 8x8 board from the upper left corner (a8) to the lower right corner (h1)
// by going left to right and then up to down.
// So a8 would be 0 and h1 is 2^63

const
  Ranks: array[1..8] of QWord =
    (18374686479671623680, 71776119061217280, 280375465082880,
    1095216660480, 4278190080, 16711680, 65280, 255);
  // From A to H
  Files: array[1..8] of QWord =
    (72340172838076673, 144680345676153346, 289360691352306692, 578721382704613384,
    1157442765409226768, 2314885530818453536, 4629771061636907072, 9259542123273814144);

function BitBoardToStr(ABitBoard: QWord): string;
function IsBitSet(ABitBoard: QWord; Index: byte): boolean;
function NumberOfLeadingZeroes(AWord: QWord): integer;
function NumberOfTrailingZeroes(AWord: QWord): integer;

implementation

function BitBoardToStr(ABitBoard: QWord): string;
var
  j: integer;
begin
  Result := '';
  for j := 1 to 64 do
  begin
    if ABitBoard mod 2 = 1 then
      Result := '1' + Result
    else
      Result := '0' + Result;
    ABitBoard := ABitBoard shr 1;
  end;
end;

function IsBitSet(ABitBoard: QWord; Index: byte): boolean;
begin
  Result := ((ABitBoard shr Index) and 1) = 1;
end;

function NumberOfLeadingZeroes(AWord: QWord): integer;
begin
  Result := 64;
  while AWord > 0 do
  begin
    AWord := AWord shr 1;
    Dec(Result);
  end;
end;

function NumberOfTrailingZeroes(AWord: QWord): integer;
begin
  if AWord = 0 then
    Result := 64
  else
  begin
    Result := 0;
    while (AWord and 1) = 0 do
    begin
      AWord := AWord shr 1;
      Inc(Result);
    end;
  end;
end;


end.
