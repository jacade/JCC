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
var
  Temp: Qword;
begin
  if AWord = 0 then
    Result := 64
  else
  begin
    Result := 63;
    Temp := AWord;
    Temp := AWord shr 32;
    if Temp <> 0 then
    begin
      Result := Result - 32;
      AWord := Temp;
    end;
    Temp := AWord shr 16;
    if Temp <> 0 then
    begin
      Result := Result - 16;
      AWord := Temp;
    end;
    Temp := AWord shr 8;
    if Temp <> 0 then
    begin
      Result := Result - 8;
      AWord := Temp;
    end;
    Temp := AWord shr 4;
    if Temp <> 0 then
    begin
      Result := Result - 4;
      AWord := Temp;
    end;
    Temp := AWord shr 2;
    if Temp <> 0 then
    begin
      Result := Result - 2;
      AWord := Temp;
    end;
    Result := (Result - (AWord shr 1));
  end;
end;

function NumberOfTrailingZeroes(AWord: QWord): integer;
var
  Temp: QWord;
begin
  // from https://stackoverflow.com/questions/6506356/java-implementation-of-long-numberoftrailingzeros
  if AWord = 0 then
    Result := 64
  else
  begin
    Result := 63;
    Temp := AWord;
    Temp := AWord shl 32;
    if Temp <> 0 then
    begin
      Result := Result - 32;
      AWord := Temp;
    end;
    Temp := AWord shl 16;
    if Temp <> 0 then
    begin
      Result := Result - 16;
      AWord := Temp;
    end;
    Temp := AWord shl 8;
    if Temp <> 0 then
    begin
      Result := Result - 8;
      AWord := Temp;
    end;
    Temp := AWord shl 4;
    if Temp <> 0 then
    begin
      Result := Result - 4;
      AWord := Temp;
    end;
    Temp := AWord shl 2;
    if Temp <> 0 then
    begin
      Result := Result - 2;
      AWord := Temp;
    end;
    Result := (Result - ((AWord shl 1) shr 63));
  end;
end;


end.
