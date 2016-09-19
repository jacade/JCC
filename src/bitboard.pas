unit BitBoard;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils;

// We map a 8x8 board from the upper left corner (a8) to the lower right corner (h1)
// by going left to right and then up to down.
// So a8 would be 0 and h1 is 2^63

const
  // From 1 to 8
  Ranks: array[1..8] of QWord =
    (18374686479671623680, 71776119061217280, 280375465082880,
    1095216660480, 4278190080, 16711680, 65280, 255);

  // From A to H
  Files: array[1..8] of QWord =
    (72340172838076673, 144680345676153346, 289360691352306692, 578721382704613384,
    1157442765409226768, 2314885530818453536, 4629771061636907072, 9259542123273814144);

  // Diagonals from top left to bottom right
  Diagonals: array[1..15] of QWord =
    (1, 258, 66052, 16909320, 4328785936, 1108169199648,
    283691315109952, 72624976668147840, 145249953336295424,
    290499906672525312, 580999813328273408, 1161999622361579520,
    2323998145211531264, 4647714815446351872, 9223372036854775808);

  // Anti-Diagonals from top right to bottom left
  AntiDiagonals: array[1..15] of QWord =
    (128, 32832, 8405024, 2151686160, 550831656968,
    141012904183812, 36099303471055874, 9241421688590303745, 4620710844295151872,
    2310355422147575808, 1155177711073755136, 577588855528488960,
    288794425616760832, 144396663052566528, 72057594037927936);

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
      Result := Result + '1'
    else
      Result := Result + '0';
    ABitBoard := ABitBoard shr 1;
  end;
  for j := 8 downto 1 do
    Insert(LineEnding, Result, j * 8 + 1);
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
