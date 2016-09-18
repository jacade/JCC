unit BitBoard;

{$mode objfpc}{$H+}{$J-}

interface

uses
  Classes, SysUtils;

// We map a 8x8 board from the upper left corner (a8) to the lower right corner (h1)
// by going left to right and then up to down.
// So a8 would be 0 and h1 is 2^63

// Rows and Lines
//Rank1: QWord = 18374686479671623680;
//Rank2: QWord = 71776119061217280;
//Rank3: QWord = 280375465082880;
//Rank4: QWord = 1095216660480;
//Rank5: QWord = 4278190080;
//Rank6: QWord = 16711680;
//Rank7: QWord = 65280;
//Rank8: QWord = 255;

//  FileA: QWord = 72340172838076673;
//  FileB: QWord = 144680345676153346;
//  FileC: QWord = 289360691352306692;
//  FileD: QWord = 578721382704613384;
//  FileE: QWord = 1157442765409226768;
//  FileF: QWord = 2314885530818453536;
//  FileG: QWord = 4629771061636907072;
//  FileH: QWord = 9259542123273814144;

const
  Ranks: array[1..8] of QWord =
    (18374686479671623680, 71776119061217280, 280375465082880,
    1095216660480, 4278190080, 16711680, 65280, 255);
  // From A to H
  Files: array[1..8] of QWord =
    (72340172838076673, 144680345676153346, 289360691352306692, 578721382704613384,
    1157442765409226768, 2314885530818453536, 4629771061636907072, 9259542123273814144);

implementation

end.
