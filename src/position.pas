{ JCC (Jan's Chess Componenents) - This file contains classes to handle chess position
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

unit Position;

{$DEFINE LOGGING}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RegExpr, ArrayTools, MoveList, Pieces, StrTools, BitBoard
  {$IFDEF Logging} , EpikTimer {$ENDIF}  ;

{$INCLUDE ChessPieceLetters.inc}

type
  // Constants are taken from http://chessprogramming.wikispaces.com/10x12+Board
  // Main resource is https://de.wikipedia.org/wiki/Schachprogramm#12.C3.9710-Darstellung
  // Bitboard stuff is based on https://www.youtube.com/playlist?list=PLQV5mozTHmacMeRzJCW_8K3qw2miYqd0c

  TCastlingTypes = (ctWKingside, ctWQueenside, ctBKingside, ctBQueenside);

  TCastlingAbility = set of TCastlingTypes;

  // The following allows more control over the output of the function MoveToSAN
  // Based on https://en.wikipedia.org/wiki/Algebraic_notation_%28chess%29

  // Example: csNone: Be5, csColon: B:e5, csColonSuffix: Be5:, csx: Bxe5
  TCaptureSymbol = (csNone, csColon, csColonSuffix, csx);
  // Example: psNone: e8Q, psEqualSign: e8=Q, psBrackets: e8(Q), psSlash: e8/Q
  TPromotionSymbol = (psNone, psEqualSign, psBrackets, psSlash);

  EInvalidFEN = class(Exception);

  { TPosition }

  TPosition = class
  private
  protected
    FBlackWins: TNotifyEvent;
    FDraw: TNotifyEvent;
    FLegalMoves: TMoveList;
    FMoveNumber: integer;
    FWhitesTurn: boolean;
    FWhiteWins: TNotifyEvent;
  const
    DiagonalMoves = [9, 11];   // Too bad, that negative values are not allowed
    HorzVertMoves = [1, 10];
    KnightMoves = [8, 12, 19, 21];

    procedure BlackWins;
    procedure Draw;
    procedure GenerateLegalMoves; virtual; abstract;
    function GetCountOfFiles: byte; virtual; abstract;
    function GetCountOfRanks: byte; virtual; abstract;
    function GetSquares(Index: integer): TPieceType; virtual; abstract;
    procedure WhiteWins;
  public
    // Copies important values from Source to Self
    procedure Copy(Source: TPosition); virtual;
    function IsLegal(AMove: TMove): boolean; virtual;
    procedure PlayMove(AMove: TMove); virtual; abstract;
    procedure SetupInitialPosition; virtual; abstract;
  public
    property CountOfFiles: byte read GetCountOfFiles;
    property CountOfRanks: byte read GetCountOfRanks;
    property LegalMoves: TMoveList read FLegalMoves;
    property MoveNumber: integer read FMoveNumber write FMoveNumber;
    property OnBlackWins: TNotifyEvent read FBlackWins write FBlackWins;
    property OnDraw: TNotifyEvent read FDraw write FDraw;
    property OnWhiteWins: TNotifyEvent read FWhiteWins write FWhiteWins;
    property Squares[Index: integer]: TPieceType read GetSquares;
    property WhitesTurn: boolean read FWhitesTurn write FWhitesTurn;
  end;


  { TStandardPosition }

  TStandardPosition = class(TPosition)//(TPersistent)
  private
  var      // Note: If Variables are added, they need to be added to Assign, too
    FBlackKing: TSquare10x12;
    FCastlingAbility: TCastlingAbility;
    FOnChange: TNotifyEvent;
    FPliesSinceLastPawnMoveOrCapture: integer; // Important for 50 move rule
    // FSquares: array[0..119] of TPieceType;
    FWhiteKing: TSquare10x12;
    // BitBoards
    FEnPassant: TBitBoard;
    // 1. Pawns 2. Rooks 3. Knights 4. Bishops 5. Queens 6. Kings 7. White 8. Black
    FBitBoards: array[1..8] of TBitBoard;
    // 1. White 2. Black
    FAttackMaps: array[1..2] of TBitBoard;
    // Occupied Squares
    FOccupied: TBitBoard; // = FBitBoards[7] and FBitBoards[8]

    procedure Changed;
    function DiagonalAndAntiDiagonalBitboard(Index: integer): TBitBoard;
    //procedure GenerateAttackMaps;
    function HorizontalAndVerticalBitBoard(Index: integer): TBitBoard;
    // Checks if the side not to move is attacking the given square
    function IsAttacked(Index: integer): boolean;
    procedure SilentFromFEN(const AFEN: string);
    // Plays the move without triggering Changed
    procedure SilentPlayMove(AMove: TMove);
  protected
    procedure GenerateLegalMoves; override;
    function GetCountOfFiles: byte; override;
    function GetCountOfRanks: byte; override;
    function GetSquares(Index: integer): TPieceType; override;
  public
  const
    InitialFEN = 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1';

    // Just for debugging
    procedure PrintBoards;
    constructor Create;
    constructor Create(AFEN: string);
    procedure Copy(Source: TPosition); override;
    destructor Destroy; override;
    // Returns a sub list of LegalMoves with those moves which fulfill the parameters
    function FilterLegalMoves(APiece: TPieceType = ptEmpty;
      StartSquare: TSquare10x12 = 0; DestSquare: TSquare10x12 = 0;
      APromotionPiece: TPieceType = ptEmpty): TMoveList;
    procedure FromFEN(const AFEN: string);
    // Checks if the side to move is check
    function IsCheck: boolean;
    // Checks if the side not to move is in check
    function IsIllegalCheck: boolean;
    function IsMate: boolean;
    function IsStaleMate: boolean;
    function IsValid: boolean;
    function MoveFromSAN(ASAN: string): TMove;
    // This uses the english piece letters
    function MoveToSAN(AMove: TMove; ShowPawnLetter: boolean = False;
      ShowEnPassantSuffix: boolean = False; CaptureSymbol: TCaptureSymbol = csx;
      PromotionSymbol: TPromotionSymbol = psNone): string;
    function MoveToSAN(AMove: TMove; PieceLetters: TChessPieceLetters;
      ShowPawnLetter: boolean = False; ShowEnPassantSuffix: boolean = False;
      CaptureSymbol: TCaptureSymbol = csx;
      PromotionSymbol: TPromotionSymbol = psNone): string;
    procedure PlayMove(AMove: TMove); override;
    procedure SetupInitialPosition; override;
    function ToFEN: string;
  public
    property CastlingAbility: TCastlingAbility
      read FCastlingAbility write FCastlingAbility;
    property EnPassant: TBitBoard read FEnPassant write FEnPassant;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property PliesSinceLastPawnMoveOrCapture: integer
      read FPliesSinceLastPawnMoveOrCapture write FPliesSinceLastPawnMoveOrCapture;
  end;

    {$IFDEF Logging}
var
  Zuege: longword = 0;
  Zeit: extended = 0;
  ET: TEpikTimer;

    {$ENDIF}

// Returns a Bitboard with zeroes and a 1 at the given position
function SquareToBitBoard(const ASquare: TSquare10x12): QWord;

implementation

function SquareToBitBoard(const ASquare: TSquare10x12): QWord;
var
  Temp: TSquare8x8;
begin
  if ASquare in OffSquares then
    Result := 0
  else
  begin
    Temp := ASquare;
    // Result := QWord(1) shl (8 * (8 - ASquare.RRank) + ASquare.RFile - 1);
    Result := Ranks[Temp.RRank] and Files[Temp.RFile];
  end;
end;

{ TPosition }

procedure TPosition.BlackWins;
begin
  if Assigned(FBlackWins) then
    FBlackWins(Self);
end;

procedure TPosition.Draw;
begin
  if Assigned(FDraw) then
    FDraw(Self);
end;

procedure TPosition.WhiteWins;
begin
  if Assigned(FWhiteWins) then
    FWhiteWins(Self);
end;

procedure TPosition.Copy(Source: TPosition);
var
  Move: TMove;
begin
  FMoveNumber := Source.FMoveNumber;
  FWhitesTurn := Source.FWhitesTurn;
  FLegalMoves.Clear;
  for Move in Source.FLegalMoves do
    FLegalMoves.Add(Move);
end;

function TPosition.IsLegal(AMove: TMove): boolean;
begin
  Result := AMove in LegalMoves;
end;

{ TStandardPosition }

procedure TStandardPosition.Changed;
begin
  //GenerateAttackMaps;
  GenerateLegalMoves;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TStandardPosition.DiagonalAndAntiDiagonalBitboard(Index: integer): TBitBoard;
var
  Temp, Diag, AntiDiag, CurrentDiag, CurrentAntiDiag: TBitBoard;
begin
  Temp := QWord(1) shl Index;
  CurrentDiag := Diagonals[(Index div 8) + (Index mod 8) + 1];
  Diag := ((FOccupied and CurrentDiag) - 2 * Temp) xor
    ReverseBitBoard(ReverseBitBoard(FOccupied and CurrentDiag) -
    2 * ReverseBitBoard(Temp));
  CurrentAntiDiag := AntiDiagonals[(Index div 8) - (Index mod 8) + 8];
  AntiDiag := ((FOccupied and CurrentAntiDiag) - (2 * Temp)) xor
    ReverseBitBoard(ReverseBitBoard(FOccupied and CurrentAntiDiag) -
    (2 * ReverseBitBoard(Temp)));
  Result := (Diag and CurrentDiag) or (AntiDiag and CurrentAntiDiag);
end;

//procedure TStandardPosition.GenerateAttackMaps;
//var
//  i, j: integer;
//  Pawns, Rooks, Knights, Bishops, Kings, CurrentRook, CurrentBishop,
//  CurrentKnight, CurrentKing: TBitBoard;
//begin
//FOccupied := FBitBoards[7] or FBitBoards[8];
//for i := 1 to 2 do
//begin
//  FAttackMaps[i] := 0;
//  if i = 1 then  // get white pieces
//  begin
//    Pawns := FBitBoards[1] and FBitBoards[7];
//    Rooks := (FBitBoards[2] or FBitBoards[5]) and FBitBoards[7];
//    Knights := FBitBoards[3] and FBitBoards[7];
//    Bishops := (FBitBoards[4] or FBitBoards[5]) and FBitBoards[7];
//    Kings := FBitBoards[6] and FBitBoards[7];
//  end
//  else  // get black pieces
//  begin
//    Pawns := FBitBoards[1] and FBitBoards[8];
//    Rooks := (FBitBoards[2] or FBitBoards[5]) and FBitBoards[8];
//    Knights := FBitBoards[3] and FBitBoards[8];
//    Bishops := (FBitBoards[4] or FBitBoards[5]) and FBitBoards[8];
//    Kings := FBitBoards[6] and FBitBoards[8];
//  end;
//  // Rook attacks
//  CurrentRook := Rooks and not (Rooks - 1);
//  while CurrentRook > 0 do
//  begin
//    j := NumberOfTrailingZeroes(CurrentRook);
//    FAttackMaps[i] := FAttackMaps[i] or HorizontalAndVerticalBitBoard(j);
//    Rooks := Rooks and not CurrentRook;
//    CurrentRook := Rooks and not (Rooks - 1);
//  end;
//  // Bishop attacks
//  CurrentBishop := Bishops and not (Bishops - 1);
//  while CurrentBishop > 0 do
//  begin
//    j := NumberOfTrailingZeroes(CurrentBishop);
//    FAttackMaps[i] := FAttackMaps[i] or DiagonalAndAntiDiagonalBitboard(j);
//    Bishops := Bishops and not CurrentBishop;
//    CurrentBishop := Bishops and not (Bishops - 1);
//  end;
//  // Knight attacks
//  CurrentKnight := Knights and not (Knights - 1);
//  while CurrentKnight > 0 do
//  begin
//    j := NumberOfTrailingZeroes(CurrentKnight);
//    FAttackMaps[i] := FAttackMaps[i] or BitBoard.KnightMoves[j];
//    Knights := Knights and not CurrentKnight;
//    CurrentKnight := Knights and not (Knights - 1);
//  end;
//  // King attacks
//  CurrentKing := Kings and not (Kings - 1);
//  while CurrentKing > 0 do
//  begin
//    j := NumberOfTrailingZeroes(CurrentKing);
//    FAttackMaps[i] := FAttackMaps[i] or KingMoves[j];
//    Kings := Kings and not CurrentKing;
//    CurrentKing := Kings and not (Kings - 1);
//  end;
//  // Pawn attacks
//  if i = 1 then
//  begin
//    // White pawn captures to the right
//    FAttackMaps[i] := FAttackMaps[i] or ((Pawns and not Files[8]) shr 7);
//    // White pawn captures to the left
//    FAttackMaps[i] := FAttackMaps[i] or ((Pawns and not Files[1]) shr 9);
//  end
//  else
//  begin
//    // Black pawn captures to the right
//    FAttackMaps[i] := FAttackMaps[i] or ((Pawns and not Files[8]) shl 9);
//    // Black pawn captures to the left
//    FAttackMaps[i] := FAttackMaps[i] or ((Pawns and not Files[1]) shl 7);
//  end;
//end;
//WriteLn(BitBoardToStr(FAttackMaps[1]), '  Weiß');
//WriteLn(BitBoardToStr(FAttackMaps[2]), '  Schwarz');
//end;

function TStandardPosition.HorizontalAndVerticalBitBoard(Index: integer): TBitBoard;
var
  Temp, Horz, Vert, CurrentFile: TBitBoard;
begin
  Temp := QWord(1) shl Index;
  Horz := (FOccupied - 2 * Temp) xor ReverseBitBoard(ReverseBitBoard(FOccupied) -
    2 * ReverseBitBoard(Temp));
  CurrentFile := Files[(Index mod 8) + 1];
  Vert := ((FOccupied and CurrentFile) - (2 * Temp)) xor
    ReverseBitBoard(ReverseBitBoard(FOccupied and CurrentFile) -
    (2 * ReverseBitBoard(Temp)));
  Result := (Horz and Ranks[8 - (Index div 8)]) or (Vert and CurrentFile);
end;

procedure TStandardPosition.Copy(Source: TPosition);
var
  i: integer;
begin
  inherited Copy(Source);
  if Source is TStandardPosition then
  begin
    FCastlingAbility := TStandardPosition(Source).FCastlingAbility;
    FEnPassant := TStandardPosition(Source).FEnPassant;
    FPliesSinceLastPawnMoveOrCapture :=
      TStandardPosition(Source).FPliesSinceLastPawnMoveOrCapture;
    FBlackKing := TStandardPosition(Source).FBlackKing;
    FWhiteKing := TStandardPosition(Source).FWhiteKing;
    for i := 1 to 8 do
      FBitBoards[i] := TStandardPosition(Source).FBitBoards[i];
  end;
end;

procedure TStandardPosition.GenerateLegalMoves;
var
  // Useful variables
  BlackPiecesWithoutKing: TBitBoard;
  WhitePiecesWithoutKing: TBitBoard;
  Empty: TBitBoard;
  WP, WR, WN, WB, WQ, WK: TBitBoard;
  BP, BR, BN, BB, BQ, BK: TBitBoard;
  OppositeColorWithoutKingOrEmpty: TBitBoard;

  procedure AddBitBoardToMoveList(PossibleMoves: TBitBoard; Start: integer;
    RelativeStart: integer = 0);
  var
    i: integer;
    CurrentMove: TBitBoard;
  begin
    CurrentMove := PossibleMoves and not (PossibleMoves - 1);
    while CurrentMove > 0 do
    begin
      i := NumberOfTrailingZeroes(CurrentMove);
      if Start < 0 then // This should be used for pawn moves
      begin
        FLegalMoves.Add(CreateMoveFromInt(i + RelativeStart, i));
      end
      else  // This is for the other pieces
      begin
        FLegalMoves.Add(CreateMoveFromInt(Start, i));
      end;
      PossibleMoves := PossibleMoves and not CurrentMove;
      CurrentMove := PossibleMoves and not (PossibleMoves - 1);
    end;
  end;

  procedure GenerateBishopMoves(Bishops: TBitBoard);
  var
    CurrentBishop, i, Moves: TBitBoard;
  begin
    CurrentBishop := Bishops and not (Bishops - 1);
    while CurrentBishop > 0 do
    begin
      i := NumberOfTrailingZeroes(CurrentBishop);
      Moves := DiagonalAndAntiDiagonalBitboard(i) and OppositeColorWithoutKingOrEmpty;
      AddBitBoardToMoveList(Moves, i);
      Bishops := Bishops and not CurrentBishop;
      CurrentBishop := Bishops and not (Bishops - 1);
    end;
  end;

  procedure GenerateCastlingMoves;
  begin
    // Check Castlings
    if FWhitesTurn then
    begin
      if (ctWKingside in FCastlingAbility) and ((WK and not FAttackMaps[2]) = WK) and
        ((CastlingSquares[1] and Empty) = CastlingSquares[1]) and
        ((CastlingSquares[1] and not FAttackMaps[2]) = CastlingSquares[1]) then
        FLegalMoves.Add(CreateMoveFromInt(60, 62));
      if (ctWQueenside in FCastlingAbility) and ((WK and not FAttackMaps[2]) = WK) and
        ((CastlingSquares[2] and Empty) = CastlingSquares[2]) and
        ((CastlingSquares[2] and not FAttackMaps[2]) = CastlingSquares[2]) then
        FLegalMoves.Add(CreateMoveFromInt(60, 58));
    end
    else
    begin
      if (ctBKingside in FCastlingAbility) and ((BK and not FAttackMaps[1]) = BK) and
        ((CastlingSquares[3] and Empty) = CastlingSquares[3]) and
        ((CastlingSquares[3] and not FAttackMaps[1]) = CastlingSquares[3]) then
        FLegalMoves.Add(CreateMoveFromInt(4, 6));
      if (ctBQueenside in FCastlingAbility) and ((BK and not FAttackMaps[1]) = BK) and
        ((CastlingSquares[4] and Empty) = CastlingSquares[4]) and
        ((CastlingSquares[4] and not FAttackMaps[1]) = CastlingSquares[4]) then
        FLegalMoves.Add(CreateMoveFromInt(4, 2));
    end;
  end;

  procedure GenerateKingMoves(Kings: TBitBoard);
  var
    CurrentKing, i, Moves: TBitBoard;
  begin
    CurrentKing := Kings and not (Kings - 1);
    while CurrentKing > 0 do
    begin
      i := NumberOfTrailingZeroes(CurrentKing);
      Moves := KingMoves[i] and OppositeColorWithoutKingOrEmpty;
      AddBitBoardToMoveList(Moves, i);
      Kings := Kings and not CurrentKing;
      CurrentKing := Kings and not (Kings - 1);
    end;
  end;

  procedure GenerateKnightMoves(Knights: TBitBoard);
  var
    CurrentKnight, i, Moves: TBitBoard;
  begin
    CurrentKnight := Knights and not (Knights - 1);
    while CurrentKnight > 0 do
    begin
      i := NumberOfTrailingZeroes(CurrentKnight);
      Moves := BitBoard.KnightMoves[i] and OppositeColorWithoutKingOrEmpty;
      AddBitBoardToMoveList(Moves, i);
      Knights := Knights and not CurrentKnight;
      CurrentKnight := Knights and not (Knights - 1);
    end;
  end;

  procedure GeneratePawnPromotionMoves(AMoveList: TMoveList);
  var
    temp: TMoveList;
    Piece: TBasicPieceType;
    i: integer;
    Start, Dest: TSquare10x12;
  begin
    temp := TMoveList.Create;
    i := 0;
    while i < AMoveList.Count do
    begin
      Start := AMoveList.Items[i].Start;
      Dest := AMoveList.Items[i].Dest;
      if (FWhitesTurn and (Start in Rank7) and (Squares[Start] = ptWPawn)) or
        (not FWhitesTurn and (Start in Rank2) and (Squares[Start] = ptBPawn)) then
      begin
        for Piece in [bptRook, bptKnight, bptBishop, bptQueen] do
          temp.Add(CreateMove(Start, Dest, PieceType(Piece, FWhitesTurn)));
        AMoveList.Delete(i);
      end
      else
        Inc(i);
    end;
    AMoveList.AddList(temp);
    FreeAndNil(temp);
  end;

  procedure GeneratePawnCaptureMoves;
  var
    PawnMoves: QWord;
  begin
    if FWhitesTurn then
    begin
      // White pawn captures to the right
      PawnMoves := ((WP and not Files[8]) shr 7) and
        (BlackPiecesWithoutKing or FEnPassant);
      AddBitBoardToMoveList(PawnMoves, -1, 7);
      // White pawn captures to the left
      PawnMoves := ((WP and not Files[1]) shr 9) and
        (BlackPiecesWithoutKing or FEnPassant);
      AddBitBoardToMoveList(PawnMoves, -1, 9);
    end
    else
    begin
      // Black pawn captures to the right
      PawnMoves := ((BP and not Files[8]) shl 9) and
        (WhitePiecesWithoutKing or FEnPassant);
      AddBitBoardToMoveList(PawnMoves, -1, -9);
      // Black pawn captures to the left
      PawnMoves := ((BP and not Files[1]) shl 7) and
        (WhitePiecesWithoutKing or FEnPassant);
      AddBitBoardToMoveList(PawnMoves, -1, -7);
    end;
  end;

  procedure GeneratePawnForwardMoves;
  var
    PawnMoves: TBitBoard;
  begin
    if FWhitesTurn then
    begin
      // White pawn goes one forward
      PawnMoves := (WP shr 8) and Empty;
      AddBitBoardToMoveList(PawnMoves, -1, 8);
      // White pawn goes two forward
      PawnMoves := ((WP and Ranks[2]) shr 16) and Empty and (Empty shr 8);
      AddBitBoardToMoveList(PawnMoves, -1, 16);
    end
    else
    begin
      // Black pawn goes one forward
      PawnMoves := (BP shl 8) and Empty;
      AddBitBoardToMoveList(PawnMoves, -1, -8);
      // Black pawn goes two forward
      PawnMoves := ((BP and Ranks[7]) shl 16) and Empty and (Empty shl 8);
      AddBitBoardToMoveList(PawnMoves, -1, -16);
    end;
  end;

  procedure GenerateRookMoves(Rooks: TBitBoard);
  var
    CurrentRook, i, Moves: TBitBoard;
  begin
    CurrentRook := Rooks and not (Rooks - 1);
    while CurrentRook > 0 do
    begin
      i := NumberOfTrailingZeroes(CurrentRook);
      Moves := HorizontalAndVerticalBitBoard(i) and OppositeColorWithoutKingOrEmpty;
      AddBitBoardToMoveList(Moves, i);
      Rooks := Rooks and not CurrentRook;
      CurrentRook := Rooks and not (Rooks - 1);
    end;
  end;

var
  i: byte;
  j: integer;
  BCastlingAbility: TCastlingAbility;
  BEnPassant: TBitBoard;
  BPliesSinceLastPawnMoveOrCapture: integer;
  BMoveNumer: integer;
  BBlackKing, BWhiteKing: TSquare10x12;
  BackupBoards: array[1..8] of TBitBoard;
  b, c, tb, tc: extended;
  {$IFDEF Logging}
  a: extended;
  {$ENDIF}
begin
  // Initiliaze variables
  WP := FBitBoards[1] and FBitBoards[7];
  WR := FBitBoards[2] and FBitBoards[7];
  WN := FBitBoards[3] and FBitBoards[7];
  WB := FBitBoards[4] and FBitBoards[7];
  WQ := FBitBoards[5] and FBitBoards[7];
  WK := FBitBoards[6] and FBitBoards[7];
  BP := FBitBoards[1] and FBitBoards[8];
  BR := FBitBoards[2] and FBitBoards[8];
  BN := FBitBoards[3] and FBitBoards[8];
  BB := FBitBoards[4] and FBitBoards[8];
  BQ := FBitBoards[5] and FBitBoards[8];
  BK := FBitBoards[6] and FBitBoards[8];
  BlackPiecesWithoutKing := FBitBoards[8] and not BK;
  WhitePiecesWithoutKing := FBitBoards[7] and not WK;
  FOccupied := FBitBoards[7] or FBitBoards[8];
  Empty := not FOccupied;
  //a := ET.Elapsed;
  tb := 0;
  tc := 0;
  // The following takes up to 1 ms, could this be made faster?
  {$IFDEF Logging}
  ET.Start;
  a := ET.Elapsed;
{$ENDIF}
  FLegalMoves.Clear;
  GeneratePawnCaptureMoves;
  GeneratePawnForwardMoves;
  GenerateCastlingMoves;
  if WhitesTurn then
  begin
    OppositeColorWithoutKingOrEmpty := BlackPiecesWithoutKing or Empty;
    GenerateBishopMoves(WB or WQ);
    GenerateRookMoves(WR or WQ);
    GenerateKnightMoves(WN);
    GenerateKingMoves(WK);
  end
  else
  begin
    OppositeColorWithoutKingOrEmpty := WhitePiecesWithoutKing or Empty;
    GenerateBishopMoves(BB or BQ);
    GenerateRookMoves(BR or BQ);
    GenerateKnightMoves(BN);
    GenerateKingMoves(BK);
  end;
  // Backup Position, Play Move, Position Valid?
  j := 0;
  // Backup current Position
  BEnPassant := FEnPassant;
  BPliesSinceLastPawnMoveOrCapture := FPliesSinceLastPawnMoveOrCapture;
  BCastlingAbility := FCastlingAbility;
  BMoveNumer := FMoveNumber;
  BBlackKing := FBlackKing;
  BWhiteKing := FWhiteKing;
  for i := 1 to 8 do
    BackupBoards[i] := FBitBoards[i];
  while j < FLegalMoves.Count do
  begin
    {$IFDEF Logging}
    b := ET.Elapsed;
    {$ENDIF}
    Self.SilentPlayMove(FLegalMoves.Items[j]);
    {$IFDEF Logging}
    tb := tb + ET.Elapsed - b;
    {$ENDIF}
    {$IFDEF Logging}
    c := ET.Elapsed;
    {$ENDIF}
    if not Self.IsIllegalCheck then
      Inc(j)
    else
      FLegalMoves.Delete(j);
    {$IFDEF Logging}
    tc := tc + ET.Elapsed - c;
    {$ENDIF}
    // Restore inital values
    FEnPassant := BEnPassant;
    FPliesSinceLastPawnMoveOrCapture := BPliesSinceLastPawnMoveOrCapture;
    FCastlingAbility := BCastlingAbility;
    FMoveNumber := BMoveNumer;
    FBlackKing := BBlackKing;
    FWhiteKing := BWhiteKing;
    FWhitesTurn := not FWhitesTurn;
    for i := 1 to 8 do
      FBitBoards[i] := BackupBoards[i];
  end;
  //Write(' 1: ', FormatFloat('0.##', (tb) * 1000000), 'µs');
  //Write('  2: ', FormatFloat('0.##', (tc) * 1000000), 'µs');
  //Writeln('  Total: ', FormatFloat('0.##', (ET.Elapsed - a) * 1000000), 'µs');

  // TODO: Replace pawn moves with actual promotion moves
  GeneratePawnPromotionMoves(FLegalMoves);
  {$IFDEF Logging}
  Inc(Zuege, FLegalMoves.Count);
  Zeit := Zeit + (ET.Elapsed - a);
  ET.Stop;
  //  WriteLn('Zahl der möglichen Züge: ', FLegalMoves.Count);
{$ENDIF}
end;

function TStandardPosition.GetCountOfFiles: byte;
begin
  Result := 8;
end;

function TStandardPosition.GetCountOfRanks: byte;
begin
  Result := 8;
end;

function TStandardPosition.GetSquares(Index: integer): TPieceType;
var
  i, j: integer;
begin
  // Result := FSquares[Index];
  Result := ptEmpty;
  for i := 1 to 6 do
  begin
    for j := 7 to 8 do
    begin
      if (SquareToBitBoard(Index) and FBitBoards[i] and FBitBoards[j]) > 0 then
        case i of
          1: Result := PieceType(bptPawn, j = 7);
          2: Result := PieceType(bptRook, j = 7);
          3: Result := PieceType(bptKnight, j = 7);
          4: Result := PieceType(bptBishop, j = 7);
          5: Result := PieceType(bptQueen, j = 7);
          6: Result := PieceType(bptKing, j = 7);
        end;
    end;
  end;
end;

function TStandardPosition.IsAttacked(Index: integer): boolean;
var
  Opp: integer;
  Moves, CurrentSquare: TBitBoard;
begin
  if FWhitesTurn then
  begin
    Opp := 8;
  end
  else
  begin
    Opp := 7;
  end;
  Result := False;
  FOccupied := FBitBoards[7] or FBitBoards[8];
  // Do rook moves from start
  Moves := HorizontalAndVerticalBitBoard(Index) and FOccupied;
  CurrentSquare := Moves and not (Moves - 1);
  while (CurrentSquare > 0) and not Result do
  begin
    Result := (FBitBoards[2] or FBitBoards[5]) and FBitBoards[Opp] and CurrentSquare > 0;
    Moves := Moves and not CurrentSquare;
    CurrentSquare := Moves and not (Moves - 1);
  end;
  // Do bishop moves from start
  Moves := DiagonalAndAntiDiagonalBitboard(Index) and FOccupied;
  CurrentSquare := Moves and not (Moves - 1);
  while (CurrentSquare > 0) and not Result do
  begin
    Result := (FBitBoards[4] or FBitBoards[5]) and FBitBoards[Opp] and CurrentSquare > 0;
    Moves := Moves and not CurrentSquare;
    CurrentSquare := Moves and not (Moves - 1);
  end;
  // Do knight moves from start
  Moves := BitBoard.KnightMoves[Index] and FOccupied;
  CurrentSquare := Moves and not (Moves - 1);
  while (CurrentSquare > 0) and not Result do
  begin
    Result := FBitBoards[3] and FBitBoards[Opp] and CurrentSquare > 0;
    Moves := Moves and not CurrentSquare;
    CurrentSquare := Moves and not (Moves - 1);
  end;
  // Do king moves from start
  Moves := KingMoves[Index] and FOccupied;
  CurrentSquare := Moves and not (Moves - 1);
  while (CurrentSquare > 0) and not Result do
  begin
    Result := FBitBoards[6] and FBitBoards[Opp] and CurrentSquare > 0;
    Moves := Moves and not CurrentSquare;
    CurrentSquare := Moves and not (Moves - 1);
  end;
  // Do opposite pawn Moves
  if not Result then
    if not FWhitesTurn then
    begin
      Moves := (((FBitBoards[1] and FBitBoards[7] and not Files[8]) shr 7)) or
        (((FBitBoards[1] and FBitBoards[7] and not Files[1]) shr 9));
      Result := (QWord(1) shl Index) and Moves > 0;
    end
    else
    begin
      Moves := (((FBitBoards[1] and FBitBoards[8] and not Files[8]) shl 9)) or
        (((FBitBoards[1] and FBitBoards[8] and not Files[1]) shl 7));
      Result := (QWord(1) shl Index) and Moves > 0;
    end;
end;

procedure TStandardPosition.SilentFromFEN(const AFEN: string);
var
  c: char;
  s, p: TStringList;
  rk, fl, i, Coordinate: TSquare10x12;
  temp: string;
  RegFEN: TRegExpr;
begin
  RegFEN := TRegExpr.Create;
  RegFEN.Expression := '(([prnbqkPRNBQK1-8]){1,8}\/){7}([prnbqkPRNBQK1-8]){1,8} ' +
    '(w|b) (KQ?k?q?|Qk?q?|kq?|q|-) (-|([a-h][36])) (0|[1-9][0-9]*) [1-9][0-9]*';
  if not RegFEN.Exec(AFEN) then
    raise EInvalidFEN.Create('FEN is invalid');
  FreeAndNil(RegFEN);
  s := Split(AFEN, ' ');
  // Put Pieces on board
  p := Split(s.Strings[0], '/');
  for rk := 0 to 7 do
  begin
    temp := p.Strings[rk];
    fl := 1;
    for i := 1 to Length(temp) do
    begin
      Coordinate := rk * 10 + 20 + fl;
      // TODO:
      //if FSquares[Coordinate] = ptOff then
      //  raise EInvalidFEN.Create('FEN is invalid');
      case temp[i] of
        '1'..'8': Inc(fl, StrToInt(temp[i]) - 1);
        'p':
        begin
          FBitBoards[1] := FBitBoards[1] or SquareToBitBoard(Coordinate);
          FBitBoards[8] := FBitBoards[8] or SquareToBitBoard(Coordinate);
        end;
        'r':
        begin
          FBitBoards[2] := FBitBoards[2] or SquareToBitBoard(Coordinate);
          FBitBoards[8] := FBitBoards[8] or SquareToBitBoard(Coordinate);
        end;
        'n':
        begin
          FBitBoards[3] := FBitBoards[3] or SquareToBitBoard(Coordinate);
          FBitBoards[8] := FBitBoards[8] or SquareToBitBoard(Coordinate);
        end;
        'b':
        begin
          FBitBoards[4] := FBitBoards[4] or SquareToBitBoard(Coordinate);
          FBitBoards[8] := FBitBoards[8] or SquareToBitBoard(Coordinate);
        end;
        'q':
        begin
          FBitBoards[5] := FBitBoards[5] or SquareToBitBoard(Coordinate);
          FBitBoards[8] := FBitBoards[8] or SquareToBitBoard(Coordinate);
        end;
        'k':
        begin
          FBitBoards[6] := FBitBoards[6] or SquareToBitBoard(Coordinate);
          FBitBoards[8] := FBitBoards[8] or SquareToBitBoard(Coordinate);
          FBlackKing := Coordinate;
        end;
        'P':
        begin
          FBitBoards[1] := FBitBoards[1] or SquareToBitBoard(Coordinate);
          FBitBoards[7] := FBitBoards[7] or SquareToBitBoard(Coordinate);
        end;
        'R':
        begin
          FBitBoards[2] := FBitBoards[2] or SquareToBitBoard(Coordinate);
          FBitBoards[7] := FBitBoards[7] or SquareToBitBoard(Coordinate);
        end;
        'N':
        begin
          FBitBoards[3] := FBitBoards[3] or SquareToBitBoard(Coordinate);
          FBitBoards[7] := FBitBoards[7] or SquareToBitBoard(Coordinate);
        end;
        'B':
        begin
          FBitBoards[4] := FBitBoards[4] or SquareToBitBoard(Coordinate);
          FBitBoards[7] := FBitBoards[7] or SquareToBitBoard(Coordinate);
        end;
        'Q':
        begin
          FBitBoards[5] := FBitBoards[5] or SquareToBitBoard(Coordinate);
          FBitBoards[7] := FBitBoards[7] or SquareToBitBoard(Coordinate);
        end;
        'K':
        begin
          FBitBoards[6] := FBitBoards[6] or SquareToBitBoard(Coordinate);
          FBitBoards[7] := FBitBoards[7] or SquareToBitBoard(Coordinate);
          FWhiteKing := Coordinate;
        end;
      end;
      Inc(fl);
    end;
  end;
  {$IFDEF Logging}
  for i := 1 to 8 do
    WriteLn(BitBoardToStr(FBitBoards[i]));
  {$ENDIF}
  FreeAndNil(p);
  // Determine who's to play
  FWhitesTurn := s.Strings[1] = 'w';
  // Determine allowed Castlings
  FCastlingAbility := [];
  for c in s.Strings[2] do
    case c of
      'K': FCastlingAbility := FCastlingAbility + [ctWKingside];
      'Q': FCastlingAbility := FCastlingAbility + [ctWQueenside];
      'k': FCastlingAbility := FCastlingAbility + [ctBKingside];
      'q': FCastlingAbility := FCastlingAbility + [ctBQueenside];
    end;
  // Is en passant possible?
  if s.Strings[3] = '-' then
    FEnPassant := 0
  else
    FEnPassant := SquareToBitBoard(AlgebraicSquare(s.Strings[3][1], s.Strings[3][2]));
  // Get plies
  FPliesSinceLastPawnMoveOrCapture := StrToInt(s.Strings[4]);
  // Get start move number
  FMoveNumber := StrToInt(s.Strings[5]);
  FreeAndNil(s);
end;

procedure TStandardPosition.SilentPlayMove(AMove: TMove);
var
  Start, Dest: TBitBoard;
  i: integer;
begin
  Start := SquareToBitBoard(AMove.Start);
  Dest := SquareToBitBoard(AMove.Dest);
  if (Start and (FBitBoards[7] or FBitBoards[8])) = 0 then
  begin
    PrintBoards;
    raise Exception.Create('Invalid Start square given!');
  end;
  // Delete Dest from all bitboards
  for i := 1 to 6 do
    FBitBoards[i] := FBitBoards[i] and not Dest;
  // Delete Dest and add Start board to given color board
  // and remove dest from opposite
  if FWhitesTurn then
  begin
    FBitBoards[7] := (FBitBoards[7] and not Start) or Dest;
    FBitBoards[8] := FBitBoards[8] and not Dest;
  end
  else
  begin
    FBitBoards[8] := (FBitBoards[8] and not Start) or Dest;
    FBitBoards[7] := FBitBoards[7] and not Dest;
  end;

  if ((Start and FBitBoards[1]) > 0) or ((Dest and FOccupied) > 0) then
    FPliesSinceLastPawnMoveOrCapture := 0
  else
    Inc(FPliesSinceLastPawnMoveOrCapture);

  if (Start and (FBitBoards[2] or FBitBoards[3] or FBitBoards[4] or
    FBitBoards[5])) > 0 then
  begin
    for i := 2 to 5 do
    begin
      if (Start and FBitBoards[i]) > 0 then
      begin
        FBitBoards[i] := (FBitBoards[i] and not Start) or Dest;
      end;
    end;
    FEnPassant := 0;
  end
  else
  if (Start and FBitBoards[1]) > 0 then
  begin
    FBitBoards[1] := (FBitBoards[1] and not Start) or Dest;
    // Promotion
    if (Dest and Ranks[1]) or (Dest and Ranks[8]) > 0 then
    begin
      case AMove.PromotionPiece of
        ptWRook, ptBRook: FBitBoards[2] := FBitBoards[2] or Dest;
        ptWKnight, ptBKnight: FBitBoards[3] := FBitBoards[3] or Dest;
        ptWBishop, ptBBishop: FBitBoards[4] := FBitBoards[4] or Dest;
        ptWQueen, ptBQueen: FBitBoards[5] := FBitBoards[5] or Dest;
      end;
    end
    else
    // En Passant
    if Dest = EnPassant then
    begin
      if (Dest and Ranks[3]) > 0 then
      begin
        for i := 1 to 8 do
          FBitBoards[i] := FBitBoards[i] and not (Dest shr 8);
      end
      else
        for i := 1 to 8 do
          FBitBoards[i] := FBitBoards[i] and not (Dest shl 8);
    end
    else
      // Normal move
    begin
      // Set FEnPassant accordingly
      if ((Dest > Start) and (Dest = Start shl 16)) then
        FEnPassant := Start shl 8
      else
      if ((Start > Dest) and (Start = Dest shl 16)) then
        FEnPassant := Start shr 8
      else
        FEnPassant := 0;
    end;
  end
  else
  begin
    // King moves
    FBitBoards[6] := (FBitBoards[6] and not Start) or Dest;
    FEnPassant := 0;
    // Kingside castling
    if ((Dest > Start) and (Dest = Start shl 2)) then
    begin
      FBitBoards[2] := (FBitBoards[2] or (Start shl 1)) and not (Start shl 3);
      if FWhitesTurn then
        FBitBoards[7] := (FBitBoards[7] or (Start shl 1)) and not (Start shl 3)
      else
        FBitBoards[8] := (FBitBoards[8] or (Start shl 1)) and not (Start shl 3);
    end
    else
    // Queenside castling
    if ((Dest < Start) and (Start = Dest shl 2)) then
    begin
      FBitBoards[2] := (FBitBoards[2] or (Start shr 1)) and not (Start shr 4);
      if FWhitesTurn then
        FBitBoards[7] := (FBitBoards[7] or (Start shr 1)) and not (Start shr 4)
      else
        FBitBoards[8] := (FBitBoards[8] or (Start shr 1)) and not (Start shr 4);
    end;
    if WhitesTurn then
      FWhiteKing := AMove.Dest
    else
      FBlackKing := Amove.Dest;
  end;
  // Set Castling abilities accordingly
  if (Start and Ranks[1] and (Files[1] or Files[5])) > 0 then
    FCastlingAbility := FCastlingAbility - [ctWQueenside];
  if (Start and Ranks[1] and (Files[8] or Files[5])) > 0 then
    FCastlingAbility := FCastlingAbility - [ctWKingside];
  if (Start and Ranks[8] and (Files[1] or Files[5])) > 0 then
    FCastlingAbility := FCastlingAbility - [ctBQueenside];
  if (Start and Ranks[8] and (Files[8] or Files[5])) > 0 then
    FCastlingAbility := FCastlingAbility - [ctBKingside];
  if not FWhitesTurn then
    Inc(FMoveNumber);
  FWhitesTurn := not FWhitesTurn;
  // GenerateAttackMaps;
end;

procedure TStandardPosition.PrintBoards;
var
  i: integer;
begin
  for i := 1 to 8 do
    Writeln(BitBoardToStr(FBitBoards[i]), i);
  WriteLn(BitBoardToStr(FEnPassant), 'En Passant');
end;

constructor TStandardPosition.Create;
begin
  FLegalMoves := TMoveList.Create;
end;

constructor TStandardPosition.Create(AFEN: string);
begin
  Create;
  FromFEN(AFEN);
end;

destructor TStandardPosition.Destroy;
begin
  FreeAndNil(FLegalMoves);
  inherited Destroy;
end;

function TStandardPosition.FilterLegalMoves(APiece: TPieceType;
  StartSquare: TSquare10x12; DestSquare: TSquare10x12;
  APromotionPiece: TPieceType): TMoveList;
var
  NoFilterPiece, NoFilterStart, NoFilterDest, NoFilterPromo: boolean;
  Move: TMove;
begin
  NoFilterPiece := APiece = ptEmpty;
  NoFilterStart := StartSquare = 0;
  NoFilterDest := DestSquare = 0;
  NoFilterPromo := APromotionPiece = ptEmpty;
  Result := TMoveList.Create;
  for Move in FLegalMoves do
  begin
    if (NoFilterPiece or (Squares[TSquare10x12(Move.Start)] = APiece)) and
      (NoFilterStart or (Move.Start = StartSquare)) and
      (NoFilterDest or (Move.Dest = DestSquare)) and
      (NoFilterPromo or (Move.PromotionPiece = APromotionPiece)) then
      Result.Add(Move);
  end;
end;

procedure TStandardPosition.FromFEN(const AFEN: string);
begin
  SilentFromFEN(AFEN);
  Changed;
end;

function TStandardPosition.IsCheck: boolean;
begin
  // Assumes that exact one black and one white king exist
  if FWhitesTurn then
    Result := IsAttacked(NumberOfTrailingZeroes(FBitBoards[6] and FBitBoards[7]))
  else
    Result := IsAttacked(NumberOfTrailingZeroes(FBitBoards[6] and FBitBoards[8]));
end;

function TStandardPosition.IsIllegalCheck: boolean;
begin
  FWhitesTurn := not FWhitesTurn;
  Result := IsCheck;
  FWhitesTurn := not FWhitesTurn;
end;

function TStandardPosition.IsMate: boolean;
begin
  Result := (FLegalMoves.Count = 0) and IsCheck;
end;

function TStandardPosition.IsStaleMate: boolean;
begin
  Result := (FLegalMoves.Count = 0) and not IsCheck;
end;

function TStandardPosition.IsValid: boolean;
var
  // Count white and black pieces in order p, r, n, b, q, k
  WhitePieces, BlackPieces: array[1..6] of integer;
  i: integer;
begin
  Result := True;
  for i := 1 to 6 do
  begin
    WhitePieces[i] := 0;
    BlackPieces[i] := 0;
  end;
  // TODO Check correct count of pieces, i. e. not more than 32
  Result := Result and (WhitePieces[6] = 1) and (BlackPieces[6] = 1) and
    (WhitePieces[1] <= 8) and (BlackPieces[1] <= 8) and
    (SumOf(WhitePieces) <= 16) and (SumOf(BlackPieces) <= 16);
  // TODO: Check if CastlingAbilities are set correct
  //if ctWKingside in FCastlingAbility then
  //  Result := Result and (FSquares[95] = ptWKing) and (FSquares[98] = ptWRook);
  //if ctWQueenside in FCastlingAbility then
  //  Result := Result and (FSquares[95] = ptWKing) and (FSquares[91] = ptWRook);
  //if ctBKingside in FCastlingAbility then
  //  Result := Result and (FSquares[25] = ptBKing) and (FSquares[28] = ptBRook);
  //if ctBQueenside in FCastlingAbility then
  //  Result := Result and (FSquares[25] = ptBKing) and (FSquares[21] = ptBRook);
  // Check if a king is in illegal check
  Result := Result and not IsIllegalCheck;
end;

function TStandardPosition.MoveFromSAN(ASAN: string): TMove;
var
  i: integer;
  NotValid: boolean;
begin
  NotValid := True;
  // This is one way, but SAN is not unique
  for i := 0 to FLegalMoves.Count - 1 do
  begin
    if MoveToSAN(FLegalMoves.Items[i]) = ASAN then
    begin
      Result := FLegalMoves.Items[i];
      NotValid := False;
    end;
  end;
  if NotValid then
    raise Exception.Create(ASAN + ' is no valid move.');
end;

function TStandardPosition.MoveToSAN(AMove: TMove; ShowPawnLetter: boolean;
  ShowEnPassantSuffix: boolean; CaptureSymbol: TCaptureSymbol;
  PromotionSymbol: TPromotionSymbol): string;
begin
  Result := MoveToSAN(AMove, PieceLetters_EN, ShowPawnLetter,
    ShowEnPassantSuffix, CaptureSymbol, PromotionSymbol);
end;

function TStandardPosition.MoveToSAN(AMove: TMove; PieceLetters: TChessPieceLetters;
  ShowPawnLetter: boolean; ShowEnPassantSuffix: boolean;
  CaptureSymbol: TCaptureSymbol; PromotionSymbol: TPromotionSymbol): string;

  function PieceToStr(Piece: TPieceType): string;
  begin
    case Piece of
      ptWPawn:
      begin
        if ShowPawnLetter then
          Result := PieceLetters[1]
        else
          Result := '';
      end;
      ptWKnight: Result := PieceLetters[2];
      ptWBishop: Result := PieceLetters[3];
      ptWRook: Result := PieceLetters[4];
      ptWQueen: Result := PieceLetters[5];
      ptWKing: Result := PieceLetters[6];
      ptBPawn:
      begin
        if ShowPawnLetter then
          Result := PieceLetters[7]
        else
          Result := '';
      end;
      ptBKnight: Result := PieceLetters[8];
      ptBBishop: Result := PieceLetters[9];
      ptBRook: Result := PieceLetters[10];
      ptBQueen: Result := PieceLetters[11];
      ptBKing: Result := PieceLetters[12];
    end;

  end;

var
  SameDest: TMoveList;
  Piece: TPieceType;
  j: integer;
  Distinguished: boolean;
  Clone: TStandardPosition;
  Castling: boolean;
  AppendColon: boolean;
begin
  SameDest := TMoveList.Create;
  Clone := TStandardPosition.Create;
  Piece := Squares[TSquare10x12(AMove.Start)];
  Castling := False;
  AppendColon := False;
  case Piece of
    ptWKing, ptBKing:
    begin
      // Handle kingside castling
      if ((AMove.Start = 95) and (AMove.Dest = 97)) or
        ((AMove.Start = 25) and (AMove.Dest = 27)) then
      begin
        Result := 'O-O';
        Castling := True;
      end
      else
      // Handle queenside castling
      if ((AMove.Start = 95) and (AMove.Dest = 93)) or
        ((AMove.Start = 25) and (AMove.Dest = 23)) then
      begin
        Result := 'O-O-O';
        Castling := True;
      end
      else
      begin
        Result := PieceToStr(Piece);
      end;
    end;
    else // every other piece could be multiple times on the board
    begin
      Result := PieceToStr(Piece);
      for j := 0 to FLegalMoves.Count - 1 do
      begin
        // Check if there is another piece of the same kind, which can go to the current square
        if (AMove.Start <> FLegalMoves.Items[j].Start) and
          (FLegalMoves.Items[j].Dest = AMove.Dest) and
          (Squares[TSquare10x12(FLegalMoves.Items[j].Start)] = Piece) then
          SameDest.Add(FLegalMoves.Items[j]);
      end;
      if SameDest.Count > 0 then  // We need to distinguish
      begin
        Distinguished := True;
        // Check if we can distinguish by file
        for j := 0 to SameDest.Count - 1 do
        begin
          Distinguished := Distinguished and
            (SameDest.Items[j].Start.RFile <> AMove.Start.RFile);
        end;
        if Distinguished then
          Result := Result + TAlgebraicSquare(AMove.Start).RFile
        else
        begin
          Distinguished := True;
          // Check if we can distinguish by rank
          for j := 0 to SameDest.Count - 1 do
          begin
            Distinguished :=
              Distinguished and (SameDest.Items[j].Start.RRank <> AMove.Start.RRank);
          end;
          if Distinguished then
            Result := Result + TAlgebraicSquare(AMove.Start).RRank
          else
            // We cannot distinguish, so we need the whole square
            Result := Result + SquareToString(AMove.Start);
        end;
      end;
    end;
  end;
  if not Castling then
  begin
    // Check if dest is occupied or a pawn is taken en passant
    if (Squares[TSquare10x12(AMove.Dest)] <> ptEmpty) or
      ((Piece in [ptWPawn, ptBPawn]) and (TSquare10x12(AMove.Dest) = FEnPassant)) then
    begin
      if (Piece in [ptWPawn, ptBPawn]) and (Length(Result) = 0) then
        Result := Result + TAlgebraicSquare(AMove.Start).RFile;
      case CaptureSymbol of
        csNone: ;// Do nothing
        csColon: Result := Result + ':';
        csColonSuffix: AppendColon := True;
        csx: Result := Result + 'x';
      end;
    end;
    Result := Result + SquareToString(AMove.Dest);
    // Add colon if desired
    if AppendColon then
      Result := Result + ':';
    // Optionally add 'e.p.' if it is an en passant move
    if ShowEnPassantSuffix and (Piece in [ptWPawn, ptBPawn]) and
      (TSquare10x12(AMove.Dest) = FEnPassant) then
    begin
      Result := Result + 'e.p.';
    end;
    // Check for promotion
    if AMove.PromotionPiece <> ptEmpty then
    begin
      case PromotionSymbol of
        psNone: Result := Result + PieceToStr(AMove.PromotionPiece);
        psEqualSign: Result := Result + '=' + PieceToStr(AMove.PromotionPiece);
        psBrackets: Result := Result + '(' + PieceToStr(AMove.PromotionPiece) + ')';
        psSlash: Result := Result + '/' + PieceToStr(AMove.PromotionPiece);
      end;
    end;
  end;
  // Look for check and mate
  Clone.Copy(Self);
  Clone.PlayMove(AMove);
  if Clone.IsCheck then
  begin
    if Clone.LegalMoves.Count > 0 then
      Result := Result + '+'
    else
      Result := Result + '#';
  end;
  SameDest.Clear;
  Clone.Free;
  SameDest.Free;
end;

procedure TStandardPosition.PlayMove(AMove: TMove);
begin
  SilentPlayMove(AMove);
  Changed;
  if IsMate then
    if WhitesTurn then
      BlackWins
    else
      WhiteWins;
  if IsStaleMate then
    Draw;
end;

procedure TStandardPosition.SetupInitialPosition;
begin
  FromFEN(InitialFEN);
end;

function TStandardPosition.ToFEN: string;
var
  i, z, j: integer;
begin
  Result := '';
  // Piece placement
  for i := 2 to 9 do
  begin
    z := 0;
    for j := 1 to 8 do
    begin
      if Squares[10 * i + j] = ptEmpty then
        Inc(z)
      else
      begin
        if z > 0 then
          Result := Result + IntToStr(z);
        case Squares[10 * i + j] of
          ptWPawn: Result := Result + 'P';
          ptWKnight: Result := Result + 'N';
          ptWBishop: Result := Result + 'B';
          ptWRook: Result := Result + 'R';
          ptWQueen: Result := Result + 'Q';
          ptWKing: Result := Result + 'K';
          ptBPawn: Result := Result + 'p';
          ptBKnight: Result := Result + 'n';
          ptBBishop: Result := Result + 'b';
          ptBRook: Result := Result + 'r';
          ptBQueen: Result := Result + 'q';
          ptBKing: Result := Result + 'k';
        end;
        z := 0;
      end;
    end;
    if z > 0 then
      Result := Result + IntToStr(z);
    if i < 9 then
      Result := Result + '/';
  end;
  // Active color
  if FWhitesTurn then
    Result := Result + ' w '
  else
    Result := Result + ' b ';
  // Castiling availity
  if FCastlingAbility = [] then
    Result := Result + '-'
  else
  begin
    if ctWKingside in FCastlingAbility then
      Result := Result + 'K';
    if ctWQueenside in FCastlingAbility then
      Result := Result + 'Q';
    if ctBKingside in FCastlingAbility then
      Result := Result + 'k';
    if ctBQueenside in FCastlingAbility then
      Result := Result + 'q';
  end;
  Result := Result + ' ';
  // En passant
  if FEnPassant and (Ranks[3] or Ranks[6]) > 0 then
    Result := Result + IntToStr(FEnPassant) // TODO : <--
  else
    Result := Result + '-';
  Result := Result + ' ';
  // Halfmove clock
  Result := Result + IntToStr(FPliesSinceLastPawnMoveOrCapture) + ' ';
  // Fullmove number
  Result := Result + IntToStr(FMoveNumber);
end;

end.
