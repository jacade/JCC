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

//{$DEFINE LOGGING}

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
    LegalMovesNeedUpdate: boolean;
    function GetLegalMoves: TMoveList;
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
    constructor Create;
    // Copies important values from Source to Self
    procedure Copy(Source: TPosition); virtual;
    function IsLegal(AMove: TMove): boolean; virtual;
    procedure PlayMove(AMove: TMove); virtual;
    procedure SetupInitialPosition; virtual; abstract;
  public
    property CountOfFiles: byte read GetCountOfFiles;
    property CountOfRanks: byte read GetCountOfRanks;
    property LegalMoves: TMoveList read GetLegalMoves;
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
    //  FBlackKing: TSquare10x12;
    FCastlingAbility: TCastlingAbility;
    FOnChange: TNotifyEvent;
    FPliesSinceLastPawnMoveOrCapture: integer; // Important for 50 move rule
    // FSquares: array[0..119] of TPieceType;
    //  FWhiteKing: TSquare10x12;
    // BitBoards
    FEnPassant: TBitBoard;
    // 1. Pawns 2. Rooks 3. Knights 4. Bishops 5. Queens 6. Kings 7. White 8. Black
    FBitBoards: array[1..8] of TBitBoard;
    // 1. White 2. Black
    FAttackMaps: array[1..2] of TBitBoard;
    // Occupied Squares
    FOccupied: TBitBoard; // = FBitBoards[7] and FBitBoards[8]

    procedure Changed;
    function AntiDiagonalAttacks(index: integer): TBitBoard;
    function DiagonalAttacks(index: integer): TBitBoard;
    function HorizontalAttacks(Index: integer): TBitBoard;
    function VerticalAttacks(Index: integer): TBitBoard;
    function DiagonalAndAntiDiagonalAttacks(Index: integer): TBitBoard;
    function HorizontalAndVerticalAttacks(Index: integer): TBitBoard;
    // Adds moves given as bitboard to given move list
    procedure AddBitBoardToMoveList(PossibleMoves: TBitBoard;
      Start: integer; AMoveList: TMoveList; RelativeStart: integer = 0);
    // In the following we create pseudo-legal moves and store them in given move list
    procedure GenerateBishopPseudoMoves(Bishops, OppositeColorWithoutKingOrEmpty:
      TBitBoard; AMoveList: TMoveList);
    procedure GenerateKingPseudoMoves(Kings, OppositeColorWithoutKingOrEmpty: TBitBoard;
      AMoveList: TMoveList);
    procedure GenerateKnightPseudoMoves(Knights, OppositeColorWithoutKingOrEmpty:
      TBitBoard; AMoveList: TMoveList);
    procedure GenerateBlackPawnPseudoMoves(Pawns: TBitBoard; AMoveList: TMoveList);
    procedure GenerateWhitePawnPseudoMoves(Pawns: TBitBoard; AMoveList: TMoveList);
    procedure GenerateQueenPseudoMoves(Queens, OppositeColorWithoutKingOrEmpty:
      TBitBoard; AMoveList: TMoveList);
    procedure GenerateRookPseudoMoves(Rooks, OppositeColorWithoutKingOrEmpty: TBitBoard;
      AMoveList: TMoveList);
    function FilterMoveList(AMoveList: TMoveList; APiece: TPieceType = ptEmpty;
      StartFile: byte = 0; StartRank: byte = 0; DestSquare: byte = 0;
      APromotionPiece: TPieceType = ptEmpty): TMoveList;
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
      StartFile: byte = 0; StartRank: byte = 0; DestSquare: byte = 0;
      APromotionPiece: TPieceType = ptEmpty): TMoveList;
    procedure FromFEN(const AFEN: string);
    // Checks if the side to move is check
    function IsCheck: boolean;
    // Checks if the side not to move is in check
    function IsIllegalCheck: boolean;
    function IsMate: boolean;
    function IsMovePseudoLegal(APiece: TPieceType; Start, Dest: TSquare8x8): boolean;
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
    // If true, the converted move can be found in ResultMove
    function ValidateMove(var ResultMove: TMove; MovingPiece: TPieceType;
      Dest: byte; StartingFile: byte = 0; StartingRank: byte = 0): boolean;
    function ValidateMove(AMove: TMove): boolean;
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

const
  PAWN_MOVE = -1;

// Returns a Bitboard with zeroes and a 1 at the given position
function SquareToBitBoard(const ASquare: TSquare8x8): QWord;

implementation

function SquareToBitBoard(const ASquare: TSquare8x8): QWord;
begin
  // Result := QWord(1) shl (8 * (8 - ASquare.RRank) + ASquare.RFile - 1);
  Result := Ranks[ASquare.RRank] and Files[ASquare.RFile];
end;

{ TPosition }

function TPosition.GetLegalMoves: TMoveList;
begin
  if LegalMovesNeedUpdate then
  begin
    GenerateLegalMoves;
    LegalMovesNeedUpdate := False;
  end;
  Result := FLegalMoves;
end;

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

constructor TPosition.Create;
begin
  LegalMovesNeedUpdate := True;
end;

procedure TPosition.Copy(Source: TPosition);
var
  Move: TMove;
begin
  FMoveNumber := Source.FMoveNumber;
  FWhitesTurn := Source.FWhitesTurn;
  LegalMovesNeedUpdate := Source.LegalMovesNeedUpdate;
  FLegalMoves.Clear;
  for Move in Source.FLegalMoves do
    FLegalMoves.Add(Move.Copy);
end;

function TPosition.IsLegal(AMove: TMove): boolean;
begin
  Result := AMove in LegalMoves;
end;

procedure TPosition.PlayMove(AMove: TMove);
begin
  LegalMovesNeedUpdate := True;
end;

{ TStandardPosition }

procedure TStandardPosition.Changed;
begin
  //GenerateAttackMaps;
  // GenerateLegalMoves;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TStandardPosition.AntiDiagonalAttacks(index: integer): TBitBoard;
var
  Temp: QWord;
  CurrentAntiDiag: TBitBoard;
begin
  Temp := QWord(1) shl Index;
  CurrentAntiDiag := AntiDiagonals[(Index div 8) - (Index mod 8) + 8];
  Result := ((FOccupied and CurrentAntiDiag) - (2 * Temp)) xor
    ReverseBitBoard(ReverseBitBoard(FOccupied and CurrentAntiDiag) -
    (2 * ReverseBitBoard(Temp)));
  Result := Result and CurrentAntiDiag;
end;

function TStandardPosition.DiagonalAttacks(index: integer): TBitBoard;
var
  Temp: QWord;
  CurrentDiag: TBitBoard;
begin
  Temp := QWord(1) shl Index;
  CurrentDiag := Diagonals[(Index div 8) + (Index mod 8) + 1];
  Result := ((FOccupied and CurrentDiag) - 2 * Temp) xor
    ReverseBitBoard(ReverseBitBoard(FOccupied and CurrentDiag) -
    2 * ReverseBitBoard(Temp));
  Result := Result and CurrentDiag;
end;

function TStandardPosition.HorizontalAttacks(Index: integer): TBitBoard;
var
  Temp: QWord;
begin
  Temp := QWord(1) shl Index;
  Result := (FOccupied - 2 * Temp) xor ReverseBitBoard(ReverseBitBoard(FOccupied) -
    2 * ReverseBitBoard(Temp));
  Result := Result and Ranks[8 - (Index div 8)];
end;

function TStandardPosition.VerticalAttacks(Index: integer): TBitBoard;
var
  Temp: QWord;
  CurrentFile: TBitBoard;
begin
  Temp := QWord(1) shl Index;
  CurrentFile := Files[(Index mod 8) + 1];
  Result := ((FOccupied and CurrentFile) - (2 * Temp)) xor
    ReverseBitBoard(ReverseBitBoard(FOccupied and CurrentFile) -
    (2 * ReverseBitBoard(Temp)));
  Result := Result and CurrentFile;
end;

function TStandardPosition.DiagonalAndAntiDiagonalAttacks(Index: integer): TBitBoard;
begin
  Result := AntiDiagonalAttacks(Index) or DiagonalAttacks(Index);
end;

function TStandardPosition.HorizontalAndVerticalAttacks(Index: integer): TBitBoard;
begin
  Result := HorizontalAttacks(Index) or VerticalAttacks(Index);
end;

procedure TStandardPosition.AddBitBoardToMoveList(PossibleMoves: TBitBoard;
  Start: integer; AMoveList: TMoveList; RelativeStart: integer);
var
  i: integer;
begin
  if Start = PAWN_MOVE then // Moving piece is a pawn
  begin
    while PossibleMoves > 0 do
    begin
      i := NumberOfTrailingZeroes(PossibleMoves);
      AMoveList.Add(CreateMoveFromInt(i + RelativeStart, i));
      PossibleMoves := PossibleMoves and (PossibleMoves - 1);
    end;
  end
  else  // This is for the other pieces
  begin
    while PossibleMoves > 0 do
    begin
      i := NumberOfTrailingZeroes(PossibleMoves);
      AMoveList.Add(CreateMoveFromInt(Start, i));
      PossibleMoves := PossibleMoves and (PossibleMoves - 1);
    end;
  end;
end;

procedure TStandardPosition.GenerateBishopPseudoMoves(Bishops,
  OppositeColorWithoutKingOrEmpty: TBitBoard; AMoveList: TMoveList);
var
  i: integer;
  Moves: TBitBoard;
begin
  while Bishops > 0 do
  begin
    i := NumberOfTrailingZeroes(Bishops);
    Moves := DiagonalAndAntiDiagonalAttacks(i) and OppositeColorWithoutKingOrEmpty;
    AddBitBoardToMoveList(Moves, i, AMoveList);
    Bishops := Bishops and (Bishops - 1);
  end;
end;

procedure TStandardPosition.GenerateKingPseudoMoves(
  Kings, OppositeColorWithoutKingOrEmpty: TBitBoard; AMoveList: TMoveList);
var
  Moves: TBitBoard;
  i: integer;
begin
  while Kings > 0 do
  begin
    i := NumberOfTrailingZeroes(Kings);
    Moves := KingMoves[i] and OppositeColorWithoutKingOrEmpty;
    AddBitBoardToMoveList(Moves, i, AMoveList);
    Kings := Kings and (Kings - 1);
  end;
end;

procedure TStandardPosition.GenerateKnightPseudoMoves(Knights,
  OppositeColorWithoutKingOrEmpty: TBitBoard; AMoveList: TMoveList);
var
  i, Moves: TBitBoard;
begin
  while Knights > 0 do
  begin
    i := NumberOfTrailingZeroes(Knights);
    Moves := BitBoard.KnightMoves[i] and OppositeColorWithoutKingOrEmpty;
    AddBitBoardToMoveList(Moves, i, AMoveList);
    Knights := Knights and (Knights - 1);
  end;
end;

procedure TStandardPosition.GenerateBlackPawnPseudoMoves(Pawns: TBitBoard;
  AMoveList: TMoveList);
var
  WhitePiecesWithoutKing, PawnMoves, Empty: TBitBoard;
begin
  WhitePiecesWithoutKing := FBitBoards[7] and not FBitBoards[6];
  Empty := not (FBitBoards[7] or FBitBoards[8]);
  // Black pawn captures to the right
  PawnMoves := ((Pawns and not Files[8]) shl 9) and
    (WhitePiecesWithoutKing or FEnPassant);
  AddBitBoardToMoveList(PawnMoves, PAWN_MOVE, AMoveList, -9);
  // Black pawn captures to the left
  PawnMoves := ((Pawns and not Files[1]) shl 7) and
    (WhitePiecesWithoutKing or FEnPassant);
  AddBitBoardToMoveList(PawnMoves, PAWN_MOVE, AMoveList, -7);
  // Black pawn goes one forward
  PawnMoves := (Pawns shl 8) and Empty;
  AddBitBoardToMoveList(PawnMoves, PAWN_MOVE, AMoveList, -8);
  // Black pawn goes two forward
  PawnMoves := ((Pawns and Ranks[7]) shl 16) and Empty and (Empty shl 8);
  AddBitBoardToMoveList(PawnMoves, PAWN_MOVE, AMoveList, -16);
end;

procedure TStandardPosition.GenerateWhitePawnPseudoMoves(Pawns: TBitBoard;
  AMoveList: TMoveList);
var
  BlackPiecesWithoutKing, PawnMoves, Empty: TBitBoard;
begin
  BlackPiecesWithoutKing := FBitBoards[8] and not FBitBoards[6];
  Empty := not (FBitBoards[7] or FBitBoards[8]);
  // White pawn captures to the right
  PawnMoves := ((Pawns and not Files[8]) shr 7) and
    (BlackPiecesWithoutKing or FEnPassant);
  AddBitBoardToMoveList(PawnMoves, PAWN_MOVE, AMoveList, 7);
  // White pawn captures to the left
  PawnMoves := ((Pawns and not Files[1]) shr 9) and
    (BlackPiecesWithoutKing or FEnPassant);
  AddBitBoardToMoveList(PawnMoves, PAWN_MOVE, AMoveList, 9);
  // White pawn goes one forward
  PawnMoves := (Pawns shr 8) and Empty;
  AddBitBoardToMoveList(PawnMoves, PAWN_MOVE, AMoveList, 8);
  // White pawn goes two forward
  PawnMoves := ((Pawns and Ranks[2]) shr 16) and Empty and (Empty shr 8);
  AddBitBoardToMoveList(PawnMoves, PAWN_MOVE, AMoveList, 16);
end;

procedure TStandardPosition.GenerateQueenPseudoMoves(Queens,
  OppositeColorWithoutKingOrEmpty: TBitBoard; AMoveList: TMoveList);
begin
  GenerateBishopPseudoMoves(Queens, OppositeColorWithoutKingOrEmpty, AMoveList);
  GenerateRookPseudoMoves(Queens, OppositeColorWithoutKingOrEmpty, AMoveList);
end;

procedure TStandardPosition.GenerateRookPseudoMoves(
  Rooks, OppositeColorWithoutKingOrEmpty: TBitBoard; AMoveList: TMoveList);
var
  i, Moves: TBitBoard;
begin
  while Rooks > 0 do
  begin
    i := NumberOfTrailingZeroes(Rooks);
    Moves := HorizontalAndVerticalAttacks(i) and OppositeColorWithoutKingOrEmpty;
    AddBitBoardToMoveList(Moves, i, AMoveList);
    Rooks := Rooks and (Rooks - 1);
  end;
end;

function TStandardPosition.FilterMoveList(AMoveList: TMoveList;
  APiece: TPieceType; StartFile: byte; StartRank: byte; DestSquare: byte;
  APromotionPiece: TPieceType): TMoveList;
var
  NoFilterPiece, NoFilterDest, NoFilterPromo, NoFilterStartR, NoFilterStartF: boolean;
  Move: TMove;
begin
  NoFilterPiece := APiece = ptEmpty;
  NoFilterDest := DestSquare = 0;
  NoFilterStartF := StartFile = 0;
  NoFilterStartR := StartRank = 0;
  NoFilterPromo := APromotionPiece = ptEmpty;
  Result := TMoveList.Create;
  for Move in AMoveList do
  begin
    if (NoFilterPiece or (Squares[Move.Start] = APiece)) and
      (NoFilterDest or (Move.Dest = DestSquare)) and
      (NoFilterStartF or (Move.Start8x8.RFile = StartFile)) and
      (NoFilterStartR or (Move.Start8x8.RRank = StartRank)) and
      (NoFilterPromo or (Move.PromotionPiece = APromotionPiece)) then
      Result.Add(Move);
  end;
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
  // 1. Horizontal 2. Vertical 3. Diagonal 4. Anti-Diagonal 5. Illegal En Passants right 6. En passants left
  Pinned: array[1..6] of TBitBoard;
  InCheck: boolean;

  procedure GenerateBishopMoves(Bishops: TBitBoard);
  var
    i, Moves, B: TBitBoard;
  begin
    B := Bishops and not Pinned[3];
    while B > 0 do
    begin
      i := NumberOfTrailingZeroes(B);
      Moves := AntiDiagonalAttacks(i) and OppositeColorWithoutKingOrEmpty;
      AddBitBoardToMoveList(Moves, i, FLegalMoves);
      B := B and (B - 1);
    end;
    B := Bishops and not Pinned[4];
    while B > 0 do
    begin
      i := NumberOfTrailingZeroes(B);
      Moves := DiagonalAttacks(i) and OppositeColorWithoutKingOrEmpty;
      AddBitBoardToMoveList(Moves, i, FLegalMoves);
      B := B and (B - 1);
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
    Moves: TBitBoard;
    i, j: integer;
  begin
    while Kings > 0 do
    begin
      i := NumberOfTrailingZeroes(Kings);
      Moves := KingMoves[i] and OppositeColorWithoutKingOrEmpty;
      while Moves > 0 do
      begin
        j := NumberOfTrailingZeroes(Moves);
        if not IsAttacked(j) then
          FLegalMoves.Add(CreateMoveFromInt(i, j));
        Moves := Moves and (Moves - 1);
      end;
      // AddBitBoardToMoveList(Moves, i);
      Kings := Kings and (Kings - 1);
    end;
  end;

  procedure GenerateKnightMoves(Knights: TBitBoard);
  var
    i, Moves: TBitBoard;
  begin
    //while Knights > 0 do
    //begin
    //  i := NumberOfTrailingZeroes(Knights);
    //  Moves := BitBoard.KnightMoves[i] and OppositeColorWithoutKingOrEmpty;
    //  AddBitBoardToMoveList(Moves, i);
    //  Knights := Knights and (Knights - 1);
    //end;
    GenerateKnightPseudoMoves(Knights, OppositeColorWithoutKingOrEmpty, FLegalMoves);
  end;

  procedure GeneratePawnPromotionMoves(AMoveList: TMoveList);
  var
    temp: TMoveList;
    Piece: TBasicPieceType;
    i: integer;
    Start, Dest: byte;
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
          temp.Add(TMove.Create(Start, Dest, PieceType(Piece, FWhitesTurn)));
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
      PawnMoves := ((WP and not (Files[8] or Pinned[1] or Pinned[2] or
        Pinned[4] or Pinned[5])) shr 7) and (BlackPiecesWithoutKing or FEnPassant);
      AddBitBoardToMoveList(PawnMoves, -1, FLegalMoves, 7);
      // White pawn captures to the left
      PawnMoves := ((WP and not (Files[1] or Pinned[1] or Pinned[2] or
        Pinned[3] or Pinned[6])) shr 9) and (BlackPiecesWithoutKing or FEnPassant);
      AddBitBoardToMoveList(PawnMoves, -1, FLegalMoves, 9);
    end
    else
    begin
      // Black pawn captures to the right
      PawnMoves := ((BP and not (Files[8] or Pinned[1] or Pinned[2] or
        Pinned[3] or Pinned[5])) shl 9) and (WhitePiecesWithoutKing or FEnPassant);
      AddBitBoardToMoveList(PawnMoves, -1, FLegalMoves, -9);
      // Black pawn captures to the left
      PawnMoves := ((BP and not (Files[1] or Pinned[1] or Pinned[2] or
        Pinned[4] or Pinned[6])) shl 7) and (WhitePiecesWithoutKing or FEnPassant);
      AddBitBoardToMoveList(PawnMoves, -1, FLegalMoves, -7);
    end;
  end;

  procedure GeneratePawnForwardMoves;
  var
    PawnMoves: TBitBoard;
  begin
    if FWhitesTurn then
    begin
      // White pawn goes one forward
      PawnMoves := ((WP and not (Pinned[1] or Pinned[3] or Pinned[4])) shr 8) and Empty;
      AddBitBoardToMoveList(PawnMoves, -1, FLegalMoves, 8);
      // White pawn goes two forward
      PawnMoves := ((WP and Ranks[2] and not (Pinned[1] or Pinned[3] or Pinned[4])) shr
        16) and Empty and (Empty shr 8);
      AddBitBoardToMoveList(PawnMoves, -1, FLegalMoves, 16);
    end
    else
    begin
      // Black pawn goes one forward
      PawnMoves := ((BP and not (Pinned[1] or Pinned[3] or Pinned[4])) shl 8) and Empty;
      AddBitBoardToMoveList(PawnMoves, -1, FLegalMoves, -8);
      // Black pawn goes two forward
      PawnMoves := ((BP and Ranks[7] and not (Pinned[1] or Pinned[3] or Pinned[4])) shl
        16) and Empty and (Empty shl 8);
      AddBitBoardToMoveList(PawnMoves, -1, FLegalMoves, -16);
    end;
  end;

  procedure GenerateRookMoves(Rooks: TBitBoard);
  var
    i, Moves, R: TBitBoard;
  begin
    R := Rooks and not Pinned[1];
    while R > 0 do
    begin
      i := NumberOfTrailingZeroes(R);
      Moves := VerticalAttacks(i) and OppositeColorWithoutKingOrEmpty;
      AddBitBoardToMoveList(Moves, i, FLegalMoves);
      R := R and (R - 1);
    end;
    R := Rooks and not Pinned[2];
    while R > 0 do
    begin
      i := NumberOfTrailingZeroes(R);
      Moves := HorizontalAttacks(i) and OppositeColorWithoutKingOrEmpty;
      AddBitBoardToMoveList(Moves, i, FLegalMoves);
      R := R and (R - 1);
    end;
  end;

var
  i: byte;
  j: integer;
  BCastlingAbility: TCastlingAbility;
  BEnPassant: TBitBoard;
  BPliesSinceLastPawnMoveOrCapture: integer;
  BMoveNumer: integer;
  BackupBoards: array[1..8] of TBitBoard;
  d, c, tb, tc: extended;
  R, B, K, Pinner, SuperKingAttacks, Blockers, Blocker: TBitBoard;
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
  for i := 1 to 6 do
    Pinned[i] := 0;
  // Find all pinned pieces
  // based on Opposite Ray-Directions http://chessprogramming.wikispaces.com/Checks+and+Pinned+Pieces+(Bitboards)
  if FWhitesTurn then
  begin
    K := WK;
    R := BR or BQ;
    B := BB or BQ;
    Blockers := FBitBoards[7];
  end
  else
  begin
    K := BK;
    R := WR or WQ;
    B := WB or WQ;
    Blockers := FBitBoards[8];
  end;
  // Horizontal pins
  Pinner := R;
  SuperKingAttacks := HorizontalAttacks(NumberOfTrailingZeroes(K));
  while Pinner > 0 do
  begin
    i := NumberOfTrailingZeroes(Pinner);
    Pinned[1] := Pinned[1] or (HorizontalAttacks(i) and SuperKingAttacks and Blockers);
    Pinner := Pinner and (Pinner - 1);
  end;
  // Vertical pins
  Pinner := R;
  SuperKingAttacks := VerticalAttacks(NumberOfTrailingZeroes(K));
  while Pinner > 0 do
  begin
    i := NumberOfTrailingZeroes(Pinner);
    Pinned[2] := Pinned[2] or (VerticalAttacks(i) and SuperKingAttacks and Blockers);
    Pinner := Pinner and (Pinner - 1);
  end;
  // Diagonal pins
  Pinner := B;
  SuperKingAttacks := DiagonalAttacks(NumberOfTrailingZeroes(K));
  while Pinner > 0 do
  begin
    i := NumberOfTrailingZeroes(Pinner);
    Pinned[3] := Pinned[3] or (DiagonalAttacks(i) and SuperKingAttacks and Blockers);
    Pinner := Pinner and (Pinner - 1);
  end;
  // Antidiagonal pins
  Pinner := B;
  SuperKingAttacks := AntiDiagonalAttacks(NumberOfTrailingZeroes(K));
  while Pinner > 0 do
  begin
    i := NumberOfTrailingZeroes(Pinner);
    Pinned[4] := Pinned[4] or (AntiDiagonalAttacks(i) and SuperKingAttacks and Blockers);
    Pinner := Pinner and (Pinner - 1);
  end;
  // Illegal en passants
  if EnPassant > 0 then
  begin
    // En passants to the right
    if FWhitesTurn then
    begin
      Pinner := R and Ranks[5];
      // Temporary remove last moved pawn
      FOccupied := FOccupied and not (FEnPassant shl 8);
      // Get Pawn that is able to capture en passant
      Blocker := Blockers and (FEnPassant shl 7);
    end
    else
    begin
      Pinner := R and Ranks[4];
      // Temporary remove last moved pawn
      FOccupied := FOccupied and not (FEnPassant shr 8);
      // Get Pawn that is able to capture en passant
      Blocker := Blockers and (FEnPassant shr 9);
    end;
    SuperKingAttacks := HorizontalAttacks(NumberOfTrailingZeroes(K));
    while Pinner > 0 do
    begin
      i := NumberOfTrailingZeroes(Pinner);
      Pinned[5] := Pinned[5] or (HorizontalAttacks(i) and SuperKingAttacks and Blocker);
      Pinner := Pinner and (Pinner - 1);
    end;
    // En passants to the left
    if FWhitesTurn then
    begin
      Pinner := R and Ranks[5];
      // Get Pawn that is able to capture en passant
      Blocker := Blockers and (FEnPassant shl 9);
    end
    else
    begin
      Pinner := R and Ranks[4];
      // Get Pawn that is able to capture en passant
      Blocker := Blockers and (FEnPassant shr 7);
    end;
    //WriteLn(BitBoardToStr(Pinner), 'PINNER');
    //WriteLn(BitBoardToStr(FOccupied), 'OCC');
    //WriteLn(BitBoardToStr(Blocker), 'BLOCKER');
    SuperKingAttacks := HorizontalAttacks(NumberOfTrailingZeroes(K));
    while Pinner > 0 do
    begin
      i := NumberOfTrailingZeroes(Pinner);
      Pinned[6] := Pinned[6] or (HorizontalAttacks(i) and SuperKingAttacks and Blocker);
      Pinner := Pinner and (Pinner - 1);
    end;
    // Add pawns again
    FOccupied := FOccupied or FBitBoards[1];
  end;
  // Check if we are in check reicht nicht brauchen auch doppelschach
  InCheck := IsCheck;
  if InCheck then
  begin

  end;
  //for i := 1 to 6 do
  //  WriteLn(BitBoardToStr(Pinned[i]), i);
  //a := ET.Elapsed;
  tb := 0;
  tc := 0;
  // The following takes up to 1 ms, could this be made faster?
  {$IFDEF Logging}
  ET.Start;
  a := ET.Elapsed;
{$ENDIF}
    {$IFDEF Logging}
  d := ET.Elapsed;
    {$ENDIF}
  FLegalMoves.Clear;
  GeneratePawnCaptureMoves;
  GeneratePawnForwardMoves;
  GenerateCastlingMoves;
  if WhitesTurn then
  begin
    OppositeColorWithoutKingOrEmpty := BlackPiecesWithoutKing or Empty;
    GenerateBishopMoves((WB or WQ) and not (Pinned[1] or Pinned[2]));
    GenerateRookMoves((WR or WQ) and not (Pinned[3] or Pinned[4]));
    GenerateKnightMoves(WN and not (Pinned[1] or Pinned[2] or Pinned[3] or Pinned[4]));
    GenerateKingMoves(WK);
  end
  else
  begin
    OppositeColorWithoutKingOrEmpty := WhitePiecesWithoutKing or Empty;
    GenerateBishopMoves((BB or BQ) and not (Pinned[1] or Pinned[2]));
    GenerateRookMoves((BR or BQ) and not (Pinned[3] or Pinned[4]));
    GenerateKnightMoves(BN and not (Pinned[1] or Pinned[2] or Pinned[3] or Pinned[4]));
    GenerateKingMoves(BK);
  end;
    {$IFDEF Logging}
  tb := tb + ET.Elapsed - d;

    {$ENDIF}
  // Backup Position, Play Move, Position Valid?
  j := 0;
  // Backup current Position
  BEnPassant := FEnPassant;
  BPliesSinceLastPawnMoveOrCapture := FPliesSinceLastPawnMoveOrCapture;
  BCastlingAbility := FCastlingAbility;
  BMoveNumer := FMoveNumber;
  //for i := 1 to 8 do
  //  BackupBoards[i] := FBitBoards[i];
  //while j < FLegalMoves.Count do
  //begin
  //  Self.SilentPlayMove(FLegalMoves.Items[j]);
  //  {$IFDEF Logging}
  //  c := ET.Elapsed;
  //  {$ENDIF}
  //  //if not Self.IsIllegalCheck then
  //  //  Inc(j)
  //  //else
  //  //  FLegalMoves.Delete(j);
  //  Inc(j);
  //  {$IFDEF Logging}
  //  tc := tc + ET.Elapsed - c;
  //  {$ENDIF}
  //  // Restore inital values
  //  FEnPassant := BEnPassant;
  //  FPliesSinceLastPawnMoveOrCapture := BPliesSinceLastPawnMoveOrCapture;
  //  FCastlingAbility := BCastlingAbility;
  //  FMoveNumber := BMoveNumer;
  //  FBlackKing := BBlackKing;
  //  FWhiteKing := BWhiteKing;
  //  FWhitesTurn := not FWhitesTurn;
  //  for i := 1 to 8 do
  //    FBitBoards[i] := BackupBoards[i];
  //end;
  GeneratePawnPromotionMoves(FLegalMoves);
  //Write(' 1: ', FormatFloat('0.##', (tb) * 1000000), 'µs');
  //Write('  2: ', FormatFloat('0.##', (tc) * 1000000), 'µs');
  //Writeln('  Total: ', FormatFloat('0.##', (ET.Elapsed - a) * 1000000), 'µs');
  //  WriteLn('Zahl der möglichen Züge: ', FLegalMoves.Count);

  {$IFDEF Logging}
  Inc(Zuege, FLegalMoves.Count);
  Zeit := Zeit + (ET.Elapsed - a);
  ET.Stop;
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
      if ((QWord(1) shl Index) and FBitBoards[i] and FBitBoards[j]) > 0 then
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
  Moves: TBitBoard;
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
  Moves := HorizontalAndVerticalAttacks(Index) and FOccupied;
  if (FBitBoards[2] or FBitBoards[5]) and FBitBoards[Opp] and Moves > 0 then
    Exit(True);
  // Do bishop moves from start
  Moves := DiagonalAndAntiDiagonalAttacks(Index) and FOccupied;
  if (FBitBoards[4] or FBitBoards[5]) and FBitBoards[Opp] and Moves > 0 then
    Exit(True);
  // Do knight moves from start
  Moves := BitBoard.KnightMoves[Index] and FOccupied;
  if FBitBoards[3] and FBitBoards[Opp] and Moves > 0 then
    Exit(True);
  // Do king moves from start
  Moves := KingMoves[Index] and FOccupied;
  if FBitBoards[6] and FBitBoards[Opp] and Moves > 0 then
    Exit(True);
  // Do opposite pawn Moves
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
  rk, fl, i, Coordinate: byte;
  temp: string;
  RegFEN: TRegExpr;
  piece, color: integer;
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
  for i := 1 to 8 do
    FBitBoards[i] := 0;
  for rk := 0 to 7 do
  begin
    temp := p.Strings[rk];
    fl := 0;
    for i := 1 to Length(temp) do
    begin
      Coordinate := rk * 8 + fl;
      if fl > 7 then
        raise EInvalidFEN.Create('FEN is invalid');
      case temp[i] of
        '1'..'8':
        begin
          Inc(fl, StrToInt(temp[i]));
          Continue;
        end;
        'p', 'r', 'n', 'b', 'q', 'k':
        begin
          piece := Pos(temp[i], 'prnbqk');
          color := 8;
        end;
        'P', 'R', 'N', 'B', 'Q', 'K':
        begin
          piece := Pos(temp[i], 'PRNBQK');
          color := 7;
        end;
      end;
      FBitBoards[piece] := FBitBoards[piece] or QWord(1) shl Coordinate;
      FBitBoards[color] := FBitBoards[color] or QWord(1) shl Coordinate;
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
  Start := SquareToBitBoard(AMove.Start8x8);
  Dest := SquareToBitBoard(AMove.Dest8x8);
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
  i, k: integer;
begin
  for i := 1 to 8 do
    Writeln(BitBoardToStr(FBitBoards[i]), i);
  WriteLn(BitBoardToStr(FEnPassant), 'En Passant');
  k := 1;
  Write('|');
  for i := 0 to 63 do
  begin
    case Squares[i] of
      ptEmpty: Write('  ');
      ptWPawn: Write(' P ');
      ptWKnight: Write(' N ');
      ptWBishop: Write(' B ');
      ptWRook: Write(' R ');
      ptWQueen: Write(' Q ');
      ptWKing: Write(' K ');
      ptBPawn: Write(' p ');
      ptBKnight: Write(' n ');
      ptBBishop: Write(' b ');
      ptBRook: Write(' r ');
      ptBQueen: Write(' q ');
      ptBKing: Write(' k ');
      ptOff: Write(' X ');
    end;
    Write('|');
    if k = 8 then
    begin
      k := 0;
      WriteLn;
      Write('|');
    end;
    Inc(k);
  end;
end;

constructor TStandardPosition.Create;
begin
  inherited Create;
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
  StartFile: byte; StartRank: byte; DestSquare: byte;
  APromotionPiece: TPieceType): TMoveList;
  //var
  //  NoFilterPiece, NoFilterDest, NoFilterPromo, NoFilterStartR, NoFilterStartF: boolean;
  //  Move: TMove;
  //begin
  //  NoFilterPiece := APiece = ptEmpty;
  //  NoFilterDest := DestSquare = 0;
  //  NoFilterStartF := StartFile = 0;
  //  NoFilterStartR := StartRank = 0;
  //  NoFilterPromo := APromotionPiece = ptEmpty;
  //  Result := TMoveList.Create;
  //  for Move in FLegalMoves do
  //  begin
  //    if (NoFilterPiece or (Squares[TSquare10x12(Move.Start)] = APiece)) and
  //      (NoFilterDest or (Move.Dest = DestSquare)) and
  //      (NoFilterStartF or (Move.Start.RFile = StartFile)) and
  //      (NoFilterStartR or (Move.Start.RRank = StartRank)) and
  //      (NoFilterPromo or (Move.PromotionPiece = APromotionPiece)) then
  //      Result.Add(Move);
  //  end;
begin
  Result := FilterMoveList(FLegalMoves, APiece, StartFile, StartRank,
    DestSquare, APromotionPiece);
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

function TStandardPosition.IsMovePseudoLegal(APiece: TPieceType;
  Start, Dest: TSquare8x8): boolean;
var
  i: integer;
  Destination: TBitBoard;
begin
  // TODO: Castling
  i := (Start.RFile + 63 - Start.RRank * 8);
  Destination := SquareToBitBoard(dest);
  case APiece of
    ptEmpty, ptOff: Result := False;
    ptWPawn: ;
    ptBPawn: ;
    else
      case BasisPieceType(APiece) of
        bptKnight:
        begin
          Result := BitBoard.KnightMoves[i] and Destination > 0;
        end;
        bptBishop:
        begin
          Result := DiagonalAndAntiDiagonalAttacks(i) and Destination > 0;
        end;
        bptRook:
        begin
          Result := HorizontalAndVerticalAttacks(i) and Destination > 0;
        end;
        bptQueen:
        begin
          Result := (DiagonalAndAntiDiagonalAttacks(i) or
            HorizontalAndVerticalAttacks(i)) and Destination > 0;
        end;
        bptKing:
        begin
          Result := KingMoves[i] and Destination > 0;
        end;
      end;
  end;
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
  Piece := Squares[AMove.Start];
  Castling := False;
  AppendColon := False;
  case Piece of
    ptWKing, ptBKing:
    begin
      // Handle kingside castling
      if ((AMove.Start = 60) and (AMove.Dest = 62)) or
        ((AMove.Start = 4) and (AMove.Dest = 6)) then
      begin
        Result := 'O-O';
        Castling := True;
      end
      else
      // Handle queenside castling
      if ((AMove.Start = 60) and (AMove.Dest = 58)) or
        ((AMove.Start = 4) and (AMove.Dest = 2)) then
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
        if (AMove.Start8x8 <> FLegalMoves.Items[j].Start8x8) and
          (FLegalMoves.Items[j].Dest8x8 = AMove.Dest8x8) and
          (Squares[FLegalMoves.Items[j].Start] = Piece) then
          SameDest.Add(FLegalMoves.Items[j]);
      end;
      if SameDest.Count > 0 then  // We need to distinguish
      begin
        Distinguished := True;
        // Check if we can distinguish by file
        for j := 0 to SameDest.Count - 1 do
        begin
          Distinguished := Distinguished and
            (SameDest.Items[j].Start8x8.RFile <> AMove.Start8x8.RFile);
        end;
        if Distinguished then
          Result := Result + TAlgebraicSquare(AMove.Start8x8).RFile
        else
        begin
          Distinguished := True;
          // Check if we can distinguish by rank
          for j := 0 to SameDest.Count - 1 do
          begin
            Distinguished :=
              Distinguished and (SameDest.Items[j].Start8x8.RRank <>
              AMove.Start8x8.RRank);
          end;
          if Distinguished then
            Result := Result + TAlgebraicSquare(AMove.Start8x8).RRank
          else
            // We cannot distinguish, so we need the whole square
            Result := Result + SquareToString(AMove.Start8x8);
        end;
      end;
    end;
  end;
  if not Castling then
  begin
    // Check if dest is occupied or a pawn is taken en passant
    if (Squares[AMove.Dest] <> ptEmpty) or
      ((Piece in [ptWPawn, ptBPawn]) and (SquareToBitBoard(AMove.Dest8x8) =
      FEnPassant)) then
    begin
      if (Piece in [ptWPawn, ptBPawn]) and (Length(Result) = 0) then
        Result := Result + TAlgebraicSquare(AMove.Start8x8).RFile;
      case CaptureSymbol of
        csNone: ;// Do nothing
        csColon: Result := Result + ':';
        csColonSuffix: AppendColon := True;
        csx: Result := Result + 'x';
      end;
    end;
    Result := Result + SquareToString(AMove.Dest8x8);
    // Add colon if desired
    if AppendColon then
      Result := Result + ':';
    // Optionally add 'e.p.' if it is an en passant move
    if ShowEnPassantSuffix and (Piece in [ptWPawn, ptBPawn]) and
      (SquareToBitBoard(AMove.Dest8x8) = FEnPassant) then
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

function TStandardPosition.ValidateMove(var ResultMove: TMove;
  MovingPiece: TPieceType; Dest: byte; StartingFile: byte; StartingRank: byte): boolean;
var
  Candidates, OppositePiecesWithoutKingOrEmpty, FilterFile, FilterRank,
  BEnPassant, Attacks: TBitBoard;
  PseudoLegalMoves, FoundMoves: TMoveList;
  j, BPliesSinceLastPawnMoveOrCapture, BMoveNumber, i: integer;
  BackupBoards: array[1..8] of TBitBoard;
  BCastlingAbility: TCastlingAbility;
  a, b, c, d, e, f: extended;
  Move: TMove;
begin
  {$IFDEF Logging}
  b := ET.Elapsed;
  {$ENDIF}
  if StartingFile > 0 then
    FilterFile := Files[StartingFile]
  else
    FilterFile := not QWOrd(0);
  if StartingRank > 0 then
    FilterRank := Ranks[StartingRank]
  else
    FilterRank := not QWord(0);
  case MovingPiece of
    ptEmpty, ptOff: raise Exception.Create('Moving piece cannot be empty or off!');
    ptWPawn: Candidates := FBitBoards[1] and FBitBoards[7] and FilterFile and FilterRank;
    ptWKnight: Candidates := FBitBoards[3] and FBitBoards[7] and
        FilterFile and FilterRank;
    ptWBishop: Candidates := FBitBoards[4] and FBitBoards[7] and
        FilterFile and FilterRank;
    ptWRook: Candidates := FBitBoards[2] and FBitBoards[7] and FilterFile and FilterRank;
    ptWQueen: Candidates := FBitBoards[5] and FBitBoards[7] and
        FilterFile and FilterRank;
    ptWKing: Candidates := FBitBoards[6] and FBitBoards[7] and FilterFile and FilterRank;
    ptBPawn: Candidates := FBitBoards[1] and FBitBoards[8] and FilterFile and FilterRank;
    ptBKnight: Candidates := FBitBoards[3] and FBitBoards[8] and
        FilterFile and FilterRank;
    ptBBishop: Candidates := FBitBoards[4] and FBitBoards[8] and
        FilterFile and FilterRank;
    ptBRook: Candidates := FBitBoards[2] and FBitBoards[8] and FilterFile and FilterRank;
    ptBQueen: Candidates := FBitBoards[5] and FBitBoards[8] and
        FilterFile and FilterRank;
    ptBKing: Candidates := FBitBoards[6] and FBitBoards[8] and FilterFile and FilterRank;
  end;
  if Candidates = 0 then
    Exit(False);

  //Write(' 1: ', FormatFloat('0.##', (ET.Elapsed - b) * 1000000), 'µs');
  {$IFDEF Logging}
  c := ET.Elapsed;
  a := ET.Elapsed;
  {$ENDIF}
  PseudoLegalMoves := TMoveList.Create(True);
  if IsWhite(MovingPiece) then
  begin
    OppositePiecesWithoutKingOrEmpty :=
      (FBitBoards[8] and not FBitBoards[6]) or not (FBitBoards[7] or FBitBoards[8]);
    if BasisPieceType(MovingPiece) = bptPawn then
      GenerateWhitePawnPseudoMoves(Candidates, PseudoLegalMoves);
  end
  else
  begin
    OppositePiecesWithoutKingOrEmpty :=
      (FBitBoards[7] and not FBitBoards[6]) or not (FBitBoards[7] or FBitBoards[8]);
    if BasisPieceType(MovingPiece) = bptPawn then
      GenerateBlackPawnPseudoMoves(Candidates, PseudoLegalMoves);
  end;
  //Write(' 2: ', FormatFloat('0.##', (ET.Elapsed - c) * 1000000), 'µs');
  {$IFDEF Logging}
  d := ET.Elapsed;
  {$ENDIF}
  case BasisPieceType(MovingPiece) of
    bptKnight:
    begin
      //GenerateKnightPseudoMoves(Candidates, OppositePiecesWithoutKingOrEmpty,
      //  PseudoLegalMoves);
      Attacks := BitBoard.KnightMoves[Dest] and Candidates;
      while Attacks > 0 do
      begin
        i := NumberOfTrailingZeroes(Attacks);
        PseudoLegalMoves.Add(CreateMoveFromInt(i, Dest));
        Attacks := Attacks and (Attacks - 1);
      end;
    end;
    bptBishop:
    begin
      //if SquareToBitBoard(Dest) and WhiteSquares > 0 then
      //  Candidates := Candidates and WhiteSquares
      //else
      //  Candidates := Candidates and BlackSquares;
      //GenerateBishopPseudoMoves(Candidates, OppositePiecesWithoutKingOrEmpty,
      //  PseudoLegalMoves);
      Attacks := DiagonalAndAntiDiagonalAttacks(Dest) and Candidates;
      while Attacks > 0 do
      begin
        i := NumberOfTrailingZeroes(Attacks);
        PseudoLegalMoves.Add(CreateMoveFromInt(i, Dest));
        Attacks := Attacks and (Attacks - 1);
      end;
    end;
    bptRook:
    begin
      //Candidates := Candidates and (Files[TSquare8x8(Dest).RFile] or
      //  Ranks[TSquare8x8(Dest).RRank]);
      //GenerateRookPseudoMoves(Candidates, OppositePiecesWithoutKingOrEmpty,
      //  PseudoLegalMoves);
      Attacks := HorizontalAndVerticalAttacks(Dest) and Candidates;
      while Attacks > 0 do
      begin
        i := NumberOfTrailingZeroes(Attacks);
        PseudoLegalMoves.Add(CreateMoveFromInt(i, Dest));
        Attacks := Attacks and (Attacks - 1);
      end;
    end;
    bptQueen:
    begin
      //GenerateQueenPseudoMoves(Candidates, OppositePiecesWithoutKingOrEmpty,
      //    PseudoLegalMoves);
      Attacks := (DiagonalAndAntiDiagonalAttacks(Dest) or
        HorizontalAndVerticalAttacks(Dest)) and Candidates;
      while Attacks > 0 do
      begin
        i := NumberOfTrailingZeroes(Attacks);
        PseudoLegalMoves.Add(CreateMoveFromInt(i, Dest));
        Attacks := Attacks and (Attacks - 1);
      end;
    end;
    bptKing:
    begin
      //GenerateKingPseudoMoves(Candidates, OppositePiecesWithoutKingOrEmpty,
      //  PseudoLegalMoves);
      Attacks := BitBoard.KingMoves[Dest] and Candidates;
      while Attacks > 0 do
      begin
        i := NumberOfTrailingZeroes(Attacks);
        PseudoLegalMoves.Add(CreateMoveFromInt(i, Dest));
        Attacks := Attacks and (Attacks - 1);
      end;
    end;
  end;
  //Write(' 3: ', FormatFloat('0.##', (ET.Elapsed - d) * 1000000), 'µs ', MovingPiece);
  {$IFDEF Logging}
  e := ET.Elapsed;
  {$ENDIF}
  if PseudoLegalMoves.Count > 0 then
  begin
    FoundMoves := TMoveList.Create(False);
    for Move in PseudoLegalMoves do
      if Move.Dest = Dest then
        FoundMoves.Add(Move);
    // Save inital values
    BEnPassant := FEnPassant;
    BPliesSinceLastPawnMoveOrCapture := FPliesSinceLastPawnMoveOrCapture;
    BCastlingAbility := FCastlingAbility;
    BMoveNumber := FMoveNumber;
    for i := 1 to 8 do
      BackupBoards[i] := FBitBoards[i];
    j := 0;
    //Write(' 4: ', FormatFloat('0.##', (ET.Elapsed - e) * 1000000), 'µs');
    while j < FoundMoves.Count do
    begin
      SilentPlayMove(FoundMoves.Items[j]);
      {$IFDEF Logging}
      f := ET.Elapsed;
      {$ENDIF}
      if not IsIllegalCheck then
        Inc(j)
      else
        FoundMoves.Delete(j);
      //Write(' 5: ', FormatFloat('0.##', (ET.Elapsed - f) * 1000000), 'µs');
      // Restore inital values
      FEnPassant := BEnPassant;
      FPliesSinceLastPawnMoveOrCapture := BPliesSinceLastPawnMoveOrCapture;
      FCastlingAbility := BCastlingAbility;
      FMoveNumber := BMoveNumber;
      FWhitesTurn := not FWhitesTurn;
      for i := 1 to 8 do
        FBitBoards[i] := BackupBoards[i];
    end;
    Result := FoundMoves.Count = 1;
    if Result then
      ResultMove := FoundMoves.Items[0].Copy;
    FoundMoves.Free;
  end
  else
    Result := False;
  PseudoLegalMoves.Free;
  {$IFDEF Logging}
  //WriteLn;
  Inc(Zuege);
  Zeit := Zeit + (ET.Elapsed - a);
  {$ENDIF}
end;

function TStandardPosition.ValidateMove(AMove: TMove): boolean;
var
  Dummy: TMove;
begin
  Result := ValidateMove(Dummy, Squares[AMove.Start], AMove.Dest,
    AMove.Start mod 8 + 1, 8 - AMove.Start div 8);
end;

function TStandardPosition.ToFEN: string;
var
  i, z, j: integer;
begin
  Result := '';
  z := 0;
  // Piece placement
  for i := 0 to 63 do
  begin
    if Squares[i] = ptEmpty then
      Inc(z)
    else
    begin
      if z > 0 then
        Result := Result + IntToStr(z);
      case Squares[i] of
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
    if i mod 8 = 7 then
    begin
      if z > 0 then
        Result := Result + IntToStr(z);
      z := 0;
      if i < 63 then
        Result := Result + '/';
    end;
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
