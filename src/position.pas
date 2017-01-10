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
  Classes, SysUtils, fgl, RegExpr, ArrayTools, MoveList, Pieces, StrTools,
  BitBoard
  {$IFDEF Logging} , EpikTimer {$ENDIF}  ;

{$INCLUDE ChessPieceLetters.inc}

type
  // Bitboard stuff is based on https://www.youtube.com/playlist?list=PLQV5mozTHmacMeRzJCW_8K3qw2miYqd0c

  TCastlingType = (ctWKingside, ctWQueenside, ctBKingside, ctBQueenside);

  TCastlingAbility = set of TCastlingType;

  // The following allows more control over the output of the function MoveToSAN
  // Based on https://en.wikipedia.org/wiki/Algebraic_notation_%28chess%29

  // Example: csNone: Be5, csColon: B:e5, csColonSuffix: Be5:, csx: Bxe5
  TCaptureSymbol = (csNone, csColon, csColonSuffix, csx);
  // Example: psNone: e8Q, psEqualSign: e8=Q, psBrackets: e8(Q), psSlash: e8/Q
  TPromotionSymbol = (psNone, psEqualSign, psBrackets, psSlash);

  EInvalidFEN = class(Exception);

  { TPosition }

  TPosition = class
  protected
    FBlackWins: TNotifyEvent;
    FDraw: TNotifyEvent;
    FMoveNumber: integer;
    FWhitesTurn: boolean;
    FWhiteWins: TNotifyEvent;
    procedure BlackWins;
    procedure Draw;
    function GetCountOfFiles: byte; virtual; abstract;
    function GetCountOfRanks: byte; virtual; abstract;
    function GetSquares(Index: integer): TPieceType; virtual; abstract;
    procedure WhiteWins;
  public
    // Copies important values from Source to Self
    procedure Copy(Source: TPosition); virtual;
    function GetAllLegalMoves: TMoveList; virtual; abstract;
    procedure PlayMove(AMove: TMove); virtual; abstract;
    procedure SetupInitialPosition; virtual; abstract;
    function ValidateMove(AMove: TMove): boolean; virtual; abstract;
  public
    property CountOfFiles: byte read GetCountOfFiles;
    property CountOfRanks: byte read GetCountOfRanks;
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
    FCastlingAbility: TCastlingAbility;
    FOnChange: TNotifyEvent;
    FPliesSinceLastPawnMoveOrCapture: integer; // Important for 50 move rule
    // BitBoards
    FEnPassant: TBitBoard;
    // 1. Pawns 2. Rooks 3. Knights 4. Bishops 5. Queens 6. Kings 7. White 8. Black
    FBitBoards: array[1..8] of TBitBoard;

    procedure Changed;
    function AntiDiagonalAttacks(index: integer; Occupied: TBitBoard): TBitBoard;
    function DiagonalAttacks(index: integer; Occupied: TBitBoard): TBitBoard;
    function HorizontalAttacks(Index: integer; Occupied: TBitBoard): TBitBoard;
    function VerticalAttacks(Index: integer; Occupied: TBitBoard): TBitBoard;
    function DiagonalAndAntiDiagonalAttacks(Index: integer;
      Occupied: TBitBoard): TBitBoard;
    function HorizontalAndVerticalAttacks(Index: integer;
      Occupied: TBitBoard): TBitBoard;
    function PawnAttacks(Pawns, Occupied: TBitBoard;
      WhitePawns: boolean): TBitBoard;
    function PawnForwards(Pawns, Occupied: TBitBoard;
      WhitePawns: boolean): TBitBoard;
    function PawnForwardTwo(Pawns, Occupied, StartRank: TBitBoard;
      WhitePawns: boolean): TBitBoard;
    function CanEvadeCheck: boolean;
    function GenerateLegalMovesToSquare(MovingPiece: TPieceType;
      Dest: byte): TMoveList;
    function GetAttackerPosition(Index: byte; PossibleAttackers: TPieceTypes): TBitBoard;
    procedure FilterMoveList(AMoveList: TMoveList; APiece: TPieceType = ptEmpty;
      StartFile: byte = 0; StartRank: byte = 0; DestSquare: byte = 255;
      APromotionPiece: TPieceType = ptEmpty);
    function GetPinnedPieces: TBitBoard;
    // Checks if the side not to move is attacking the given square
    function IsAttacked(Index: integer; ByWhite: boolean): boolean;
    function MayMove(Start, Dest: byte; Occupied: TBitBoard): boolean;
    procedure SilentFromFEN(const AFEN: string);
    // Plays the move without triggering Changed
    procedure SilentPlayMove(AMove: TMove);
    function ValidateCastling(Castling: TCastlingType): boolean;
  protected
    function GetCountOfFiles: byte; override;
    function GetCountOfRanks: byte; override;
    function GetSquares(Index: integer): TPieceType; override;
  public
  const
    InitialFEN = 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1';

    // Just for debugging
    procedure PrintBoards;
    constructor Create(AFEN: string); overload;
    procedure Copy(Source: TPosition); override;
    procedure FromFEN(const AFEN: string);
    function GetAllLegalMoves: TMoveList; override;
    // Checks if the side to move is check
    function IsCheck: boolean;
    // Checks if the side not to move is in check
    function IsIllegalCheck: boolean;
    function IsMate: boolean;
    function IsStaleMate: boolean;
    function IsValid: boolean;
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
    function ValidateMove(AMove: TMove): boolean; override;
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
function SquareToInt(const ASquare: TSquare8x8): integer;

implementation

function SquareToBitBoard(const ASquare: TSquare8x8): QWord;
begin
  Result := Ranks[ASquare.RRank] and Files[ASquare.RFile];
end;

function SquareToInt(const ASquare: TSquare8x8): integer;
begin
  Result := (8 - ASquare.RRank) * 8 + ASquare.RFile - 1;
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
begin
  FMoveNumber := Source.FMoveNumber;
  FWhitesTurn := Source.FWhitesTurn;
end;

{ TStandardPosition }

procedure TStandardPosition.Changed;
begin
  //GenerateAttackMaps;
  // GenerateLegalMoves;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TStandardPosition.AntiDiagonalAttacks(index: integer;
  Occupied: TBitBoard): TBitBoard;
var
  Temp: QWord;
  CurrentAntiDiag: TBitBoard;
begin
  Temp := QWord(1) shl Index;
  CurrentAntiDiag := AntiDiagonals[(Index div 8) - (Index mod 8) + 8];
  Result := ((Occupied and CurrentAntiDiag) - (2 * Temp)) xor
    ReverseBitBoard(ReverseBitBoard(Occupied and CurrentAntiDiag) -
    (2 * ReverseBitBoard(Temp)));
  Result := Result and CurrentAntiDiag;
end;

function TStandardPosition.DiagonalAttacks(index: integer;
  Occupied: TBitBoard): TBitBoard;
var
  Temp: QWord;
  CurrentDiag: TBitBoard;
begin
  Temp := QWord(1) shl Index;
  CurrentDiag := Diagonals[(Index div 8) + (Index mod 8) + 1];
  Result := ((Occupied and CurrentDiag) - 2 * Temp) xor
    ReverseBitBoard(ReverseBitBoard(Occupied and CurrentDiag) - 2 *
    ReverseBitBoard(Temp));
  Result := Result and CurrentDiag;
end;

function TStandardPosition.HorizontalAttacks(Index: integer;
  Occupied: TBitBoard): TBitBoard;
var
  Temp: QWord;
begin
  Temp := QWord(1) shl Index;
  Result := (Occupied - 2 * Temp) xor ReverseBitBoard(ReverseBitBoard(Occupied) -
    2 * ReverseBitBoard(Temp));
  Result := Result and Ranks[8 - (Index div 8)];
end;

function TStandardPosition.VerticalAttacks(Index: integer;
  Occupied: TBitBoard): TBitBoard;
var
  Temp: QWord;
  CurrentFile: TBitBoard;
begin
  Temp := QWord(1) shl Index;
  CurrentFile := Files[(Index mod 8) + 1];
  Result := ((Occupied and CurrentFile) - (2 * Temp)) xor
    ReverseBitBoard(ReverseBitBoard(Occupied and CurrentFile) -
    (2 * ReverseBitBoard(Temp)));
  Result := Result and CurrentFile;
end;

function TStandardPosition.DiagonalAndAntiDiagonalAttacks(Index: integer;
  Occupied: TBitBoard): TBitBoard;
begin
  Result := AntiDiagonalAttacks(Index, Occupied) or DiagonalAttacks(Index, Occupied);
end;

function TStandardPosition.HorizontalAndVerticalAttacks(Index: integer;
  Occupied: TBitBoard): TBitBoard;
begin
  Result := HorizontalAttacks(Index, Occupied) or VerticalAttacks(Index, Occupied);
end;

function TStandardPosition.PawnAttacks(Pawns, Occupied: TBitBoard;
  WhitePawns: boolean): TBitBoard;
begin
  if WhitePawns then
  begin
    Result := ((Pawns and not Files[8]) shr 7) or ((Pawns and not Files[1]) shr 9);
  end
  else
  begin
    Result := ((Pawns and not Files[8]) shl 9) or ((Pawns and not Files[1]) shl 7);
  end;
  Result := Result and Occupied;
end;

function TStandardPosition.PawnForwards(Pawns, Occupied: TBitBoard;
  WhitePawns: boolean): TBitBoard;
var
  Empty: TBitBoard;
begin
  Empty := not Occupied;
  if WhitePawns then
  begin
    Result := (Pawns shr 8) and Empty;
  end
  else
  begin
    Result := (Pawns shl 8) and Empty;
  end;
end;

function TStandardPosition.PawnForwardTwo(Pawns, Occupied, StartRank: TBitBoard;
  WhitePawns: boolean): TBitBoard;
var
  Empty: TBitBoard;
begin
  Empty := not Occupied;
  if WhitePawns then
  begin
    Result := ((Pawns and StartRank) shr 16) and (Empty shr 8) and Empty;
  end
  else
  begin
    Result := ((Pawns and StartRank) shl 16) and (Empty shl 8) and Empty;
  end;
end;

function TStandardPosition.CanEvadeCheck: boolean;
var
  KingPos, Index: byte;
  OwnPieces, OppPieces: TPieceTypes;
  Attackers, Moves, OwnPiecePos, Temp, PinnedPieces, King: TBitBoard;
begin
  Result := False;
  if FWhitesTurn then
  begin
    King := FBitBoards[6] and FBitBoards[7];
    KingPos := NumberOfTrailingZeroes(King);
    OwnPieces := WhitePieces;
    OppPieces := BlackPieces;
    OwnPiecePos := FBitBoards[7];
  end
  else
  begin
    King := FBitBoards[6] and FBitBoards[8];
    KingPos := NumberOfTrailingZeroes(King);
    OwnPieces := BlackPieces;
    OppPieces := WhitePieces;
    OwnPiecePos := FBitBoards[8];
  end;
  Attackers := GetAttackerPosition(KingPos, OppPieces);
  if Attackers > 0 then // check if we are really in check
  begin
    // First look if the king can move
    Moves := KingMoves[KingPos] and not OwnPiecePos;
    Temp := Moves;
    while Temp > 0 do
    begin
      if not IsAttacked(NumberOfTrailingZeroes(Temp), not FWhitesTurn) then
        Exit(True);
      Temp := Temp and (Temp - 1);
    end;
    if PopCnt(Attackers) = 1 then
    begin
      // we are not in double check, so we first try to capture the attacker
      Index := NumberOfTrailingZeroes(Attackers);
      PinnedPieces := GetPinnedPieces;
      if GetAttackerPosition(Index, OwnPieces) and not (PinnedPieces or King) > 0 then
        Exit(True);
      if (BasicPieceType(Squares[Index]) <> bptKnight) and (Attackers and Moves = 0) then
      begin
        // Attacker is not a knight or next to the king, so we can try to block the check
        Temp := InBetweens[KingPos, Index];
        while Temp > 0 do
        begin
          if GetAttackerPosition(NumberOfTrailingZeroes(Temp), OwnPieces) and
            not (PinnedPieces or King) > 0 then
            Exit(True);
          Temp := Temp and (Temp - 1);
        end;
      end;
    end;
  end
  else
    Result := True;
end;

function TStandardPosition.GenerateLegalMovesToSquare(MovingPiece: TPieceType;
  Dest: byte): TMoveList;
var
  Starts, Occupied, Occ, PawnMoves: TBitBoard;
  i: integer;
  Clone: TStandardPosition;
  Rank: byte;
begin
  Result := TMoveList.Create(True);
  Occupied := FBitBoards[7] or FBitBoards[8];
  if BasicPieceType(MovingPiece) = bptPawn then
  begin
    Rank := 8 - (Dest div 8);
    if IsWhite(MovingPiece) then
    begin
      Occ := Occupied and not (FBitBoards[1] and Ranks[Rank - 1]);
      if (QWord(1) shl Dest) and Occupied = 0 then // if Dest is empty
      begin
        PawnMoves := PawnForwards(QWord(1) shl Dest, Occ, False);
        if Rank = 4 then // Check for double moves
        begin
          Occ := Occupied and not (FBitBoards[1] and Ranks[2]);
          PawnMoves := PawnMoves or PawnForwardTwo(QWord(1) shl Dest,
            Occ, Ranks[4], False);
        end;
      end
      else
        PawnMoves := 0;
      if ((QWord(1) shl Dest) and (Occupied or FEnPassant)) > 0 then
        // Check for captures
        PawnMoves := PawnMoves or PawnAttacks(QWord(1) shl Dest,
          Occupied or FEnPassant, False);
      Starts := FBitBoards[1] and FBitBoards[7] and PawnMoves;
    end
    else
    begin
      Occ := Occupied and not (FBitBoards[1] and Ranks[Rank + 1]);
      if (QWord(1) shl Dest) and Occupied = 0 then // if Dest is empty
      begin
        PawnMoves := PawnForwards(QWord(1) shl Dest, Occ, True);
        if Rank = 5 then // Check for double moves
        begin
          Occ := Occupied and not (FBitBoards[1] and Ranks[7]);
          PawnMoves := PawnMoves or PawnForwardTwo(QWord(1) shl Dest,
            Occ, Ranks[5], True);
        end;
      end
      else
        PawnMoves := 0;
      if ((QWord(1) shl Dest) and (Occupied or FEnPassant)) > 0 then
        // Check for captures
        PawnMoves := PawnMoves or PawnAttacks(QWord(1) shl Dest,
          Occupied or FEnPassant, True);
      Starts := FBitBoards[1] and FBitBoards[8] and PawnMoves;
    end;
  end
  else
    Starts := GetAttackerPosition(Dest, [MovingPiece]);
  while Starts > 0 do
  begin
    Result.Add(CreateMoveFromInt(NumberOfTrailingZeroes(Starts), Dest));
    Starts := Starts and (Starts - 1);
  end;
  // Check moves for legality
  Clone := TStandardPosition.Create;
  i := 0;
  while i < Result.Count do
  begin
    Clone.Copy(Self);
    Clone.SilentPlayMove(Result.Items[i]);
    if Clone.IsIllegalCheck then
      Result.Delete(i)
    else
      Inc(i);
  end;
  Clone.Free;
end;

function TStandardPosition.GetAttackerPosition(Index: byte;
  PossibleAttackers: TPieceTypes): TBitBoard;
var
  Occupied, Pieces, Starts: TBitBoard;
  MovingPiece: TPieceType;
begin
  Result := 0;
  Occupied := FBitBoards[7] or FBitBoards[8];
  for MovingPiece in PossibleAttackers do
  begin
    case MovingPiece of
      ptWPawn: Pieces := FBitBoards[1] and FBitBoards[7];
      ptWRook: Pieces := FBitBoards[2] and FBitBoards[7];
      ptWKnight: Pieces := FBitBoards[3] and FBitBoards[7];
      ptWBishop: Pieces := FBitBoards[4] and FBitBoards[7];
      ptWQueen: Pieces := FBitBoards[5] and FBitBoards[7];
      ptWKing: Pieces := FBitBoards[6] and FBitBoards[7];
      ptBPawn: Pieces := FBitBoards[1] and FBitBoards[8];
      ptBRook: Pieces := FBitBoards[2] and FBitBoards[8];
      ptBKnight: Pieces := FBitBoards[3] and FBitBoards[8];
      ptBBishop: Pieces := FBitBoards[4] and FBitBoards[8];
      ptBQueen: Pieces := FBitBoards[5] and FBitBoards[8];
      ptBKing: Pieces := FBitBoards[6] and FBitBoards[8];
    end;
    case BasicPieceType(MovingPiece) of
      bptPawn:
      begin
        Starts := PawnAttacks(QWord(1) shl Index, Pieces, not IsWhite(MovingPiece));
      end;
      bptKnight:
      begin
        Starts := Pieces and KnightMoves[Index];
      end;
      bptBishop:
      begin
        Starts := Pieces and DiagonalAndAntiDiagonalAttacks(Index, Occupied);
      end;
      bptRook:
      begin
        Starts := Pieces and HorizontalAndVerticalAttacks(Index, Occupied);
      end;
      bptQueen:
      begin
        Starts := Pieces and (DiagonalAndAntiDiagonalAttacks(Index, Occupied) or
          HorizontalAndVerticalAttacks(Index, Occupied));
      end;
      bptKing:
      begin
        Starts := Pieces and KingMoves[Index];
      end;
    end;
    Result := Result or Starts;
  end;
end;

procedure TStandardPosition.FilterMoveList(AMoveList: TMoveList;
  APiece: TPieceType; StartFile: byte; StartRank: byte; DestSquare: byte;
  APromotionPiece: TPieceType);
var
  NoFilterPiece, NoFilterDest, NoFilterPromo, NoFilterStartR, NoFilterStartF: boolean;
  Move: TMove;
  j: integer;
begin
  NoFilterPiece := APiece = ptEmpty;
  NoFilterDest := DestSquare = 255;
  NoFilterStartF := StartFile = 0;
  NoFilterStartR := StartRank = 0;
  NoFilterPromo := APromotionPiece = ptEmpty;
  j := 0;
  while j < AMoveList.Count do
  begin
    Move := AMoveList.Items[j];
    if (NoFilterPiece or (Squares[Move.Start] = APiece)) and
      (NoFilterDest or (Move.Dest = DestSquare)) and
      (NoFilterStartF or (Move.Start8x8.RFile = StartFile)) and
      (NoFilterStartR or (Move.Start8x8.RRank = StartRank)) and
      (NoFilterPromo or (Move.PromotionPiece = APromotionPiece)) then
      Inc(j)
    else
      AMoveList.Delete(j);
  end;
end;

function TStandardPosition.GetPinnedPieces: TBitBoard;
var
  K, R, B, Blockers, Pinner, SuperKingAttacks, Occupied: TBitBoard;
  i: integer;
begin
  Result := 0;
  // based on Opposite Ray-Directions http://chessprogramming.wikispaces.com/Checks+and+Pinned+Pieces+(Bitboards)
  if FWhitesTurn then
  begin
    K := FBitBoards[6] and FBitBoards[7];
    R := (FBitBoards[2] or FBitBoards[5]) or FBitBoards[8];
    B := (FBitBoards[4] or FBitBoards[5]) or FBitBoards[8];
    Blockers := FBitBoards[7];
  end
  else
  begin
    K := FBitBoards[6] and FBitBoards[8];
    R := (FBitBoards[2] or FBitBoards[5]) or FBitBoards[7];
    B := (FBitBoards[4] or FBitBoards[5]) or FBitBoards[7];
    Blockers := FBitBoards[8];
  end;
  Occupied := FBitBoards[7] and FBitBoards[8];
  // Horizontal pins
  Pinner := R;
  SuperKingAttacks := HorizontalAttacks(NumberOfTrailingZeroes(K), Occupied);
  while Pinner > 0 do
  begin
    i := NumberOfTrailingZeroes(Pinner);
    Result := Result or (HorizontalAttacks(i, Occupied) and
      SuperKingAttacks and Blockers);
    Pinner := Pinner and (Pinner - 1);
  end;
  // Vertical pins
  Pinner := R;
  SuperKingAttacks := VerticalAttacks(NumberOfTrailingZeroes(K), Occupied);
  while Pinner > 0 do
  begin
    i := NumberOfTrailingZeroes(Pinner);
    Result := Result or (VerticalAttacks(i, Occupied) and SuperKingAttacks and Blockers);
    Pinner := Pinner and (Pinner - 1);
  end;
  // Diagonal pins
  Pinner := B;
  SuperKingAttacks := DiagonalAttacks(NumberOfTrailingZeroes(K), Occupied);
  while Pinner > 0 do
  begin
    i := NumberOfTrailingZeroes(Pinner);
    Result := Result or (DiagonalAttacks(i, Occupied) and SuperKingAttacks and Blockers);
    Pinner := Pinner and (Pinner - 1);
  end;
  // Antidiagonal pins
  Pinner := B;
  SuperKingAttacks := AntiDiagonalAttacks(NumberOfTrailingZeroes(K), Occupied);
  while Pinner > 0 do
  begin
    i := NumberOfTrailingZeroes(Pinner);
    Result := Result or (AntiDiagonalAttacks(i, Occupied) and
      SuperKingAttacks and Blockers);
    Pinner := Pinner and (Pinner - 1);
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
  Result := ptEmpty;
  for i := 1 to 6 do
  begin
    if (QWord(1) shl Index) and FBitBoards[i] > 0 then
    begin
      for j := 7 to 8 do
      begin
        if ((QWord(1) shl Index) and FBitBoards[j]) > 0 then
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
end;

function TStandardPosition.IsAttacked(Index: integer; ByWhite: boolean): boolean;
begin
  if ByWhite then
  begin
    Result := GetAttackerPosition(Index, WhitePieces) > 0;
  end
  else
  begin
    Result := GetAttackerPosition(Index, BlackPieces) > 0;
  end;
end;

function TStandardPosition.MayMove(Start, Dest: byte; Occupied: TBitBoard): boolean;
begin
  // https://chessprogramming.wikispaces.com/Square+Attacked+By#Legality%20Test
  Result := (InBetweens[Start, Dest] and Occupied) = 0;
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

  if ((Start and FBitBoards[1]) > 0) or
    ((Dest and FBitBoards[7] and FBitBoards[8]) > 0) then
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

function TStandardPosition.ValidateCastling(Castling: TCastlingType): boolean;
var
  HasToBeEmpty: TBitBoard;
  CantBeAttacked: set of byte;
  Attackers: TPieceTypes;
  i: byte;
begin
  if (Castling in FCastlingAbility) and not IsCheck then
  begin
    case Castling of
      ctWKingside:
      begin
        HasToBeEmpty := CastlingSquares[1];
        CantBeAttacked := [61, 62];
        Attackers := BlackPieces;
      end;
      ctWQueenside:
      begin
        HasToBeEmpty := CastlingSquares[2];
        CantBeAttacked := [58, 59];
        Attackers := BlackPieces;
      end;
      ctBKingside:
      begin
        HasToBeEmpty := CastlingSquares[3];
        CantBeAttacked := [5, 6];
        Attackers := WhitePieces;
      end;
      ctBQueenside:
      begin
        HasToBeEmpty := CastlingSquares[4];
        CantBeAttacked := [3, 4];
        Attackers := WhitePieces;
      end;
    end;
    Result := (FBitBoards[7] and FBitBoards[8] and HasToBeEmpty) = 0;
    for i in CantBeAttacked do
    begin
      Result := Result and (GetAttackerPosition(i, Attackers) = 0);
    end;
  end
  else
    Result := False;
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

constructor TStandardPosition.Create(AFEN: string);
begin
  FromFEN(AFEN);
end;

procedure TStandardPosition.FromFEN(const AFEN: string);
begin
  SilentFromFEN(AFEN);
  Changed;
end;

function TStandardPosition.GetAllLegalMoves: TMoveList;
var
  Temp: TBitBoard;
  i, j: byte;
  Move: TMove;
begin
  Result := TMoveList.Create;
  if WhitesTurn then
    Temp := FBitBoards[7]
  else
    Temp := FBitBoards[8];
  while Temp > 0 do
  begin
    i := NumberOfTrailingZeroes(Temp);
    for j := 0 to 63 do
    begin
      Move := TMove.Create(i, j);
      if ValidateMove(Move) then
      begin
        if FWhitesTurn then
        begin
          // Pawn Promotion
          if (j in Rank8) and (Squares[i] = ptWPawn) then
          begin
            Result.Add(TMove.Create(i, j, ptWQueen));
            Result.Add(TMove.Create(i, j, ptWKnight));
            Result.Add(TMove.Create(i, j, ptWBishop));
            Result.Add(TMove.Create(i, j, ptWRook));
            Move.Free;
          end
          else
            Result.Add(Move);
        end
        else
        begin
          // Pawn Promotion
          if (j in Rank1) and (Squares[i] = ptBPawn) then
          begin
            Result.Add(TMove.Create(i, j, ptBQueen));
            Result.Add(TMove.Create(i, j, ptBKnight));
            Result.Add(TMove.Create(i, j, ptBBishop));
            Result.Add(TMove.Create(i, j, ptBRook));
            Move.Free;
          end
          else
            Result.Add(Move);
        end;
      end
      else
        Move.Free;
    end;
    Temp := Temp and (Temp - 1);
  end;
end;

function TStandardPosition.IsCheck: boolean;
begin
  // Assumes that exact one black and one white king exist
  if FWhitesTurn then
    Result := IsAttacked(NumberOfTrailingZeroes(FBitBoards[6] and FBitBoards[7]), False)
  else
    Result := IsAttacked(NumberOfTrailingZeroes(FBitBoards[6] and FBitBoards[8]), True);
end;

function TStandardPosition.IsIllegalCheck: boolean;
begin
  FWhitesTurn := not FWhitesTurn;
  Result := IsCheck;
  FWhitesTurn := not FWhitesTurn;
end;

function TStandardPosition.IsMate: boolean;
begin
  Result := IsCheck and not CanEvadeCheck;
end;

function TStandardPosition.IsStaleMate: boolean;
var
  Temp: TMoveList;
begin
  Temp := GetAllLegalMoves;
  Result := (Temp.Count = 0) and not IsCheck;
  Temp.Free;
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
      SameDest := GenerateLegalMovesToSquare(Piece, AMove.Dest);
      // Check if there is another piece of the same kind, which can go to the current square
      for j := 0 to SameDest.Count - 1 do
      begin
        // We need to delete the played move from the list
        if AMove.Start8x8 = SameDest.Items[j].Start8x8 then
        begin
          SameDest.Delete(j);
          Break;
        end;
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
      SameDest.Clear;
      SameDest.Free;
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
    if Clone.CanEvadeCheck then
      Result := Result + '+'
    else
      Result := Result + '#';
  end;
  Clone.Free;
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
  FoundMoves: TMoveList;
begin
  // TODO special validation of castling
  case MovingPiece of
    ptWKing: if StartingFile = 5 then // e-file
      begin
        if Dest = 62 then
        begin
          ResultMove := TMove.Create(60, 62);
          Exit(ValidateCastling(ctWKingside));
        end
        else
        if Dest = 58 then
        begin
          ResultMove := TMove.Create(60, 58);
          Exit(ValidateCastling(ctWQueenside));
        end;
      end;
    ptBKing: if StartingFile = 5 then // e-file
      begin
        if Dest = 6 then
        begin
          ResultMove := TMove.Create(4, 6);
          Exit(ValidateCastling(ctBKingside));
        end
        else
        if Dest = 2 then
        begin
          ResultMove := TMove.Create(4, 2);
          Exit(ValidateCastling(ctBQueenside));
        end;
      end;
  end;
  // Get all possible moves to Dest
    FoundMoves := GenerateLegalMovesToSquare(MovingPiece, Dest);
  // We need to test the found moves with more information
  if FoundMoves.Count > 1 then
    FilterMoveList(FoundMoves, ptEmpty, StartingFile, StartingRank);
  Result := FoundMoves.Count = 1;
  if Result then
    ResultMove := FoundMoves.Items[0].Copy;
  FoundMoves.Free;
end;

function TStandardPosition.ValidateMove(AMove: TMove): boolean;
var
  Dummy: TMove;
begin
  if (IsWhite(Squares[AMove.Start]) <> FWhitesTurn) or
    (Squares[AMove.Start] in [ptEmpty, ptOff]) then
    Exit(False);
  if Squares[AMove.Dest] <> ptEmpty then
    if IsWhite(Squares[AMove.Start]) = IsWhite(Squares[AMove.Dest]) then
      Exit(False);
  Dummy := nil;
  Result := ValidateMove(Dummy, Squares[AMove.Start], AMove.Dest,
    AMove.Start mod 8 + 1, 8 - AMove.Start div 8);
  if Dummy <> nil then
  begin
    Result := Result and Dummy.Equals(AMove);
    Dummy.Free;
  end;
end;

function TStandardPosition.ToFEN: string;
var
  i, z: integer;
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
