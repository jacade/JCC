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

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RegExpr, ArrayTools, MoveList, Pieces, StrTools;

{$INCLUDE ChessPieceLetters.inc}

type
  // Constants are taken from http://chessprogramming.wikispaces.com/10x12+Board
  // Main resource is https://de.wikipedia.org/wiki/Schachprogramm#12.C3.9710-Darstellung

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
    function IsLegal(AMove: TMove): boolean; virtual;
    procedure PlayMove(AMove: TMove); virtual; abstract;
    procedure SetupInitialPosition; virtual; abstract;
  public
    property CountOfFiles: byte read GetCountOfFiles;
    property CountOfRanks: byte read GetCountOfRanks;
    property LegalMoves: TMoveList read FLegalMoves;
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
    FEnPassant: TSquare10x12;
    FMoveNumber: integer;
    FOnChange: TNotifyEvent;
    FPliesSinceLastPawnMoveOrCapture: integer; // Important for 50 move rule
    FSquares: array[0..119] of TPieceType;

    procedure Changed;
    procedure Copy(Source: TStandardPosition);
    function GenerateBishopMoves(Start: TSquare10x12): TMoveList;
    function GenerateCastlingMoves(Start: TSquare10x12): TMoveList;
    function GenerateKingMoves(Start: TSquare10x12): TMoveList;
    function GenerateKnightMoves(Start: TSquare10x12): TMoveList;
    // This generates only capture moves
    function GeneratePawnCaptureMoves(Start: TSquare10x12): TMoveList;
    // This generates only forward moves
    function GeneratePawnForwardMoves(Start: TSquare10x12): TMoveList;
    // The moves in AMoveList are replaced by promoting moves.
    procedure GeneratePawnPromotionMoves(AMoveList: TMoveList);
    function GenerateQueenMoves(Start: TSquare10x12): TMoveList;
    function GenerateRookMoves(Start: TSquare10x12): TMoveList;
    // Checks if the side not to move is attacking the given square
    function IsAttacked(Square: TSquare10x12): boolean;
    // Plays the move without triggering Changed
    procedure SilentPlayMove(AMove: TMove);
  protected
    procedure GenerateLegalMoves; override;
    function GetCountOfFiles: byte; override;
    function GetCountOfRanks: byte; override;
    function GetSquares(Index: integer): TPieceType; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure FromFEN(AFEN: string);
    // Checks if the side to move is check
    function IsCheck: boolean;
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
    property EnPassant: TSquare10x12 read FEnPassant write FEnPassant;
    property MoveNumber: integer read FMoveNumber write FMoveNumber;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property PliesSinceLastPawnMoveOrCapture: integer
      read FPliesSinceLastPawnMoveOrCapture write FPliesSinceLastPawnMoveOrCapture;
  end;


implementation

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

function TPosition.IsLegal(AMove: TMove): boolean;
begin
  Result := AMove in LegalMoves;
end;

{ TStandardPosition }

procedure TStandardPosition.Changed;
begin
  GenerateLegalMoves;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TStandardPosition.Copy(Source: TStandardPosition);
var
  i: integer;
begin
  FCastlingAbility := Source.FCastlingAbility;
  FEnPassant := Source.FEnPassant;
  FMoveNumber := Source.FMoveNumber;
  FPliesSinceLastPawnMoveOrCapture := Source.FPliesSinceLastPawnMoveOrCapture;
  for i := 0 to 119 do
    FSquares[i] := Source.FSquares[i];
  FWhitesTurn := Source.FWhitesTurn;
end;

function TStandardPosition.GenerateBishopMoves(Start: TSquare10x12): TMoveList;
var
  i, j, Dest: integer;
  Sign: integer;
  Flag: boolean;
begin
  Result := TMoveList.Create;
  for j := 1 to 2 do
  begin
    if j = 1 then
      Sign := 1
    else
      Sign := -1;
    for i in DiagonalMoves do
    begin
      Dest := Start + Sign * i;
      Flag := False;
      while (FSquares[Dest] <> ptOff) and not Flag do
      begin
        if (FSquares[Dest] = ptEmpty) then
        begin
          Result.Add(TMove.Create(Start, Dest));
          Dest := Dest + Sign * i;
        end
        else
        begin
          if not SameColor(FSquares[Start], FSquares[Dest]) then
            Result.Add(TMove.Create(Start, Dest));
          Flag := True;
        end;
      end;
    end;
  end;
end;

function TStandardPosition.GenerateCastlingMoves(Start: TSquare10x12): TMoveList;
begin
  Result := TMoveList.Create;
  // Check Castlings
  if FWhitesTurn then
  begin
    if (ctWKingside in FCastlingAbility) and not IsAttacked(Start) and
      (FSquares[96] = ptEmpty) and (FSquares[97] = ptEmpty) and not
      IsAttacked(96) and not IsAttacked(97) then
      Result.Add(TMove.Create(Start, 97));
    if (ctWQueenside in FCastlingAbility) and not IsAttacked(Start) and
      (Fsquares[94] = ptEmpty) and (FSquares[93] = ptEmpty) and
      (FSquares[92] = ptEmpty) and not IsAttacked(94) and not IsAttacked(93) then
      Result.Add(TMove.Create(Start, 93));
  end
  else
  begin
    if (ctBKingside in FCastlingAbility) and not IsAttacked(Start) and
      (FSquares[26] = ptEmpty) and (FSquares[27] = ptEmpty) and not
      IsAttacked(26) and not IsAttacked(27) then
      Result.Add(TMove.Create(Start, 27));
    if (ctBQueenside in FCastlingAbility) and not IsAttacked(Start) and
      (Fsquares[24] = ptEmpty) and (FSquares[23] = ptEmpty) and
      (FSquares[22] = ptEmpty) and not IsAttacked(24) and not IsAttacked(23) then
      Result.Add(TMove.Create(Start, 23));
  end;
end;

function TStandardPosition.GenerateKingMoves(Start: TSquare10x12): TMoveList;
var
  i, j, Sign, Dest: integer;
begin
  Result := TMoveList.Create;
  for j := 1 to 2 do
  begin
    if j = 1 then
      Sign := 1
    else
      Sign := -1;
    for i in (HorzVertMoves + DiagonalMoves) do
    begin
      Dest := Start + Sign * i;
      if (FSquares[Dest] <> ptOff) and ((FSquares[Dest] = ptEmpty) or
        not SameColor(FSquares[Start], FSquares[Dest])) then
        Result.Add(TMove.Create(Start, Dest));
    end;
  end;
end;

function TStandardPosition.GenerateKnightMoves(Start: TSquare10x12): TMoveList;
var
  i, j, Sign, Dest: integer;
begin
  Result := TMoveList.Create;
  for j := 1 to 2 do
  begin
    if j = 1 then
      Sign := 1
    else
      Sign := -1;
    for i in KnightMoves do
    begin
      Dest := Start + Sign * i;
      if (FSquares[Dest] <> ptOff) and ((FSquares[Dest] = ptEmpty) or
        not SameColor(FSquares[Start], FSquares[Dest])) then
        Result.Add(TMove.Create(Start, Dest));
    end;
  end;
end;

procedure TStandardPosition.GenerateLegalMoves;
var
  i: byte;
  temp, t: TMoveList;
  Clone: TStandardPosition;
  j: integer;
begin
  FLegalMoves.Clear;
  for i in ValidSquares do
  begin
    // First check if there is a piece with the right color
    if (FSquares[i] = ptEmpty) or not (((Ord(FSquares[i]) and 128) = 0) =
      FWhitesTurn) then
      Continue;
    // Then, generate pseudo-legal moves
    case FSquares[i] of
      ptWPawn, ptBPawn:
      begin
        temp := GeneratePawnCaptureMoves(i);
        t := GeneratePawnForwardMoves(i);
        temp.AddList(t);
        t.Free;
      end;
      ptWKnight, ptBKnight: temp := GenerateKnightMoves(i);
      ptWBishop, ptBBishop: temp := GenerateBishopMoves(i);
      ptWRook, ptBRook: temp := GenerateRookMoves(i);
      ptWQueen, ptBQueen: temp := GenerateQueenMoves(i);
      ptWKing, ptBKing:
      begin
        temp := GenerateKingMoves(i);
        t := GenerateCastlingMoves(i);
        temp.AddList(t);
        t.Free;
      end;
    end;
    // Copy Position, Play Move, Position Valid?
    Clone := TStandardPosition.Create;
    j := 0;
    while j < temp.Count do
    begin
      Clone.Copy(Self);
      Clone.SilentPlayMove(temp.Items[j]);
      if Clone.IsValid then
        Inc(j)
      else
      begin
        temp.Delete(j);
      end;
    end;
    FreeAndNil(Clone);
    FLegalMoves.AddList(temp);
    temp.Free;
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
begin
  Result := FSquares[Index];
end;

function TStandardPosition.GeneratePawnCaptureMoves(Start: TSquare10x12): TMoveList;
var
  i, Sign, Dest: integer;
begin
  Result := TMoveList.Create;
  if FWhitesTurn then
    Sign := -1
  else
    Sign := 1;
  for i in [9, 11] do
  begin
    Dest := Start + Sign * i;
    if not (FSquares[Dest] in [ptOff, ptEmpty]) and not
      SameColor(FSquares[Start], FSquares[Dest]) then
      Result.Add(TMove.Create(Start, Dest));
  end;
  // En Passant
  if FEnPassant in [Start + Sign * 9, Start + Sign * 11] then
    Result.Add(TMove.Create(Start, FEnPassant));
  // Check for Promotion
  if (FWhitesTurn and (Start in Rank7)) or (not FWhitesTurn and (Start in Rank2)) then
    GeneratePawnPromotionMoves(Result);
end;

function TStandardPosition.GeneratePawnForwardMoves(Start: TSquare10x12): TMoveList;
var
  Sign: integer;
begin
  Result := TMoveList.Create;
  if FWhitesTurn then
    Sign := -1
  else
    Sign := 1;
  if FSquares[Start + Sign * 10] = ptEmpty then
  begin
    Result.Add(TMove.Create(Start, Start + Sign * 10));
    // Pawn can go two?
    if ((FWhitesTurn and (Start in Rank2)) or (not FWhitesTurn and
      (Start in Rank7))) and (FSquares[Start + Sign * 20] = ptEmpty) then
      Result.Add(TMove.Create(Start, Start + Sign * 20));
  end;
  // Check for Promotion
  if (FWhitesTurn and (Start in Rank7)) or (not FWhitesTurn and (Start in Rank2)) then
    GeneratePawnPromotionMoves(Result);
end;

procedure TStandardPosition.GeneratePawnPromotionMoves(AMoveList: TMoveList);
var
  temp: TMoveList;
  Move: TMove;
  Piece: TPieceType;
begin
  temp := TMoveList.Create;
  for Move in AMoveList do
  begin
    if FWhitesTurn then
    begin
      for Piece in [ptWRook, ptWKnight, ptWBishop, ptWQueen] do
        temp.Add(TMove.Create(Move.Start, Move.Dest, Piece));
    end
    else
    begin
      for Piece in [ptBRook, ptBKnight, ptBBishop, ptBQueen] do
        temp.Add(TMove.Create(Move.Start, Move.Dest, Piece));
    end;
  end;
  AMoveList.Clear;
  AMoveList.AddList(temp);
  FreeAndNil(temp);
end;

function TStandardPosition.GenerateQueenMoves(Start: TSquare10x12): TMoveList;
var
  i, j, Dest, Sign: integer;
  Flag: boolean;
begin
  Result := TMoveList.Create;
  for j := 1 to 2 do
  begin
    if j = 1 then
      Sign := 1
    else
      Sign := -1;
    for i in (HorzVertMoves + DiagonalMoves) do
    begin
      Dest := Start + Sign * i;
      Flag := False;
      while (FSquares[Dest] <> ptOff) and not Flag do
      begin
        if (FSquares[Dest] = ptEmpty) then
        begin
          Result.Add(TMove.Create(Start, Dest));
          Dest := Dest + Sign * i;
        end
        else
        begin
          if not SameColor(FSquares[Start], FSquares[Dest]) then
            Result.Add(TMove.Create(Start, Dest));
          Flag := True;
        end;
      end;
    end;
  end;
end;

function TStandardPosition.GenerateRookMoves(Start: TSquare10x12): TMoveList;
var
  i, j, Dest, Sign: integer;
  Flag: boolean;
begin
  Result := TMoveList.Create;
  for j := 1 to 2 do
  begin
    if j = 1 then
      Sign := 1
    else
      Sign := -1;
    for i in HorzVertMoves do
    begin
      Dest := Start + Sign * i;
      Flag := False;
      while (FSquares[Dest] <> ptOff) and not Flag do
      begin
        if (FSquares[Dest] = ptEmpty) then
        begin
          Result.Add(TMove.Create(Start, Dest));
          Dest := Dest + Sign * i;
        end
        else
        begin
          if not SameColor(FSquares[Start], FSquares[Dest]) then
            Result.Add(TMove.Create(Start, Dest));
          Flag := True;
        end;
      end;
    end;
  end;
end;

function TStandardPosition.IsAttacked(Square: TSquare10x12): boolean;
var
  temp: TMoveList;
  Move: TMove;
  i: integer;
  Attackers: set of TPieceType;
begin
  Result := False;
  for i := 1 to 5 do
  begin
    case i of
      1:
      begin
        temp := GenerateBishopMoves(Square);
        if FWhitesTurn then
          Attackers := [ptBBishop, ptBQueen]
        else
          Attackers := [ptWBishop, ptWQueen];
      end;
      2:
      begin
        temp := GenerateKnightMoves(Square);
        if FWhitesTurn then
          Attackers := [ptBKnight]
        else
          Attackers := [ptWKnight];
      end;
      3:
      begin
        temp := GenerateKingMoves(Square);
        if FWhitesTurn then
          Attackers := [ptBKing]
        else
          Attackers := [ptWKing];
      end;
      4:
      begin
        temp := GenerateRookMoves(Square);
        if FWhitesTurn then
          Attackers := [ptBRook, ptBQueen]
        else
          Attackers := [ptWRook, ptWQueen];
      end;
      5:
      begin
        temp := GeneratePawnCaptureMoves(Square);
        if FWhitesTurn then
          Attackers := [ptBPawn]
        else
          Attackers := [ptWPawn];
      end;
    end;
    for Move in temp do
      Result := Result or (FSquares[TSquare10x12(Move.Dest)] in Attackers);
    temp.Free;
  end;
end;

procedure TStandardPosition.SilentPlayMove(AMove: TMove);
var
  Start, Dest: TSquare10x12;
  CastlingRook: TPieceType;
begin
  Start := AMove.Start;
  Dest := AMove.Dest;
  if FSquares[Start] in [ptEmpty, ptOff] then
    raise Exception.Create('Invalid Start square given!');

  if (FSquares[Start] in [ptWPawn, ptBPawn]) or (FSquares[Dest] <> ptEmpty) then
    FPliesSinceLastPawnMoveOrCapture := 0
  else
    Inc(FPliesSinceLastPawnMoveOrCapture);

  if FSquares[Start] in [ptWBishop, ptWKnight, ptWRook, ptWQueen,
    ptBBishop, ptBKnight, ptBRook, ptBQueen] then
  begin
    FSquares[Dest] := FSquares[Start];
    FSquares[Start] := ptEmpty;
    FEnPassant := 0;
  end
  else
  if FSquares[Start] in [ptWPawn, ptBPawn] then
  begin
    // Promotion
    if (Dest in Rank1) or (Dest in Rank8) then
    begin
      FSquares[Dest] := AMove.PromotionPiece;
      FSquares[Start] := ptEmpty;
    end
    else
    // En Passant
    if Dest = FEnPassant then
    begin
      FSquares[Dest] := FSquares[Start];
      FSquares[Start] := ptEmpty;
      if Dest in Rank3 then
        FSquares[Dest - 10] := ptEmpty
      else
        FSquares[Dest + 10] := ptEmpty;
    end
    else
      // Normal move
    begin
      FSquares[Dest] := FSquares[Start];
      FSquares[Start] := ptEmpty;
      // Set FEnPassant accordingly
      if ((Dest > Start) and (Dest - Start = 20)) then
        FEnPassant := Start + 10
      else
      if ((Start > Dest) and (Start - Dest = 20)) then
        FEnPassant := Start - 10
      else
        FEnPassant := 0;
    end;
  end
  else
  begin
    // King moves
    if FSquares[Start] in WhitePieces then
      CastlingRook := ptWRook
    else
      CastlingRook := ptBRook;
    // Kingside castling
    if ((Dest > Start) and (Dest - Start = 2)) then
    begin
      FSquares[Start + 1] := CastlingRook;
      FSquares[Dest] := FSquares[Start];
      FSquares[Start] := ptEmpty;
      FSquares[Dest + 1] := ptEmpty;
    end
    else
    // Queenside castling
    if ((Dest < Start) and (Start - Dest = 2)) then
    begin
      FSquares[Start - 1] := CastlingRook;
      FSquares[Dest] := FSquares[Start];
      FSquares[Start] := ptEmpty;
      FSquares[Dest - 2] := ptEmpty;
    end
    else
    begin
      FSquares[Dest] := FSquares[Start];
      FSquares[Start] := ptEmpty;
    end;
  end;
  if (ctWKingside in FCastlingAbility) and ((FSquares[95] <> ptWKing) or
    (FSquares[98] <> ptWRook)) then
    FCastlingAbility := FCastlingAbility - [ctWKingside];
  if (ctWQueenside in FCastlingAbility) and ((FSquares[95] <> ptWKing) or
    (FSquares[91] <> ptWRook)) then
    FCastlingAbility := FCastlingAbility - [ctWQueenside];
  if (ctBKingside in FCastlingAbility) and ((FSquares[25] <> ptBKing) or
    (FSquares[28] <> ptBRook)) then
    FCastlingAbility := FCastlingAbility - [ctBKingside];
  if (ctBQueenside in FCastlingAbility) and ((FSquares[25] <> ptBKing) or
    (FSquares[21] <> ptBRook)) then
    FCastlingAbility := FCastlingAbility - [ctBQueenside];
  if not FWhitesTurn then
    Inc(FMoveNumber);
  FWhitesTurn := not FWhitesTurn;
end;

constructor TStandardPosition.Create;
var
  Coordinate: integer;
begin
  FLegalMoves := TMoveList.Create(True);
  for Coordinate := 0 to 119 do
    if Coordinate in OffSquares then
      FSquares[Coordinate] := ptOff
    else
      FSquares[Coordinate] := ptEmpty;
end;

destructor TStandardPosition.Destroy;
begin
  FreeAndNil(FLegalMoves);
end;

procedure TStandardPosition.FromFEN(AFEN: string);
var
  c: char;
  s, p: TStringList;
  rk, fl, i, Coordinate: integer;
  temp: string;
  RegFEN: TRegExpr;
begin
  RegFEN := TRegExpr.Create;
  RegFEN.Expression := '(([prnbqkPRNBQK1-8]){1,8}/){7}([prnbqkPRNBQK]){1,8} ' +
    '(w|b) (KQ?k?q?|Qk?q?|kq?|q|-) (-|([a-h][36])) (0|[1..9][0..9]*) [1..9][0..9]*';
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
      if FSquares[Coordinate] = ptOff then
        raise EInvalidFEN.Create('FEN is invalid');
      case temp[i] of
        '1'..'8': Inc(fl, StrToInt(temp[i]) - 1);
        'p': FSquares[Coordinate] := ptBPawn;
        'r': FSquares[Coordinate] := ptBRook;
        'n': FSquares[Coordinate] := ptBKnight;
        'b': FSquares[Coordinate] := ptBBishop;
        'q': FSquares[Coordinate] := ptBQueen;
        'k': FSquares[Coordinate] := ptBKing;
        'P': FSquares[Coordinate] := ptWPawn;
        'R': FSquares[Coordinate] := ptWRook;
        'N': FSquares[Coordinate] := ptWKnight;
        'B': FSquares[Coordinate] := ptWBishop;
        'Q': FSquares[Coordinate] := ptWQueen;
        'K': FSquares[Coordinate] := ptWKing;
      end;
      Inc(fl);
    end;
  end;
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
    FEnPassant := AlgebraicSquare(s.Strings[3][1], s.Strings[3][2]);
  // Get plies
  FPliesSinceLastPawnMoveOrCapture := StrToInt(s.Strings[4]);
  // Get start move number
  FMoveNumber := StrToInt(s.Strings[5]);
  FreeAndNil(s);
  Changed;
end;

function TStandardPosition.IsCheck: boolean;
var
  King: TPieceType;
  i: integer;
begin
  if FWhitesTurn then
    King := ptWKing
  else
    King := ptBKing;
  for i in ValidSquares do
  begin
    if Squares[i] = King then
    begin
      Result := IsAttacked(i);
    end;
  end;
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
  temp: boolean;
begin
  Result := True;
  for i := 1 to 6 do
  begin
    WhitePieces[i] := 0;
    BlackPieces[i] := 0;
  end;
  // Check correct count of pieces, i. e. not more than 32
  for i in ValidSquares do
    case FSquares[i] of
      ptWPawn: Inc(WhitePieces[1]);
      ptWKnight: Inc(WhitePieces[2]);
      ptWBishop: Inc(WhitePieces[3]);
      ptWRook: Inc(WhitePieces[4]);
      ptWQueen: Inc(WhitePieces[5]);
      ptWKing: Inc(WhitePieces[6]);
      ptBPawn: Inc(BlackPieces[1]);
      ptBKnight: Inc(BlackPieces[2]);
      ptBBishop: Inc(BlackPieces[3]);
      ptBRook: Inc(BlackPieces[4]);
      ptBQueen: Inc(BlackPieces[5]);
      ptBKing: Inc(BlackPieces[6]);
    end;
  Result := Result and (WhitePieces[6] = 1) and (BlackPieces[6] = 1) and
    (WhitePieces[1] <= 8) and (BlackPieces[1] <= 8) and
    (SumOf(WhitePieces) <= 16) and (SumOf(BlackPieces) <= 16);
  // Check if CastlingAbilities are set correct
  if ctWKingside in FCastlingAbility then
    Result := Result and (FSquares[95] = ptWKing) and (FSquares[98] = ptWRook);
  if ctWQueenside in FCastlingAbility then
    Result := Result and (FSquares[95] = ptWKing) and (FSquares[91] = ptWRook);
  if ctBKingside in FCastlingAbility then
    Result := Result and (FSquares[25] = ptBKing) and (FSquares[28] = ptBRook);
  if ctBQueenside in FCastlingAbility then
    Result := Result and (FSquares[25] = ptBKing) and (FSquares[21] = ptBRook);
  // Check if a king is in illegal check
  for i in ValidSquares do
  begin
    if FSquares[i] = ptWKing then
    begin
      temp := FWhitesTurn;
      FWhitesTurn := True;
      Result := Result and not (not temp and IsAttacked(i));
      FWhitesTurn := temp;
    end;
    if FSquares[i] = ptBKing then
    begin
      temp := FWhitesTurn;
      FWhitesTurn := False;
      Result := Result and not (temp and IsAttacked(i));
      FWhitesTurn := temp;
    end;
  end;
end;

function TStandardPosition.MoveFromSAN(ASAN: string): TMove;
var
  i: integer;
begin
  // This is one way, but SAN is not unique
  Result := nil;
  for i := 0 to FLegalMoves.Count - 1 do
  begin
    if MoveToSAN(FLegalMoves.Items[i]) = ASAN then
      Result := FLegalMoves.Items[i];
  end;
  if Result = nil then
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
  SameDest := TMoveList.Create(False);
  Clone := TStandardPosition.Create;
  Piece := FSquares[TSquare10x12(AMove.Start)];
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
          (FSquares[TSquare10x12(FLegalMoves.Items[j].Start)] = Piece) then
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
    if (FSquares[TSquare10x12(AMove.Dest)] <> ptEmpty) or
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
var
  i: byte;
begin
  FSquares[91] := ptWRook;
  FSquares[92] := ptWKnight;
  FSquares[93] := ptWBishop;
  FSquares[94] := ptWQueen;
  FSquares[95] := ptWKing;
  FSquares[96] := ptWBishop;
  FSquares[97] := ptWKnight;
  FSquares[98] := ptWRook;
  for i in Rank2 do
    FSquares[i] := ptWPawn;
  for i in Rank7 do
    FSquares[i] := ptBPawn;
  FSquares[21] := ptBRook;
  FSquares[22] := ptBKnight;
  FSquares[23] := ptBBishop;
  FSquares[24] := ptBQueen;
  FSquares[25] := ptBKing;
  FSquares[26] := ptBBishop;
  FSquares[27] := ptBKnight;
  FSquares[28] := ptBRook;
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
      if FSquares[10 * i + j] = ptEmpty then
        Inc(z)
      else
      begin
        if z > 0 then
          Result := Result + IntToStr(z);
        case FSquares[10 * i + j] of
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
  if FEnPassant in ValidSquares then
    Result := Result + SquareToString(FEnPassant)
  else
    Result := Result + '-';
  Result := Result + ' ';
  // Halfmove clock
  Result := Result + IntToStr(FPliesSinceLastPawnMoveOrCapture) + ' ';
  // Fullmove number
  Result := Result + IntToStr(FMoveNumber);
end;

end.
