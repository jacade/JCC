unit PGNGame;

{$mode objfpc}{$H+}

//{$DEFINE LOGGING}

interface

uses
  Classes, SysUtils, FileUtil, RegExpr, Game, MoveList, Position, Pieces
  {$IFDEF Logging}
  , Ply
  {$ENDIF}  ;
// as in http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm

const
  TabCharacters = [#9, #11];
  ContinuationCharacters = ['A'..'Z', 'a'..'z', '0'..'9', '-', '=', '+', '#', ':'];

type
  EPGNImportException = Exception;

  TPGNTag = record
    Name: string;
    Value: string;
  end;

  { TPGNGame }

  TPGNGame = class(TStandardGame)
  private
    // Seven Tag Roster
    FEvent, FSite, FDate, FRound, FWhite, FBlack, FResult: string;
    function PGNToMove(const APGNMove: string): TMove;
  public
    // These procedures add a move in pgn format e.g. Nf3, e8=Q
    procedure AddPGNMove(const APGNMove: string);
    procedure AddPGNMoveAsSideLine(const APGNMove: string);
  public
    property Event: string read FEvent write FEvent;
    property Site: string read FSite write FSite;
    property Date: string read FDate write FDate;
    property Round: string read FRound write FRound;
    property White: string read FWhite write FWhite;
    property Black: string read FBlack write FBlack;
    property Result: string read FResult write FResult;
  end;

implementation


{ TPGNGame }

function TPGNGame.PGNToMove(const APGNMove: string): TMove;
var
  Move: TMove;
  Piece, PromoPiece: TPieceType;
  AFile, ARank: byte;
  Start: TSquare10x12;
  s: string;
  Dest: TAlgebraicSquare;
  Temp: TMoveList;
  NotValid: boolean;
begin
  // NOTE: This is very strict and only works with pgn export format
  // However this should be sufficient in most of the cases
  s := APGNMove;
  // We don't need signs for check, mate or a capture, so we delete them
  if (s[Length(s)] = '+') or (s[Length(s)] = '+') then
    Delete(s, Length(s), 1);
  if Pos('x', s) > 0 then
    Delete(s, Pos('x', s), 1);
  // TODO: For now we ignore question and exclamation marks
  while Pos('!', s) > 0 do
    Delete(s, Pos('!', s), 1);
  while Pos('?', s) > 0 do
    Delete(s, Pos('?', s), 1);
  // Now s should be O-O, O-O-O or like [N, B, R, Q, K]?[a-h]?[1-8]?[a-h][1-8](=[N, B, R, Q, K])?
  if s = 'O-O' then
  begin
    if FCurrentPosition.WhitesTurn then
      Result := CreateMove(95, 97)
    else
      Result := CreateMove(25, 27);
  end
  else
  if s = 'O-O-O' then
  begin
    if FCurrentPosition.WhitesTurn then
      Result := CreateMove(95, 93)
    else
      Result := CreateMove(25, 23);
  end
  else
  begin
    // Get the piece and delete it afterwards from s
    case s[1] of
      'N': Piece := PieceType(bptKnight, FCurrentPosition.WhitesTurn);
      'B': Piece := PieceType(bptBishop, FCurrentPosition.WhitesTurn);
      'R': Piece := PieceType(bptRook, FCurrentPosition.WhitesTurn);
      'Q': Piece := PieceType(bptQueen, FCurrentPosition.WhitesTurn);
      'K': Piece := PieceType(bptKing, FCurrentPosition.WhitesTurn);
      'a'..'h', 'P': Piece := PieceType(bptPawn, FCurrentPosition.WhitesTurn);
    end;
    if s[1] in ['B', 'K', 'N', 'P', 'Q', 'R'] then
      Delete(s, 1, 1);
    // Look whether the move is a promotion
    if s[Length(s) - 1] = '=' then
    begin
      case s[Length(s)] of
        'N': PromoPiece := PieceType(bptKnight, FCurrentPosition.WhitesTurn);
        'B': PromoPiece := PieceType(bptBishop, FCurrentPosition.WhitesTurn);
        'R': PromoPiece := PieceType(bptRook, FCurrentPosition.WhitesTurn);
        'Q': PromoPiece := PieceType(bptQueen, FCurrentPosition.WhitesTurn);
      end;
      // Now we know the promotion piece and so we delete the last two chars
      Delete(s, Length(s) - 1, 2);
    end
    else
      PromoPiece := ptEmpty;
    // Get the Destination square and delete it from s afterwards
    Dest := AlgebraicSquare(s[Length(s) - 1], s[Length(s)]);
    Delete(s, Length(s) - 1, 2);
    // There is still some information left
    AFile := 0;
    Arank := 0;
    Start := 0;
    case Length(s) of
      0: ;// Noting
      1:
      begin  // There is either a file or a rank left
        if s[1] in ['a'..'h'] then
        begin
          AFile := Ord(s[1]) - 96;
        end
        else
        if s[1] in ['1'..'8'] then
        begin
          ARank := Ord(s[1]) - 48;
        end;
      end;
      2: Start := AlgebraicSquare(s[1], s[2]);
      else
        raise Exception.Create(APGNMove + ' is longer than expected');
    end;
    // Get all moves which match the extracted information
    Temp := (FCurrentPosition as TStandardPosition).FilterLegalMoves(Piece,
      Start, Dest, PromoPiece);
    case Temp.Count of
      0: raise Exception.Create(APGNMove + ' is no valid move.');
      1: Result := temp.Items[0];
      else // we need to extract more information from s
        for Move in Temp do
        begin
          if ((AFile > 0) and (Move.Start.RFile = AFile)) or
            ((ARank > 0) and (Move.Start.RRank = ARank)) then
            Result := Move;
        end;
    end;
    Temp.Free;
  end;
  //if Result = nil then
  //  raise Exception.Create(APGNMove + ' is no valid move.');
end;

{$IFDEF Logging}
procedure PrintTree(const AData: TPly);
begin
  if AData <> nil then
    Write(SquareToString(AData.Move.Start), SquareToString(AData.Move.Dest), '|');
end;

{$ENDIF}

procedure TPGNGame.AddPGNMove(const APGNMove: string);
var
  temp: TMove;
begin
  temp := PGNToMove(APGNMove);
  {$IFDEF Logging}
  WriteLn(SquareToString(temp.Start), SquareToString(temp.Dest));
  {$ENDIF}
  AddMove(temp);
  {$IFDEF Logging}
  FPlyTree.DepthFirstTraverse(@PrintTree);
  WriteLn;
  {$ENDIF}
end;

procedure TPGNGame.AddPGNMoveAsSideLine(const APGNMove: string);
begin
  AddMoveAsSideLine(PGNToMove(APGNMove));
{$IFDEF Logging}
  FPlyTree.DepthFirstTraverse(@PrintTree);
  WriteLn;
{$ENDIF}
end;

end.
