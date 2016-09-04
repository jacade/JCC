unit PGNGame;

{$mode objfpc}{$H+}

{$DEFINE LOGGING}

interface

uses
  Classes, SysUtils, FileUtil, RegExpr, Game, MoveList, Position;
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
  temp: TStandardPosition;
  Move: TMove;
begin
  // NOTE: This is very strict and only works with pgn export format
  // However this should be sufficient in most of the cases
  Result := nil;
  temp := FCurrentPosition as TStandardPosition;
  for Move in temp.LegalMoves  do
  begin
    if temp.MoveToSAN(Move, False, False, csx, psEqualSign) = APGNMove then
      Result := Move;
  end;
  if Result = nil then
    raise Exception.Create(APGNMove + ' is no valid move.');
end;

procedure TPGNGame.AddPGNMove(const APGNMove: string);
begin
  AddMove(PGNToMove(APGNMove));
end;

procedure TPGNGame.AddPGNMoveAsSideLine(const APGNMove: string);
begin
  AddMoveAsSideLine(PGNToMove(APGNMove));
end;

end.
