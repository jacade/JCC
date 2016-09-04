unit PGNGame;

{$mode objfpc}{$H+}

{$DEFINE LOGGING}

interface

uses
  Classes, SysUtils, FileUtil, RegExpr, Game;
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

end.
