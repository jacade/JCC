{ JCC (Jan's Chess Componenents) - This file contains token classes to interchange notation between TGame and other classes like TNotationMemo
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
unit NotationToken;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, MoveList, Ply;

type
  {$PACKENUM 1}
  TGameResult = (grNone, grWhiteWins, grBlackWins, grDraw);

  TTokenKind = (tkMove, tkMoveNumber, tkBeginLine, tkEndLine, tkComment,
    tkNAG, tkResult);

  { TNotationToken }

  TNotationToken = class
  private
    FComment: string;
    FMove: TMove;
    FMoveNumber: string;
    FText: string;
    FNAG: TNAG;
    FResult: TGameResult;
    FTokenKind: TTokenKind;
  public
    function GetKind: TTokenKind; virtual; abstract;
  public
    property Comment: string read FComment write FComment;
    property Move: TMove read FMove write FMove;
    property MoveNumber: string read FMoveNumber write FMoveNumber;
    property Text: string read FText write FText;
    property NAG: TNAG read FNAG write FNAG;
    property Result: TGameResult read FResult write FResult;
  end;

  TGameNotation = specialize TFPGObjectList<TNotationToken>;

  { TBeginLineToken }

  TBeginLineToken = class(TNotationToken)
  public
    function GetKind: TTokenKind; override;
  end;

  { TCommentToken }

  TCommentToken = class(TNotationToken)
  public
    constructor Create(const AComment: string);
    function GetKind: TTokenKind; override;
  end;

  { TEndLineToken }

  TEndLineToken = class(TNotationToken)
  public
    function GetKind: TTokenKind; override;
  end;

  { TMoveToken }

  TMoveToken = class(TNotationToken)
  public
    constructor Create(const AMove: TMove; const AMoveText: string);
    function GetKind: TTokenKind; override;
  end;

  { TMoveNumberToken }

  TMoveNumberToken = class(TNotationToken)
  public
    constructor Create(const AMoveNumber: string);
    function GetKind: TTokenKind; override;
  end;

  { TNAGToken }

  TNAGToken = class(TNotationToken)
  public
    constructor Create(const ANAG: TNAG; const ANAGText: string);
    function GetKind: TTokenKind; override;
  end;

  { TResultToken }

  TResultToken = class(TNotationToken)
  public
    constructor Create(const AResult: TGameResult);
    function GetKind: TTokenKind; override;
  end;

function GameResultToStr(const AGameResult: TGameResult): string;

implementation

function GameResultToStr(const AGameResult: TGameResult): string;
begin
  case AGameResult of
    grNone: Result := '*';
    grWhiteWins: Result := '1-0';
    grBlackWins: Result := '0-1';
    grDraw: Result := '½-½';
  end;
end;

{ TBeginLineToken }

function TBeginLineToken.GetKind: TTokenKind;
begin
  Result := tkBeginLine;
end;

{ TCommentToken }

constructor TCommentToken.Create(const AComment: string);
begin
  FComment := AComment;
  FText := AComment;
end;

function TCommentToken.GetKind: TTokenKind;
begin
  Result := tkComment;
end;

{ TEndLineToken }

function TEndLineToken.GetKind: TTokenKind;
begin
  Result := tkEndLine;
end;

{ TMoveToken }

constructor TMoveToken.Create(const AMove: TMove; const AMoveText: string);
begin
  FMove := AMove;
  FText := AMoveText;
end;

function TMoveToken.GetKind: TTokenKind;
begin
  Result := tkMove;
end;

{ TMoveNumberToken }

constructor TMoveNumberToken.Create(const AMoveNumber: string);
begin
  FMoveNumber := AMoveNumber;
  FText := AMoveNumber;
end;

function TMoveNumberToken.GetKind: TTokenKind;
begin
  Result := tkMoveNumber;
end;

{ TNAGToken }

constructor TNAGToken.Create(const ANAG: TNAG; const ANAGText: string);
begin
  FNAG := ANAG;
  FText := ANAGText;
end;

function TNAGToken.GetKind: TTokenKind;
begin
  Result := tkNAG;
end;

{ TResultToken }

constructor TResultToken.Create(const AResult: TGameResult);
begin
  FResult := AResult;
  FText := GameResultToStr(AResult);
end;

function TResultToken.GetKind: TTokenKind;
begin
  Result := tkResult;
end;

end.

