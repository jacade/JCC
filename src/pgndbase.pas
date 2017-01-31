{ JCC (Jan's Chess Componenents) - This file contains a class to import/export pgn files
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

unit PGNdbase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, fgl, strutils, Game, PGNGame, Position, Ply,
  NotationToken, Database;

const
  TabCharacters = [#9, #11];
  ContinuationCharacters = ['A'..'Z', 'a'..'z', '0'..'9', '-', '=', '+', '#', ':'];

type

  { TPGNDatabase }

  TPGNDatabase = class(TDatabase)
  public
    procedure LoadFromStream(const AStream: TStream); override;
    procedure SaveToStream(const AStream: TStream); override;
  end;

function StrToRating(const s: string): integer;

implementation

function StrToRating(const s: string): integer;
begin
  if s = '-' then
    Result := NO_RATING
  else
    Result := StrToInt(s);
end;

{ TPGNDatabase }

function ComparePGNGames(const Item1, Item2: TGame): integer;
var
  s1, s2: string;
  pgn1, pgn2: TPGNGame;
begin
  pgn1 := Item1 as TPGNGame;
  pgn2 := Item2 as TPGNGame;
  Result := CompareStr(StringReplace(pgn1.Date, '?', '0', [rfReplaceAll]),
    StringReplace(pgn2.Date, '?', '0', [rfReplaceAll]));
  if Result = 0 then
  begin
    Result := CompareStr(pgn1.Event, pgn2.Event);
    if Result = 0 then
    begin
      Result := CompareStr(pgn1.Site, pgn2.Site);
      if Result = 0 then
      begin
        s1 := StringReplace(pgn1.Round, '-', '0', [rfReplaceAll]);
        s1 := StringReplace(s1, '?', '/', [rfReplaceAll]);
        s2 := StringReplace(pgn2.Round, '-', '0', [rfReplaceAll]);
        s2 := StringReplace(s2, '?', '/', [rfReplaceAll]);
        Result := CompareStr(s1, s2);
        if Result = 0 then
        begin
          Result := CompareStr(pgn1.White, pgn2.White);
          if Result = 0 then
          begin
            Result := CompareStr(pgn1.Black, pgn2.Black);
            if Result = 0 then
            begin
              Result := CompareStr(pgn1.Result, pgn2.Result);
              if Result = 0 then
              begin
                Result := CompareStr(pgn1.GetPGNNotation, pgn2.GetPGNNotation);
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TPGNDatabase.LoadFromStream(const AStream: TStream);
var
  s: string;
  Index: longint;
  c: char;
  VariationLevel: word;

  function ReadStringToken: string;
  begin
    Result := '';
    Inc(Index);
    //if Index > Len then
    //  raise EPGNImportException.Create('Unexpected end of string');
    c := s[Index];
    while not (c = '"') do
    begin
      if c = #10 then
        raise EPGNImportException.Create(
          'Illegal newline character found in a string token');
      if c in TabCharacters then
        raise EPGNImportException.Create(
          'Illegal tab characters found in a string token');
      if c = '\' then
      begin
        Inc(Index);
        c := s[Index];
        if not ((c = '\') or (c = '"')) then
          raise EPGNImportException.Create('Found "' + c +
            '", but expected "\" or """ after "\"');
      end;
      Result := Result + c;
      Inc(Index);
      //if Index > Len then
      //  raise EPGNImportException.Create('Unexpected end of string');
      c := s[Index];
    end;
  end;

  function ExtractTag: TPGNTag;

    function ExtractTagName: string;
    begin
      Result := '';
      while c in ['A'..'Z', 'a'..'z', '0'..'9', '_'] do
      begin
        Result := Result + c;
        Inc(Index);
        //if Index > Len then
        //  raise EPGNImportException.Create('Unexpected end of string');
        c := s[Index];
      end;
    end;

  var
    Found: boolean;
  begin
    Found := False;
    // Search for the tag name
    repeat
      Inc(Index);
      //if Index > Len then
      //  raise EPGNImportException.Create('Unexpected end of string');
      c := s[Index];
      if c in ['A'..'Z', 'a'..'z', '0'..'9', '_'] then
      begin
        Result.Name := ExtractTagName;
        Found := True;
      end;
      if c = ']' then
        raise EPGNImportException.Create('Found "]", but expected tag name');
    until Found;
    Found := False;
    // Search for the tag value
    repeat
      if c = '"' then
      begin
        Result.Value := ReadStringToken;
        Found := True;
      end;
      if c = ']' then
        raise EPGNImportException.Create('Found "]", but expected tag value');
      Inc(Index);
      //if Index > Len then
      //  raise EPGNImportException.Create('Unexpected end of string');
      c := s[Index];
    until Found;
  end;

  function ExtractComment: string;
  begin
    Inc(Index);
    //if Index > Len then
    //  raise EPGNImportException.Create('Unexpected end of string');
    // There are two different comment types
    if c = ';' then
    begin
      Result := ExtractSubstr(s, Index, [#10]);
    end
    else
    begin
      Result := ExtractSubstr(s, Index, ['}']);
      Result := ReplaceStr(Result, #10, ' ');
      Result := ReplaceStr(Result, #13, '');
    end;
  end;

  function ExtractNAG: string;
  begin
    Result := '';
    Inc(Index);
    c := s[Index];
    while (c in ['0'..'9']) do
    begin
      Result := Result + c;
      Inc(Index);
      c := s[Index];
    end;
  end;

  function ExtractGameResult: string;
  begin
    Result := '';
    while (c in ['0', '1', '2', '/', '-']) do
    begin
      Result := Result + c;
      Inc(Index);
      c := s[Index];
    end;
  end;

  function ExtractMoveNumber: string;
  begin
    Result := '';
    // Extract the entire number
    while (c in ['0'..'9']) do
    begin
      Result := Result + c;
      Inc(Index);
      c := s[Index];
    end;
    // Now we have to skip to the first non-period character and we have to skip spaces
    while ((c = '.') or (c = ' ')) do
    begin
      Inc(Index);
      c := s[Index];
    end;
  end;

  function ExtractMove: string;
  begin
    Result := '';
    while (c in ['A'..'Z', 'a'..'z', '0'..'9', '-', '=', '+', '#', '!', '?']) do
    begin
      Result := Result + c;
      Inc(Index);
      c := s[Index];
    end;
  end;

var
  EndOfGame, NewVariation: boolean;
  TempPGNGame: TPGNGame;
  Tag: TPGNTag;
  PGNMove, Comment: string;
  VariationPlies: TPlyTreeNodeStack;
  Gesamt, Parsen: extended;
  Len: QWord;

  procedure ExtractLine;
  var
    EndOfLine: boolean;
    NAG: TNAG;
    GameResult: string;
  begin
    EndOfLine := False;
    while (not EndOfLine) and (not EndOfGame) and (Index < Len) do
    begin
      c := s[Index];
      case c of
        '[':
        begin
          Tag := ExtractTag;
          case Lowercase(Tag.Name) of
            // Seven Tag Roster
            'event': TempPGNGame.Event := Tag.Value;
            'site': TempPGNGame.Site := Tag.Value;
            'date': TempPGNGame.Date := Tag.Value;
            'round': TempPGNGame.Round := Tag.Value;
            'white': TempPGNGame.White := Tag.Value;
            'black': TempPGNGame.Black := Tag.Value;
            'result': TempPGNGame.Result := Tag.Value;
            else
              TempPGNGame.AddAdditionalTag(Tag);
          end;
        end;
        '%':
        begin
          // Check if we are at the beginning of a line
          if s[Index - 1] = #10 then
          begin
            // Skip to the next line
            while not (c = #10) do
            begin
              Inc(Index);
              c := s[Index];
            end;
          end;
        end;
        '{', ';':
        begin
          Comment := ExtractComment;
          if (not NewVariation) and (TempPGNGame.PlyTree.Count > 1) then
          begin
            TempPGNGame.SetCommentAfterCurrentPly(Comment);
            Comment := '';
          end;
        end;
        '$':
        begin
          NAG := StrToInt(ExtractNAG);
          if NAG in MoveAssessments then
            TempPGNGame.CurrentPlyNode.Data.MoveAssessment := NAG
          else
          if NAG in PositionalAssessments then
            TempPGNGame.CurrentPlyNode.Data.PositionalAssessment := NAG
          else
          if NAG in TimePressureComments then
            TempPGNGame.CurrentPlyNode.Data.TimePressureCommentary := NAG
          else
          if NAG in NonStandardGlyphs then
            TempPGNGame.CurrentPlyNode.Data.NonStandardGlyph := NAG;
        end;
        '0'..'9', '*':
        begin
          //if Index = Len then
          //  raise EPGNImportException.Create('Unexpected end of string');
          // Peek to decide whether this is a move number or a game result
          if (c = '*') or (s[Index + 1] in ['-', '/']) then
          begin
            GameResult := ExtractGameResult;
            case GameResult of
              '1-0': TempPGNGame.GameResult := grWhiteWins;
              '0-1': TempPGNGame.GameResult := grBlackWins;
              '1/2-1/2': TempPGNGame.GameResult := grDraw;
              else
                TempPGNGame.GameResult := grNone;
            end;
            EndOfGame := True;
            // We do not expect anything more to come, so we skip into the next line
            while (c <> #10) do
            begin
              Inc(Index);
              c := s[Index];
            end;
            Inc(Index);
          end
          else
          begin
            ExtractMoveNumber;
          end;
        end;
        'A'..'Z', 'a'..'z': // this could only be a move
        begin
          PGNMove := ExtractMove;
          NAG := 0;
          if Pos('!!', PGNMove) > 0 then
          begin
            NAG := 3;
            System.Delete(PGNMove, Pos('!!', PGNMove), 2);
          end
          else
          if Pos('!?', PGNMove) > 0 then
          begin
            NAG := 5;
            System.Delete(PGNMove, Pos('!?', PGNMove), 2);
          end
          else
          if Pos('?!', PGNMove) > 0 then
          begin
            NAG := 6;
            System.Delete(PGNMove, Pos('?!', PGNMove), 2);
          end
          else
          if Pos('??', PGNMove) > 0 then
          begin
            NAG := 4;
            System.Delete(PGNMove, Pos('??', PGNMove), 2);
          end
          else
          if Pos('!', PGNMove) > 0 then
          begin
            NAG := 1;
            System.Delete(PGNMove, Pos('!', PGNMove), 1);
          end
          else
          if Pos('?', PGNMove) > 0 then
          begin
            NAG := 2;
            System.Delete(PGNMove, Pos('?', PGNMove), 1);
          end;
          // *********************
          if NewVariation then
          begin
            VariationPlies.Push(TempPGNGame.CurrentPlyNode);
            TempPGNGame.GoOneMoveBackward;
            TempPGNGame.AddPGNMoveAsSideLine(PGNMove);
            NewVariation := False;
          end
          else
            TempPGNGame.AddPGNMove(PGNMove);
          if NAG > 0 then
            TempPGNGame.CurrentPlyNode.Data.MoveAssessment := NAG;
          if Length(Comment) > 0 then
            TempPGNGame.SetCommentBeforeCurrentPly(Comment);
          Comment := '';
        end;
        '(':
        begin
          Inc(VariationLevel);
          Inc(Index);
          NewVariation := True;
          ExtractLine;
        end;
        ')':
        begin
          if VariationLevel = 0 then
            raise EPGNImportException.Create('Found invalid ")"');
          Dec(VariationLevel);
          Inc(Index);
          EndOfLine := True;
          TempPGNGame.GoToPositionAfterPlyNode(VariationPlies.Top);
          VariationPlies.Pop;
        end;
        else
          Inc(Index);
      end;
    end;

  end;

var
  temp: TStandardPosition;
  g: integer = 0;
  i: integer;
begin
  // if not FileExists(APGNFile) then
  //  raise Exception.Create('File does not exist');
  Index := 1;
  s := '';
  Comment := '';
  VariationPlies := TPlyTreeNodeStack.Create;
  // TODO: in case we find a FEN tag, we need to handle this
  temp := TStandardPosition.Create(TStandardPosition.InitialFEN);
  Len := AStream.Size;
  SetLength(s, Len);
  AStream.ReadBuffer(s[1], Len);
  while Index < Len do
  begin
    EndOfGame := False;
    NewVariation := False;
    TempPGNGame := TPGNGame.Create(temp);
    VariationLevel := 0;
    ExtractLine;
    if EndOfGame then
    begin
      Add(TempPGNGame);
    end
    else
    begin
      TempPGNGame.Free;
    end;
  end;
  temp.Free;
  VariationPlies.Free;
end;

procedure TPGNDatabase.SaveToStream(const AStream: TStream);
var
  PGNGame: TPGNGame;
  i: integer;
  s: string;
begin
  Self.Sort(@ComparePGNGames);
  for i := 0 to Count - 1 do
  begin
    PGNGame := TPGNGame(Items[i]);
    s := PGNGame.GetPGNNotation + #10#10;
    AStream.WriteBuffer(s[1], Length(s));
  end;
end;

end.
