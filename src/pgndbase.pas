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

// {$DEFINE LOGGING}
uses
  Classes, SysUtils, FileUtil, fgl, Game, PGNGame, Position, Ply, NotationToken;

const
  TabCharacters = [#9, #11];
  ContinuationCharacters = ['A'..'Z', 'a'..'z', '0'..'9', '-', '=', '+', '#', ':'];

type
  TPGNDatabase = specialize TFPGObjectList<TPGNGame>;

  { TPGNDatabaseHelper }

  TPGNDatabaseHelper = class helper for TPGNDatabase
  public
    procedure LoadFromFile(const APGNFile: string);
    procedure SaveToFile(const APGNFile: string);
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

{ TPGNDatabaseHelper }

function ComparePGNGames(const Item1, Item2: TPGNGame): Integer;
var
  s1, s2: string;
begin
  Result := CompareStr(StringReplace(Item1.Date, '?', '0', [rfReplaceAll]), StringReplace(Item2.Date, '?', '0', [rfReplaceAll]));
  if Result = 0 then
  begin
    Result := CompareStr(Item1.Event, Item2.Event);
    if Result = 0 then
    begin
      Result := CompareStr(Item1.Site, Item2.Site);
      if Result = 0 then
      begin
        s1 := StringReplace(Item1.Round, '-', '0', [rfReplaceAll]);
        s1 := StringReplace(s1, '?', '/', [rfReplaceAll]);
        s2 := StringReplace(Item2.Round, '-', '0', [rfReplaceAll]);
        s2 := StringReplace(s2, '?', '/', [rfReplaceAll]);
        Result := CompareStr(s1, s2);
        if Result = 0 then
        begin
          Result := CompareStr(Item1.White, Item2.White);
          if Result = 0 then
          begin
            Result := CompareStr(Item1.Black, Item2.Black);
            if Result = 0 then
            begin
              Result := CompareStr(Item1.Result, Item2.Result);
              if Result = 0 then
              begin
                Result := CompareStr(Item1.GetPGNNotation, Item2.GetPGNNotation);
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TPGNDatabaseHelper.LoadFromFile(const APGNFile: string);
var
  F: Text;
  s: string;
  AlreadyRead, Index: word;
  c: char;
  VariationLevel: word;

  function getChar(i: word): char;
  begin
    if i > AlreadyRead + Length(s) then
    begin
      Inc(AlreadyRead, Length(s));
      ReadLn(F, s);
      {$IFDEF Logging}
      WriteLn('Gelesen: ', s);
      {$ENDIF}
      s := s + #10;
    end;
    Result := s[i - AlreadyRead];
  end;

  function ReadStringToken: string;
  begin
    Result := '';
    Inc(Index);
    //if Index > Len then
    //  raise EPGNImportException.Create('Unexpected end of string');
    c := getChar(Index);
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
        c := getChar(Index);
        if not ((c = '\') or (c = '"')) then
          raise EPGNImportException.Create('Found "' + c +
            '", but expected "\" or """ after "\"');
      end;
      Result := Result + c;
      Inc(Index);
      //if Index > Len then
      //  raise EPGNImportException.Create('Unexpected end of string');
      c := getChar(Index);
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
        c := getChar(Index);
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
      c := getChar(Index);
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
      c := getChar(Index);
    until Found;
  end;

  function ExtractComment: string;
  begin
    Result := '';
    Inc(Index);
    //if Index > Len then
    //  raise EPGNImportException.Create('Unexpected end of string');
    // There are two different comment types
    if c = ';' then
    begin
      c := getChar(Index);
      while not (c = #10) do
      begin
        Result := Result + c;
        Inc(Index);
        c := getChar(Index);
      end;
    end
    else
    begin
      c := getChar(Index);
      while not (c = '}') do
      begin
        // TODO Check for an existing space
        if c = #10 then
          Result := Result + ' '
        else
          Result := Result + c;
        Inc(Index);
        //if Index > Len then
        //  raise EPGNImportException.Create('Unexpected end of string');
        c := getChar(Index);
      end;
    end;
  end;

  function ExtractNAG: string;
  begin
    Result := '';
    Inc(Index);
    c := getChar(Index);
    while (c in ['0'..'9']) do
    begin
      Result := Result + c;
      Inc(Index);
      c := getChar(Index);
    end;
  end;

  function ExtractGameResult: string;
  begin
    Result := '';
    while (c in ['0', '1', '2', '/', '-']) do
    begin
      Result := Result + c;
      Inc(Index);
      c := getChar(Index);
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
      c := getChar(Index);
    end;
    // Now we have to skip to the first non-period character and we have to skip spaces
    while ((c = '.') or (c = ' ')) do
    begin
      Inc(Index);
      c := getChar(Index);
    end;
  end;

  function ExtractMove: string;
  begin
    Result := '';
    while (c in ['A'..'Z', 'a'..'z', '0'..'9', '-', '=', '+', '#', '!', '?']) do
    begin
      Result := Result + c;
      Inc(Index);
      c := getChar(Index);
    end;
  end;

var
  EndOfGame, NewVariation: boolean;
  TempPGNGame: TPGNGame;
  Tag: TPGNTag;
  PGNMove, Comment: string;
  VariationPlies: TPlyTreeNodeStack;

  procedure ExtractLine;
  var
    EndOfLine: boolean;
    NAG: TNAG;
    GameResult: string;
  begin
    EndOfLine := False;
    while (not EndOfLine) and (not EndOfGame) {and (not EOF(F))} do
    begin
      c := getChar(Index);
      case c of
        '[':
        begin
          Tag := ExtractTag;
        {$IFDEF LOGGING}
          Writeln('Found Tag: ', Tag.Name, ' ', Tag.Value);
        {$ENDIF LOGGING}
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
          if Index - AlreadyRead = 1 then
          begin
            // Skip to the next line
            while not (c = #10) do
            begin
              Inc(Index);
              c := getChar(Index);
            end;
          end;
        end;
        '{', ';':
        begin
          Comment := ExtractComment;
          {$IFDEF Logging}
          WriteLn('Found Comment: ', Comment);
          {$ENDIF}
          if (not NewVariation) and (TempPGNGame.PlyTree.Count > 1) then
          begin
            TempPGNGame.SetCommentAfterCurrentPly(Comment);
            Comment := '';
          end;
        end;
        '$':
        begin
          NAG := StrToInt(ExtractNAG);
          {$IFDEF Logging}
          WriteLn('Found NAG: ', NAG);
          {$ENDIF}
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
          if (c = '*') or (getChar(Index + 1) in ['-', '/']) then
          begin
            GameResult := ExtractGameResult;
            {$IFDEF LOGGING}
            WriteLn('Found Game result: ', GameResult);
            {$ENDIF LOGGING}
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
              c := getChar(Index);
            end;
            getChar(Index + 1);
          end
          else
          begin
        {$IFDEF LOGGING}
            WriteLn('Found Move Number: ', ExtractMoveNumber);
        {$ELSE}
            ExtractMoveNumber;
        {$ENDIF LOGGING}
          end;
        end;
        'A'..'Z', 'a'..'z': // this could only be a move
        begin
          PGNMove := ExtractMove;
        {$IFDEF LOGGING}
          WriteLn('Found Move: ', PGNMove);
        {$ENDIF LOGGING}
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
          {$IFDEF Logging}
          WriteLn('Current Variation Level: ', VariationLevel);
          {$ENDIF}
          Inc(Index);
          NewVariation := True;
          ExtractLine;
        end;
        ')':
        begin
          if VariationLevel = 0 then
            raise EPGNImportException.Create('Found invalid ")"');
          Dec(VariationLevel);
          {$IFDEF Logging}
          WriteLn('Current Variation Level: ', VariationLevel);
          {$ENDIF}
          Inc(Index);
          EndOfLine := True;
          TempPGNGame.GoToPositionAfterPlyNode(VariationPlies.Top);
          VariationPlies.Pop;
        end;
        else
          Inc(Index);
      end;
      {$IFDEF LOGGING}
      WriteLn('Current Index: ', Index);
      {$ENDIF LOGGING}
    end;

  end;

var
  temp: TStandardPosition;
begin
  if not FileExists(APGNFile) then
    raise Exception.Create('File does not exist');
  AssignFile(F, APGNFile);
  Reset(F);
  s := '';
  Comment := '';
  VariationPlies := TPlyTreeNodeStack.Create;
  // TODO: in case we find a FEN tag, we need to handle this
  temp := TStandardPosition.Create(TStandardPosition.InitialFEN);
  while not EOF(F) do
  begin
    EndOfGame := False;
    NewVariation := False;
    TempPGNGame := TPGNGame.Create(temp);
    AlreadyRead := 0;
    Index := 1;
    VariationLevel := 0;
    ExtractLine;
    if EndOfGame then
    begin
      {$IFDEF LOGGING}
      WriteLn('Added game');
      {$ENDIF LOGGING}
      Add(TempPGNGame);
    end
    else
    begin
      {$IFDEF LOGGING}
      WriteLn('Dismissed game');
      {$ENDIF LOGGING}
      TempPGNGame.Free;
    end;
  end;
  temp.Free;
  VariationPlies.Free;
end;

procedure TPGNDatabaseHelper.SaveToFile(const APGNFile: string);
var
  PGNGame: TPGNGame;
  F: Text;
  Temp: TPGNTag;
  i: Integer;
begin
  AssignFile(F, APGNFile);
  Rewrite(F);
  try
    Self.Sort(@ComparePGNGames);
    for PGNGame in Self do
    begin
      // Write tag roster
      WriteLn(F, '[Event "' + PGNGame.Event + '"]');
      WriteLn(F, '[Site "' + PGNGame.Site + '"]');
      WriteLn(F, '[Date "' + PGNGame.Date + '"]');
      WriteLn(F, '[Round "' + PGNGame.Round + '"]');
      WriteLn(F, '[White "' + PGNGame.White + '"]');
      WriteLn(F, '[Black "' + PGNGame.Black + '"]');
      WriteLn(F, '[Result "' + PGNGame.Result + '"]');
      // Write any additional tags
      for i := 0 to PGNGame.CountAdditionalTags - 1 do
      begin
        Temp := PGNGame.AdditionalTags[i]^;
        WriteLn(F, '[' + Temp.Name + ' "' + Temp.Value + '"]');
      end;
      // Write move section
      WriteLn(F);
      WriteLn(F, PGNGame.GetPGNNotation);
      WriteLn(F);
    end;

  finally
    CloseFile(F);
  end;
end;

end.
