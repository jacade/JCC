{ JCC (Jan's Chess Componenents) - This file contains some functions to work with strings
  Copyright (C) 2016-2017  Jan Dette

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

unit StrTools;

{$mode objfpc}{$H+}

// In here some useful string operations for parsing

interface

uses
  Classes, SysUtils;

type
  TStringArray = array of string;

function CountOccurences(const Substr, Source: string): integer;
function CountOccurencesOfChars(const Chars, Source: string): integer;
function GetValuesOfKeys(const Source: string;
  const Keys: array of string): TStringArray;
function Split(const Source, Delimiter: string): TStringList;

implementation

function CountOccurences(const Substr, Source: string): integer;
var
  p: integer;
begin
  Result := 0;
  p := Pos(Substr, Source);
  while p > 0 do
  begin
    Inc(Result);
    p := Pos(Substr, Copy(Source, p, Length(Source) - p));
  end;
end;

// Counts the Occurences of each char given in Chars and sum then up
function CountOccurencesOfChars(const Chars, Source: string): integer;
var
  c: char;
begin
  Result := 0;
  for c in Chars do
    Inc(Result, CountOccurences(c, Source));
end;

function GetValuesOfKeys(const Source: string;
  const Keys: array of string): TStringArray;
var
  s: TStringList;
  i, j, Token: integer;
  TokenFound: boolean;
begin
  if Length(Trim(Source)) = 0 then
    Exit;
  SetLength(Result, Length(Keys));
  for i := 0 to Length(Result) - 1 do
    Result[i] := '';
  s := Split(Trim(Source), ' ');
  Token := -1;
  for i := 0 to s.Count - 1 do
  begin
    TokenFound := False;
    for j := 0 to Length(Result) - 1 do
    begin
      if Keys[j] = s.Strings[i] then
      begin
        Token := j;
        TokenFound := True;
      end;
    end;
    if not TokenFound then
      if Length(Result[Token]) = 0 then
        Result[Token] := s.Strings[i]
      else
        Result[Token] := Result[Token] + ' ' + s.Strings[i];
  end;
  FreeAndNil(s);
end;

function Split(const Source, Delimiter: string): TStringList;
var
  p: integer;
  s: string;
begin
  s := Source;
  Result := TStringList.Create;
  p := Pos(Delimiter, s);
  while p > 0 do
  begin
    Result.Add(Copy(s, 1, p - 1));
    Delete(s, 1, p + Length(Delimiter) - 1);
    p := Pos(Delimiter, s);
  end;
  Result.Add(s);
end;

end.
