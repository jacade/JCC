{ JCC (Jan's Chess Componenents) - This file contains a base class for chess databases
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
unit Database;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, Game;

type

  { TDatabase }

  TDatabase = class(specialize TFPGObjectList<TGame>)
  public
    procedure LoadFromFile(const AFile: string);
    procedure LoadFromStream(const AStream: TStream); virtual; abstract;
    procedure SaveToFile(const AFile: string);
    procedure SaveToStream(const AStream: TStream); virtual; abstract;
  end;

implementation

{ TDatabase }

procedure TDatabase.LoadFromFile(const AFile: string);
var
  ms: TMemoryStream;
begin
  ms := TMemoryStream.Create;
  ms.LoadFromFile(AFile);
  Self.LoadFromStream(ms);
  ms.Free;
end;

procedure TDatabase.SaveToFile(const AFile: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AFile, fmCreate);
  Self.SaveToStream(fs);
  fs.Free;
end;

end.

