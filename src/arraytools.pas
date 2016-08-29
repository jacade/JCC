{ JCC (Jan's Chess Componenents) - This file contains some useful array functions
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

unit ArrayTools;

{$mode objfpc}{$H+}

interface

function SumOf(AArray: array of integer): integer;

implementation

function SumOf(AArray: array of integer): integer;
var
  i: integer;
begin
  Result := 0;
  for i := Low(AArray) to High(AArray) do
    Inc(Result, AArray[i]);
end;

end.

