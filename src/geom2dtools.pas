{ JCC (Jan's Chess Componenents) - This file contains function to calculate with rects and points on a canvas
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

unit Geom2DTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

operator +(const Point1, Point2: TPoint): TPoint;
operator +(const APoint: TPoint; const ARect: TRect): TRect;

implementation

operator +(const Point1, Point2: TPoint): TPoint;
begin
  Result := Point(Point1.X + Point2.X, Point1.Y + Point2.Y);
end;

operator+(const APoint: TPoint; const ARect: TRect): TRect;
begin
  Result := Rect(APoint.X + ARect.Left, APoint.Y + ARect.Top, APoint.X +
    ARect.Right, APoint.Y + ARect.Bottom);
end;

end.
